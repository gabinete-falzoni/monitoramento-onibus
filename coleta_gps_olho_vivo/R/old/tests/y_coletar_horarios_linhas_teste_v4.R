library(chromote)
library(rvest)
library(purrr)
library(dplyr)
library(tibble)
library(xml2)
library(arrow)


pasta_base <- '/mnt/fern/Dados/projetos/2026_Monitoramento_Olho_Vivo'
pasta_gtfs <- sprintf('%s/cittamobi_gtfs', pasta_base)


# ------------------------------------------------------------
# Dados GTFS das linhas
# ------------------------------------------------------------

trip <- sprintf('%s/trips.txt', pasta_gtfs)
trip <- read_delim(trip, delim = ',', col_types = 'ccccic')
trip <- trip %>% select(route_id, direction_id, service_id, trip_headsign)

linhas <- trip %>% select(route_id) %>% distinct()

linhas %>% mutate(n = row_number()) %>% filter(route_id == '701A-10') # todos os dias da semana
# linhas %>% mutate(n = row_number()) %>% filter(route_id == '1014-10') # só opera em dias úteis
linhas %>% slice(869) # linhas[869, ]

linha <- linhas[869, ]
# linha <- linhas[3, ]

# ------------------------------------------------------------
# 1. Start browser session
# ------------------------------------------------------------
b <- ChromoteSession$new()

url <- sprintf("https://www.sptrans.com.br/itinerarios/linha/?numero=%s", linha)

b$Page$navigate(url)

# Wait for initial DOM to exist (IMPORTANT: do NOT use loadEventFired)
Sys.sleep(5)


# ------------------------------------------------------------
# 2. Function: extract updated DOM
# ------------------------------------------------------------
get_page <- function() {
  html <- b$Runtime$evaluate(
    "document.documentElement.outerHTML"
  )$result$value

  read_html(html)
}

# ------------------------------------------------------------
# 3. Get available sentidos from dropdown
# ------------------------------------------------------------
get_sentidos <- function(page) {
  options <- page %>%
    html_elements("select#sel_sentido option")

  tibble(
    sentido_id = as.integer(html_attr(options, "value")),
    letreiro = html_text(options, trim = TRUE)
  )
}

# ------------------------------------------------------------
# 4. Function: change sentido (simulate click)
# ------------------------------------------------------------
set_sentido <- function(id) {

  b$Runtime$evaluate(sprintf("
    (function() {
      var el = document.getElementById('sel_sentido');
      el.value = '%s';
      el.dispatchEvent(new Event('change', { bubbles: true }));
    })();
  ", id))

  # wait for SPTrans JS to update DOM via internal XHR
  Sys.sleep(3)
}


set_dia_semana <- function(dia) {

  b$Runtime$evaluate(sprintf("
    (function() {
      var el = document.getElementById('sel_tipodia');
      el.value = '%s';
      el.dispatchEvent(new Event('change', { bubbles: true }));
    })();
  ", dia))

  # wait for SPTrans JS to update DOM via internal XHR
  Sys.sleep(3)
}

# ------------------------------------------------------------
# 5. Get available working weekdays from table
# ------------------------------------------------------------
get_funcionamento <- function(page) {
  blocos_func <- page %>%
    html_element(".tabelaFuncionamento") %>%
    html_elements(".tabelaTempo > div")

  funcionamento <- map_dfr(blocos_func, function(bloco) {
    legenda <- bloco %>%
      html_element(".painel-legenda") %>%
      html_text(trim = TRUE)

    ativo <- bloco %>%
      html_element(".painel-valor .glyphicon-ok") %>%
      length() > 0

    tibble(
      dia = legenda,
      operando = ativo
    )
  })
}


get_dias_semana <- function(page) {
  options <- page %>%
    html_element("#sel_tipodia") %>%
    html_elements("option")

  tipo_dia <- tibble(
    value = html_attr(options, "value"),
    dia = html_text(options, trim = TRUE),
    selected = !is.na(html_attr(options, "selected")),
    disabled = !is.na(html_attr(options, "disabled"))
  ) %>% filter(disabled == FALSE)
}


# ------------------------------------------------------------
# 6. Get available empresas from page
# ------------------------------------------------------------
get_empresas <- function(page) {
  # Consórcio / empresa operadora
  empresas <- page %>%
    html_elements(".headerAreaMaior + div .row")

  empresa_info <- map_dfr(empresas, function(linha) {
    chave <- linha %>%
      html_element(".painel-legenda") %>%
      html_text(trim = TRUE)

    valor <- linha %>%
      html_element(".painel-valor") %>%
      html_text(trim = TRUE)

    tibble(
      campo = chave,
      valor = valor
    )
  })
}


get_itinerario <- function(page) {
  # Parse itinerario as table, getting event warnings in data-featherlight
  rows <- page %>%
    html_elements("div.tabelaItinerario table tr")

  itinerario <- map_dfr(rows, function(row) {

    cells <- row %>% html_elements("td")
    texts <- cells %>% html_text(trim = TRUE)

    # extract featherlight content (if exists)
    link <- row %>% html_element("a")
    evento <- NA_character_

    if (!is.na(link)) {
      raw_html <- link %>% html_attr("data-featherlight")

      if (!is.na(raw_html)) {
        parsed <- read_html(raw_html)
        evento <- parsed %>% html_text(trim = TRUE)
      }
    }

    tibble(
      evento_na_via = evento,
      via = texts[2],
      numeral = texts[3]
    )
  })
  itinerario <- itinerario %>% select(2, 3, 1) %>% filter(!is.na(via))

  return(itinerario)
}


get_partidas <- function(page) {
  # Partidas
  partidas <- page %>%
    html_element("div.tabelaPartidas") %>%
    html_elements(".partidas span")

  partidas <- tibble(
    hora = html_text(partidas, trim = TRUE),
    invert = html_attr(partidas, "class") == "invert"
  )
}

get_tempos_viagem <- function(page) {
  # Blocos tempo de viagem
  blocos_tempo <- page %>%
    html_element(".tempoEstimado") %>%
    html_elements(".tabelaTempo > div")

  tempo_estimado <- map_dfr(blocos_tempo, function(bloco) {
    legenda <- bloco %>%
      html_element(".painel-legenda") %>%
      html_text(trim = TRUE)

    valor <- bloco %>%
      html_element(".painel-valor") %>%
      html_text(trim = TRUE)

    tibble(
      periodo = legenda,
      minutos = as.numeric(str_extract(valor, "\\d+"))
    )
  })
}


# dias_semana <- get_funcionamento(page)
# dias_semana <- dias_semana %>%
#   mutate(linha = linha$route_id) %>%
#   pivot_wider(id_cols = linha,
#               names_from = dia,
#               values_from = operando) %>%
#   select(linha, U = 2, S = 3, D = 4)
#
# empresas <- get_empresas(page)
# empresas <- empresas %>%
#   mutate(linha = linha$route_id) %>%
#   pivot_wider(id_cols = linha,
#               names_from = campo,
#               values_from = valor)
#
# basic <- left_join(dias_semana, empresas, by = 'linha')




# # Letreiro == Sentido
# letreiro <- page %>%
#   html_element("span.letreiroTitulo") %>%
#   html_text(trim = TRUE)

page <- get_page()

# Quantos sentidos há na linha? 0 ou 1?
sentidos <- get_sentidos(page)

# Cobertura em dias da semana? 0, 1 ou 2?
dias_semana <- get_dias_semana(page)

data_base <- format(Sys.Date(), "%Y%m%d")
pasta_grav <- file.path(pasta_base, data_base)
dir.create(pasta_grav, recursive = TRUE, showWarnings = FALSE)

# dia <- 0; sentido <- 0
for (sentido in sentidos$sentido_id) {
  # sentido <- 0
  for (dia in dias_semana$value) {
    # dia <- 0
    print(sprintf('Sentido: %s - Dia: %s', sentido, dia))

    letra_semana <- dias_semana$dia[dias_semana$value == as.character(dia)]

    if (letra_semana == 'Segunda a Sexta') {
      letra_semana <- 'U'
    } else if (letra_semana == 'Sábado') {
      letra_semana <- 'S'
    } else if (letra_semana == 'Domingo') {
      letra_semana <- 'D'
    }

    out_partidas <- sprintf('%s/%s_%s_%s_%s_partidas.csv', pasta_grav, data_base, linha, sentido, letra_semana)
    if (!file.exists(out_partidas)) {
      # Atualizar página conforme dia e sentido
      set_dia_semana(dia)
      set_sentido(sentido)
      page <- get_page()

      partidas <- get_partidas(page)
      write_delim(partidas, out_partidas, delim = ';')

      out_tempos <- str_replace(out_partidas, '_partidas\\.csv', '_tempos.csv')
      tempos_viagem <- get_tempos_viagem(page)
      write_delim(tempos_viagem, out_tempos, delim = ';')

      out_rotas <- str_replace(out_partidas, '_partidas\\.csv', '_rotas.csv')
      itinerario <- get_itinerario(page)
      write_delim(itinerario, out_rotas, delim = ';')

      # get_horarios(page, out_partidas)
      #
      # out_tempos <- str_replace(out_partidas, '_partidas\\.csv', '_tempos.csv')
      # get_tempos(page, out_tempos)
      #
      # out_rotas <- str_replace(out_partidas, '_partidas\\.csv', '_rotas.csv')
      # get_rotas(page, out_rotas)
    }
  }
}

























# ------------------------------------------------------------
# 7. Run for all sentidos
# ------------------------------------------------------------
df <- map_dfr(sentidos$sentido_id, get_sentido_data)

# ------------------------------------------------------------
# 8. Attach metadata
# ------------------------------------------------------------
numero_linha <- df$letreiro[1]  # optional fallback if needed

df <- df %>%
  mutate(
    numero_linha = "701A-10"
  )

# ------------------------------------------------------------
# 9. Final output
# ------------------------------------------------------------
df