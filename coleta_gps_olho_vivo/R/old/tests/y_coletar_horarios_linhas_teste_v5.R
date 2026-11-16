library(chromote)
library(rvest)
library(purrr)
library(dplyr)
library(tibble)
library(xml2)
library(arrow)
# detach("package:tidylog")

pasta_base <- '/mnt/fern/Dados/projetos/2026_Monitoramento_Olho_Vivo'
pasta_gtfs <- sprintf('%s/cittamobi_gtfs', pasta_base)


# ------------------------------------------------------------------------------
# Funções gerais para ler elementos da página
# ------------------------------------------------------------------------------

# Extrair versão atualizada da página (DOM)
get_page <- function() {
  html <- b$Runtime$evaluate(
    "document.documentElement.outerHTML"
  )$result$value

  read_html(html)
}


# Ler os dias da semana habilitados para escolha
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


# Ler sentidos existentes da linha habilitados para escolha
get_sentidos <- function(page) {
  options <- page %>%
    html_elements("select#sel_sentido option")

  tibble(
    sentido_id = html_attr(options, "value"),
    letreiro = html_text(options, trim = TRUE)
  )
}


# Ler dias da semana em que a linha opera (funcionamento da linha)
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


# Ler tempos estimados de viagem (varia conforme dia da semana e sentido)
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


# Ler itinerário da linha (varia conforme o sentido)
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


# Ler os horários de partida programados (varia conforme sentido e dia da semana)
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

# Ler consórcio e empresa operadora
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



# ------------------------------------------------------------------------------
# Funcões de atualização da página de linhas
# ------------------------------------------------------------------------------

# Mudar o sentido da linha (simular clique)
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

# Mudar o dia da semana (simular clique)
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
# Dados GTFS das linhas
# ------------------------------------------------------------

trip <- sprintf('%s/trips.txt', pasta_gtfs)
trip <- read_delim(trip, delim = ',', col_types = 'ccccic')
trip <- trip %>% select(route_id, direction_id, service_id, trip_headsign)

linhas <- trip %>% select(route_id) %>% distinct()

# linhas %>% mutate(n = row_number()) %>% filter(route_id == '701A-10') # todos os dias da semana
# # linhas %>% mutate(n = row_number()) %>% filter(route_id == '1014-10') # só opera em dias úteis
# linhas %>% slice(869) # linhas[869, ]
#
# linha <- linhas[869, ]
# # linha <- linhas[3, ]


# ------------------------------------------------------------
# Puxar dados das páginas para cada linha existente no GTFS
# ------------------------------------------------------------

# Iniciar sessão do navegador
b <- ChromoteSession$new()

# Pasta de gravação dos arquivos
# data_base <- format(Sys.Date(), "%Y%m%d")
data_base <- Sys.Date()
pasta_grav <- file.path(pasta_base, 'linhas_collect', data_base)
pasta_part <- file.path(pasta_grav, 'partidas')
pasta_itin <- file.path(pasta_grav, 'itinerarios')
pasta_tmps <- file.path(pasta_grav, 'tempos')
dir.create(pasta_part, recursive = TRUE, showWarnings = FALSE)
dir.create(pasta_itin, recursive = TRUE, showWarnings = FALSE)
dir.create(pasta_tmps, recursive = TRUE, showWarnings = FALSE)

# Puxar informações para cada linha - dados serão gravados em arquivos
for (linha in linhas$route_id) {
  # linha <- linhas[3, ]
  # linha <- linhas[1, ]

  url <- sprintf("https://www.sptrans.com.br/itinerarios/linha/?numero=%s", linha)

  b$Page$navigate(url)

  # Esperar que DOM inicial passe a existir (IMPORTANTE: não usar loadEventFired
  # porque ele nunca termina e resulta em erro)
  Sys.sleep(5)


  # # Letreiro == Sentido
  # letreiro <- page %>%
  #   html_element("span.letreiroTitulo") %>%
  #   html_text(trim = TRUE)

  # Ler página da forma inicial (sentido = 0 e dia = 0)
  page <- get_page()

  # Quantos sentidos há na linha? 0 ou 1?
  sentidos <- get_sentidos(page)

  # Cobertura em dias da semana? 0, 1 ou 2?
  dias_semana <- get_dias_semana(page)


  # Combinações sentido x dias, para puxar dados a partir dessa combinação
  sentido_linha <- expand.grid(
    sentido = sentidos$sentido_id,
    dia = dias_semana$value,
    stringsAsFactors = FALSE
  )


  for (i in seq_along(sentido_linha$sentido)) {
    # i <- 1
    sentido <- as.integer(sentido_linha$sentido[i])
    dia     <- as.integer(sentido_linha$dia[i])

    letra_semana <- dias_semana$dia[dias_semana$value == as.character(dia)]
    if (letra_semana == 'Segunda a Sexta') {
      letra_semana <- 'U'
    } else if (letra_semana == 'Sábado') {
      letra_semana <- 'S'
    } else if (letra_semana == 'Domingo') {
      letra_semana <- 'D'
    }

    # Definir arquivos de saída
    out_base <- str_c(data_base, linha, sentido, letra_semana, sep = '_')
    out_partidas <- sprintf('%s/%s.csv', pasta_part, out_base)
    out_tempos   <- sprintf('%s/%s.csv', pasta_tmps, out_base)
    out_itinerarios <- sprintf('%s/%s.csv', pasta_itin, out_base)


    # Se algum dos arquivos de saída não existir, continuar
    if (!file.exists(out_partidas) | !file.exists(out_tempos) | !file.exists(out_itinerarios)) {
      # sentido <- 0; dia <- 0
      print(sprintf('Linha: %s - Sentido: %s - Dia: %s', linha, sentido, dia))


      # Atualizar página conforme dia e sentido
      set_dia_semana(dia)
      set_sentido(sentido)
      page <- get_page()


      if (!file.exists(out_partidas)) {
        partidas <- get_partidas(page)
        partidas <- partidas %>% mutate(data_base = data_base,
                                        linha = linha,
                                        sentido = sentido,
                                        dia_tipo = letra_semana,
                                        .before = 'hora')
        write_delim(partidas, out_partidas, delim = ';')

        arrow::write_dataset(
          partidas,
          path = file.path(pasta_base, 'linhas_collect/data/partidas'),
          format = "parquet",
          partitioning = c("data_base", "linha", "sentido", "dia_tipo")
        )
      }

      if (!file.exists(out_tempos)) {
        tempos_viagem <- get_tempos_viagem(page)
        tempos_viagem <- tempos_viagem %>% mutate(data_base = data_base,
                                        linha = linha,
                                        sentido = sentido,
                                        dia_tipo = letra_semana,
                                        .before = 'periodo')
        write_delim(tempos_viagem, out_tempos, delim = ';')

        arrow::write_dataset(
          tempos_viagem,
          path = file.path(pasta_base, 'linhas_collect/data/tempos_viagem'),
          format = "parquet",
          partitioning = c("data_base", "linha", "sentido", "dia_tipo")
        )
      }

      if (!file.exists(out_itinerarios)) {
        itinerario <- get_itinerario(page)
        itinerario <- itinerario %>% mutate(data_base = data_base,
                                            linha = linha,
                                            sentido = sentido,
                                            dia_tipo = letra_semana,
                                            .before = 'via')
        write_delim(itinerario, out_itinerarios, delim = ';')

        arrow::write_dataset(
          itinerario,
          path = file.path(pasta_base, 'linhas_collect/data/itinerario'),
          format = "parquet",
          partitioning = c("data_base", "linha", "sentido", "dia_tipo")
        )
      }

    }
  }
}




