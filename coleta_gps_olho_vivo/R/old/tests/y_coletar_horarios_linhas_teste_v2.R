library(chromote)
library(rvest)
library(purrr)
library(dplyr)
library(tibble)
library(xml2)

# ------------------------------------------------------------
# 1. Start browser session
# ------------------------------------------------------------
b <- ChromoteSession$new()

url <- "https://www.sptrans.com.br/itinerarios/linha/?numero=701A-10"

b$Page$navigate(url)

# Wait for initial DOM to exist (IMPORTANT: do NOT use loadEventFired)
Sys.sleep(5)

# ------------------------------------------------------------
# 2. Get available sentidos from dropdown
# ------------------------------------------------------------
get_sentidos <- function() {
  html <- b$Runtime$evaluate("document.documentElement.outerHTML")$result$value
  page <- read_html(html)

  options <- page %>%
    html_elements("select#sel_sentido option")

  tibble(
    sentido_id = as.integer(html_attr(options, "value")),
    label = html_text(options, trim = TRUE)
  )
}

sentidos <- get_sentidos()

# ------------------------------------------------------------
# 3. Function: change sentido (simulate click)
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

# ------------------------------------------------------------
# 4. Function: extract updated DOM
# ------------------------------------------------------------
get_page <- function() {
  html <- b$Runtime$evaluate(
    "document.documentElement.outerHTML"
  )$result$value

  read_html(html)
}

# ------------------------------------------------------------
# 5. Parser
# ------------------------------------------------------------
parse_page <- function(page, sentido_id) {

  # Letreiro
  letreiro <- page %>%
    html_element("span.letreiroTitulo") %>%
    html_text(trim = TRUE)

  # # Itinerario -  Parse as pure text (no delimiters)
  # itinerario <- page %>%
  #   html_element("div.tabelaItinerario") %>%
  #   html_text(trim = TRUE)

  # # Itinerario - Parse as table
  # itinerario <- page %>%
  #   html_element("div.tabelaItinerario") %>%
  #   html_element("table") %>%
  #   html_table() %>%
  #   rename(
  #     evento_na_via = 1,
  #     via = Via,
  #     numeral = `N°`
  #   )

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


  # Partidas
  partidas <- page %>%
    html_element("div.tabelaPartidas") %>%
    html_elements(".partidas span")

  partidas <- tibble(
    hora = html_text(partidas, trim = TRUE),
    invert = html_attr(partidas, "class") == "invert"
  )


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



# ------------------------------------------------------------
# 6. Full workflow per sentido
# ------------------------------------------------------------
get_sentido_data <- function(sentido_id) {
  # sentido_id <- 0
  set_sentido(sentido_id)

  page <- get_page()

  parse_page(page, sentido_id)
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