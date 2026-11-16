library(tidyverse)
library(httr)
library(jsonlite)
library(arrow)
library(glue)

# =========================
# Configuration
# =========================

pasta_base  <- "/mnt/fern/Dados/projetos/2026_Monitoramento_Olho_Vivo"
pasta_json  <- file.path(pasta_base, "parquet_posicao")
dir.create(pasta_json, recursive = TRUE, showWarnings = FALSE)

token_path  <- sprintf('%s/token_olho_vivo.txt', pasta_base)
token       <- trimws(readLines(token_path, warn = FALSE))

base_url    <- "https://api.olhovivo.sptrans.com.br/v2.1"

poll_interval <- 20   # seconds
retry_pause   <- 20   # seconds
max_failures  <- 10

handle <- handle(base_url)

# =========================
# Authentication
# =========================

authenticate <- function() {
  message("Authenticating...")

  res <- POST(
    url    = paste0(base_url, "/Login/Autenticar?token=", token),
    # body   = list(token = token),
    encode = "form",
    handle = handle
  )

  stop_for_status(res)

  ok <- content(res, "text", encoding = "UTF-8")

  if (ok != "true") {
    stop("Authentication returned FALSE")
  }

  invisible(TRUE)
}

# =========================
# Safe GET wrapper
# =========================

safe_get_json <- function(endpoint) {

  res <- GET(
    url    = paste0(base_url, endpoint),
    handle = handle
  )

  # session expired
  if (status_code(res) == 401) {
    message("Session expired. Re-authenticating...")
    authenticate()

    res <- GET(
      url    = paste0(base_url, endpoint),
      handle = handle
    )
  }

  stop_for_status(res)

  fromJSON(content(res, "text", encoding = "UTF-8"),
           flatten = TRUE)
}

# =========================
# Data collection
# =========================

get_positions <- function() {
  json <- safe_get_json("/Posicao")
  json$l %>% unnest(vs) %>% select(-any_of(c("lt0","lt1","qv","a","sv","is")))
}

# =========================
# File writer
# =========================

write_snapshot <- function(df) {
  filename <- glue("{format(Sys.time(), '%Y%m%d_%H%M%S')}.parquet")
  write_parquet(df, file.path(pasta_json, filename))
}

# =========================
# Main loop
# =========================

run_collector <- function() {

  authenticate()

  failures <- 0

  repeat {

    tryCatch({
      message("Collecting at ", Sys.time())

      df <- get_positions()
      write_snapshot(df)
      failures <- 0
      Sys.sleep(poll_interval)

    }, error = function(e) {
      failures <<- failures + 1

      message("Error: ", e$message)
      message("Failure #", failures)

      if (failures >= max_failures) {
        stop("Max failures reached. Aborting.")
      }

      message("Pausing ", retry_pause, " seconds before retry.")
      Sys.sleep(retry_pause)
    })
  }
}

# =========================
# Start
# =========================

run_collector()