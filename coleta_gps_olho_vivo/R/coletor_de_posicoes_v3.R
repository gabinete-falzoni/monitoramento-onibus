library(tidyverse)
library(httr)
library(jsonlite)
library(arrow)
library(glue)
# library(sendmailR)

# =========================
# Configuration
# =========================

# Caminhos configuráveis por variáveis de ambiente. Padrão: pasta x_dados/ na
# raiz do repositório (fora do git).
#   OLHOVIVO_DADOS_DIR   pasta de dados          (padrão: <repo>/x_dados)
#   OLHOVIVO_TOKEN       token da API Olho Vivo  (ou, se vazio, lido de
#   OLHOVIVO_TOKEN_FILE  arquivo com o token     <dados>/token_olho_vivo.txt)
#   OLHOVIVO_SAIDA_GPS   onde gravar os .parquet (padrão: <dados>/gps_collect)
dir_script <- local({
  f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
  if (length(f)) dirname(normalizePath(f[1])) else getwd()
})
pasta_base <- Sys.getenv("OLHOVIVO_DADOS_DIR",
                         normalizePath(file.path(dir_script, "..", "..", "x_dados"), mustWork = FALSE))
pasta_out  <- Sys.getenv("OLHOVIVO_SAIDA_GPS", file.path(pasta_base, "gps_collect"))
dir.create(pasta_out, recursive = TRUE, showWarnings = FALSE)

token <- Sys.getenv("OLHOVIVO_TOKEN")
if (!nzchar(token)) {
  token_path <- Sys.getenv("OLHOVIVO_TOKEN_FILE", file.path(pasta_base, "token_olho_vivo.txt"))
  if (!file.exists(token_path))
    stop("Token da API Olho Vivo não encontrado: defina OLHOVIVO_TOKEN ou crie ", token_path)
  token <- trimws(readLines(token_path, warn = FALSE))
}

base_url    <- "https://api.olhovivo.sptrans.com.br/v2.1"

poll_interval    <- 30   # seconds
retry_pause      <- 10   # seconds
max_failures     <- 30   # retryable errors only
auth_max_retries <- 10   # auth retries

handle <- handle(base_url)

# =========================
# Email Alert (with cooldown)
# =========================

last_auth_alert <- NULL
auth_alert_cooldown <- 300  # seconds

should_send_auth_alert <- function() {
  now <- Sys.time()

  if (is.null(last_auth_alert) ||
      difftime(now, last_auth_alert, units = "secs") > auth_alert_cooldown) {

    last_auth_alert <<- now
    return(TRUE)
  }

  FALSE
}

# send_alert_email <- function(msg) {
#
#   from <- "monitor@yourdomain.com"
#   to   <- "you@yourdomain.com"
#
#   subject <- "🚨 Olho Vivo Collector - AUTH ERROR"
#
#   body <- paste(
#     "Authentication failure detected:\n\n",
#     msg,
#     "\n\nTime:", Sys.time()
#   )
#
#   tryCatch({
#     sendmail(
#       from = from,
#       to = to,
#       subject = subject,
#       msg = body,
#       control = list(smtpServer = "your.smtp.server")
#     )
#   }, error = function(e) {
#     message("Failed to send alert email: ", e$message)
#   })
# }

# =========================
# Connectivity Check
# =========================

has_internet <- function() {
  tryCatch({
    GET("https://www.google.com", timeout(3))
    TRUE
  }, error = function(e) FALSE)
}

# =========================
# Custom error helpers
# =========================

auth_error <- function(msg) {
  structure(list(message = msg), class = c("auth_error", "error", "condition"))
}

retryable_error <- function(msg) {
  structure(list(message = msg), class = c("retryable_error", "error", "condition"))
}

# =========================
# Authentication
# =========================

authenticate <- function() {

  message("Authenticating...")

  res <- tryCatch({
    POST(
      url    = paste0(base_url, "/Login/Autenticar?token=", token),
      encode = "form",
      handle = handle,
      timeout(10)
    )
  }, error = function(e) {
    stop(retryable_error(paste("Auth request failed:", e$message)))
  })

  if (status_code(res) >= 500) {
    stop(retryable_error(paste("Auth server error:", status_code(res))))
  }

  if (status_code(res) == 401) {
    stop(auth_error("Authentication unauthorized (401) - invalid token"))
  }

  stop_for_status(res)

  ok <- content(res, "text", encoding = "UTF-8")

  if (ok != "true") {
    stop(auth_error("Authentication returned FALSE - invalid token"))
  }

  invisible(TRUE)
}

# =========================
# Safe GET wrapper
# =========================

safe_get_json <- function(endpoint) {

  res <- tryCatch({
    GET(
      url    = paste0(base_url, endpoint),
      handle = handle,
      timeout(10)
    )
  }, error = function(e) {
    stop(retryable_error(paste("GET failed:", e$message)))
  })

  # Session expired → try re-auth
  if (status_code(res) == 401) {
    message("Session expired. Re-authenticating...")

    tryCatch({
      authenticate()
    }, error = function(e) {
      stop(auth_error("Re-authentication failed"))
    })

    res <- GET(
      url    = paste0(base_url, endpoint),
      handle = handle,
      timeout(10)
    )
  }

  if (status_code(res) >= 500) {
    stop(retryable_error(paste("Server error:", status_code(res))))
  }

  stop_for_status(res)

  fromJSON(content(res, "text", encoding = "UTF-8"), flatten = TRUE)
}

# =========================
# Data collection
# =========================

get_positions <- function() {
  json <- safe_get_json("/Posicao")

  json$l %>%
    unnest(vs) %>%
    select(-any_of(c("lt0","lt1","qv","a","sv","is")))
}

# =========================
# File writer
# =========================

write_snapshot <- function(df) {
  filename <- glue("{format(Sys.time(), '%Y%m%d_%H%M%S')}.parquet")
  write_parquet(df, file.path(pasta_out, filename))
}

# =========================
# Main loop
# =========================

run_collector <- function() {

  authenticate()

  failures <- 0

  repeat {

    tryCatch({

      # message("Collecting at ", Sys.time())

      df <- get_positions()
      write_snapshot(df)

      failures <- 0
      Sys.sleep(poll_interval)

    },

    retryable_error = function(e) {

      failures <<- failures + 1

      message("Retryable error: ", e$message)
      message("Failure #", failures)

      if (!has_internet()) {
        message("No internet connection detected.")
      }

      if (failures >= max_failures) {
        stop("Max retryable failures reached. Aborting.")
      }

      message("Retrying in ", retry_pause, " seconds...")
      Sys.sleep(retry_pause)
    },

    auth_error = function(e) {

      message("AUTH ERROR: ", e$message)

      # if (should_send_auth_alert()) {
      #   send_alert_email(e$message)
      # }

      success <- FALSE

      for (i in seq_len(auth_max_retries)) {

        message("Auth retry attempt ", i, "/", auth_max_retries)

        Sys.sleep(retry_pause)

        success <- tryCatch({
          authenticate()
          TRUE
        }, error = function(err) {
          message("Retry failed: ", err$message)
          FALSE
        })

        if (success) {
          message("Re-authentication succeeded. Resuming operation.")
          break
        }
      }

      if (!success) {
        stop("Authentication failed after retries. Aborting.")
      }
    },

    error = function(e) {

      failures <<- failures + 1

      message("Unknown error: ", e$message)
      message("Failure #", failures)

      if (failures >= max_failures) {
        stop("Max failures reached. Aborting.")
      }

      Sys.sleep(retry_pause)
    })
  }
}

# =========================
# Start
# =========================

# run_collector()

run_with_restart <- function() {

  repeat {

    message("=== Starting collector cycle at ", Sys.time(), " ===")

    # Optional: reset global/session state
    # (not strictly needed here)
    gc()

    tryCatch({

      run_collector()

    }, error = function(e) {

      message("FATAL ERROR: ", e$message)

      message("Waiting 10 minutes (possible API maintenance)...")
      Sys.sleep(600)

      message("Reinitializing everything...")
    })
  }
}

run_with_restart()
