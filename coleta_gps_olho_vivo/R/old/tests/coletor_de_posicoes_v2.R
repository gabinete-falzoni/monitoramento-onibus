library(tidyverse)
library(httr)
library(jsonlite)
library(arrow)
library(glue)
# library(sendmailR)

# =========================
# Configuration
# =========================

pasta_base  <- "/mnt/fern/Dados/projetos/2026_Monitoramento_Olho_Vivo"
pasta_json  <- file.path(pasta_base, "parquet_posicao")
dir.create(pasta_json, recursive = TRUE, showWarnings = FALSE)

token_path  <- sprintf('%s/token_olho_vivo.txt', pasta_base)
token       <- trimws(readLines(token_path, warn = FALSE))

base_url    <- "https://api.olhovivo.sptrans.com.br/v2.1"

poll_interval    <- 25   # seconds
retry_pause      <- 10    # seconds
max_failures     <- 25   # retryable errors only
auth_max_retries <- 5    # auth retries

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

run_collector()