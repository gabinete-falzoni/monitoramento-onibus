############################################################
# rodar.R — inicia o app de auditoria
#   Rscript app_auditoria/rodar.R              (porta 8766)
#   Rscript app_auditoria/rodar.R --porta 9000
############################################################
a <- commandArgs(trailingOnly = TRUE)
porta <- if (length(i <- which(a == "--porta"))) as.integer(a[i + 1]) else 8766L
f_arg <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
APP_DIR <- if (length(f_arg)) normalizePath(dirname(f_arg)) else normalizePath("app_auditoria")
message("App de auditoria em http://127.0.0.1:", porta)
shiny::runApp(shiny::shinyAppFile(file.path(APP_DIR, "app.R")), port = porta, host = "127.0.0.1",
              launch.browser = interactive())
