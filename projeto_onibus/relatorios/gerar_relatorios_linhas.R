############################################################
# gerar_relatorios_linhas.R — um relatório HTML por linha × mês
#
#   Rscript relatorios/gerar_relatorios_linhas.R             (todas as linhas/meses)
#   Rscript relatorios/gerar_relatorios_linhas.R 2026-09     (só um mês)
#   Rscript relatorios/gerar_relatorios_linhas.R 2026-09 701A-10 7267-10
#
# Saída: relatorios/linhas/<mes>/<linha>.html
############################################################
args <- commandArgs(trailingOnly = TRUE)
f_arg <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
REL_DIR <- if (length(f_arg)) normalizePath(dirname(f_arg)) else normalizePath("relatorios")
PROJ_DIR <- dirname(REL_DIR)
source(file.path(PROJ_DIR, "R", "00_config.R"))

base <- file.path(cfg$saida_dir, "auditoria_saidas")
parts <- list.dirs(base, recursive = TRUE, full.names = FALSE)
parts <- parts[grepl("^mes=[^/]+/linha=[^/]+$", parts)]
idx <- data.frame(mes = sub("^mes=([^/]+)/.*", "\\1", parts), linha = sub(".*/linha=", "", parts))
if (length(args) >= 1) idx <- idx[idx$mes %in% args[1], ]
if (length(args) >= 2) idx <- idx[idx$linha %in% args[-1], ]

meses_pt <- c("janeiro", "fevereiro", "março", "abril", "maio", "junho", "julho", "agosto",
              "setembro", "outubro", "novembro", "dezembro")
setwd(REL_DIR)
qmd <- "relatorio_linha.qmd"
for (i in seq_len(nrow(idx))) {
  m <- idx$mes[i]; l <- idx$linha[i]
  rot <- sprintf("%s de %s", meses_pt[as.integer(substr(m, 6, 7))], substr(m, 1, 4))
  dest_dir <- file.path(REL_DIR, "linhas", m)
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  nome <- paste0(l, ".html")
  message(sprintf("[%d/%d] %s %s", i, nrow(idx), m, l))
  st <- system2("quarto", c("render", shQuote(qmd), "-P", paste0("linha:", l), "-P", paste0("mes:", m),
                            "-P", shQuote(paste0("mes_rotulo:", rot)), "--output", nome, "--quiet"))
  if (st == 0) file.rename(file.path(REL_DIR, nome), file.path(dest_dir, nome))
  else warning("falhou: ", m, " ", l)
}
