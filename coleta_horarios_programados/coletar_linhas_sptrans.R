#!/usr/bin/env Rscript
################################################################################
# coletar_linhas_sptrans.R — programação das linhas (partidas, itinerários,
# tempos estimados) publicada em https://www.sptrans.com.br/itinerarios/linha/
#
# Substitui y_coletar_horarios_linhas_teste_v6_helper.R.
#
# Como funciona: a página da linha não traz os dados no HTML; ela os baixa, em
# JSON, de uma API pública (itinerariosapi.sptrans.com.br) e só então os desenha
# conforme o dia/sentido escolhido. Este script consulta essa mesma API:
#   1 requisição  -> catálogo de linhas (RetornarLinhasTeste)
#   1 requisição/linha -> detalhe completo (BuscarDetalheLinhaTeste): partidas,
#                        itinerários e tempos de TODOS os dias e sentidos.
# Nenhum navegador, nenhum clique, nenhuma espera por renderização.
#
# Uso:   Rscript coletar_linhas_sptrans.R [--saida DIR] [--intervalo SEG]
#                                         [--linhas 701A-10,7267-10] [--refazer]
# Ver README.md para detalhes.
################################################################################

suppressPackageStartupMessages({
  library(httr2)
  library(jsonlite)
  library(data.table)
  library(arrow)
})

# ------------------------------------------------------------------------------
# Configuração (pode ser sobrescrita pela linha de comando)
# ------------------------------------------------------------------------------
args_cli <- commandArgs(trailingOnly = TRUE)
arg <- function(nome, padrao = NULL) {
  i <- match(paste0("--", nome), args_cli)
  if (is.na(i)) return(padrao)
  if (i == length(args_cli) || startsWith(args_cli[i + 1], "--")) return(TRUE)
  args_cli[i + 1]
}

dir_script <- local({
  f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
  if (length(f)) dirname(normalizePath(f[1])) else getwd()
})

cfg <- list(
  api        = "https://itinerariosapi.sptrans.com.br",
  # pasta de saída: <OLHOVIVO_DADOS_DIR ou raiz/x_dados>/linhas_collect/api
  saida      = arg("saida", file.path(Sys.getenv("OLHOVIVO_DADOS_DIR",
                                                 file.path(dirname(dir_script), "x_dados")),
                                      "linhas_collect", "api")),
  intervalo  = as.numeric(arg("intervalo", 0.5)),  # segundos entre requisições
  linhas     = arg("linhas", NULL),                # filtro opcional (vírgulas)
  refazer    = isTRUE(arg("refazer", FALSE)),      # ignora JSON já baixado hoje
  data_coleta = as.character(arg("data", format(Sys.Date(), "%Y-%m-%d"))),
  max_falhas_seguidas = 20L                        # aborta se o site cair
)

ua <- "coleta-programacao-linhas/2.0 (pesquisa; consulta sequencial e espacada; R httr2)"

dir_bruto <- file.path(cfg$saida, "bruto", cfg$data_coleta)
dir.create(dir_bruto, recursive = TRUE, showWarnings = FALSE)

msg <- function(...) cat(format(Sys.time(), "%H:%M:%S"), sprintf(...), "\n")

# ------------------------------------------------------------------------------
# Requisições
# ------------------------------------------------------------------------------
req_api <- function(endpoint, corpo = NULL) {
  r <- request(cfg$api) |>
    req_url_path_append(endpoint) |>
    req_method("POST") |>
    req_user_agent(ua) |>
    req_headers(Origin = "https://www.sptrans.com.br",
                Referer = "https://www.sptrans.com.br/") |>
    req_timeout(60) |>
    # 429/5xx/erros de rede: até 5 tentativas com espera exponencial
    # (respeita Retry-After se o servidor enviar)
    req_retry(max_tries = 5, backoff = function(i) min(60, 2^i),
              is_transient = function(resp) resp_status(resp) %in% c(429, 500, 502, 503, 504))
  if (is.null(corpo)) r <- r |> req_body_raw("", type = "application/json")
  else r <- r |> req_body_json(corpo, auto_unbox = TRUE)
  r
}

baixar_json <- function(endpoint, corpo = NULL) {
  resp <- req_perform(req_api(endpoint, corpo))
  resp_body_string(resp, encoding = "UTF-8")
}

gravar_gz <- function(txt, arq) {
  tmp <- paste0(arq, ".tmp")
  con <- gzfile(tmp, "w", encoding = "UTF-8"); writeLines(txt, con); close(con)
  file.rename(tmp, arq)   # escrita atômica: arquivo parcial nunca vira "pronto"
}
ler_gz <- function(arq) paste(readLines(gzfile(arq, encoding = "UTF-8"), warn = FALSE), collapse = "\n")

# ------------------------------------------------------------------------------
# 1. Catálogo
# ------------------------------------------------------------------------------
t0 <- Sys.time()
msg("Saída: %s | data de coleta: %s | intervalo: %.2fs", cfg$saida, cfg$data_coleta, cfg$intervalo)

arq_cat <- file.path(dir_bruto, "_catalogo.json.gz")
if (!file.exists(arq_cat) || cfg$refazer) invisible(gravar_gz(baixar_json("RetornarLinhasTeste"), arq_cat))
catalogo <- as.data.table(fromJSON(ler_gz(arq_cat)))
catalogo[, codigo := trimws(codigo)]
msg("Catálogo: %d linhas", nrow(catalogo))

alvo <- catalogo
if (!is.null(cfg$linhas)) alvo <- catalogo[codigo %in% strsplit(cfg$linhas, ",")[[1]]]

# ------------------------------------------------------------------------------
# 2. Detalhe de cada linha (sequencial, com intervalo fixo)
# ------------------------------------------------------------------------------
nome_arq <- function(codigo) file.path(dir_bruto, paste0(gsub("[^A-Za-z0-9-]", "_", codigo), ".json.gz"))
alvo[, arquivo := nome_arq(codigo)]
pendentes <- if (cfg$refazer) alvo else alvo[!file.exists(arquivo)]
msg("A baixar: %d (já baixadas hoje: %d)", nrow(pendentes), nrow(alvo) - nrow(pendentes))

log_coleta <- vector("list", nrow(pendentes))
falhas_seguidas <- 0L
for (i in seq_len(nrow(pendentes))) {
  p <- pendentes[i]
  ini <- Sys.time()
  res <- tryCatch({
    txt <- baixar_json("BuscarDetalheLinhaTeste", list(codPlanejamento = p$CdPjOID))
    gravar_gz(txt, p$arquivo)
    "ok"
  }, error = function(e) conditionMessage(e))
  log_coleta[[i]] <- data.table(codigo = p$codigo, CdPjOID = p$CdPjOID, status = res,
                                momento = format(ini, "%Y-%m-%d %H:%M:%S"))
  falhas_seguidas <- if (res == "ok") 0L else falhas_seguidas + 1L
  if (res != "ok") msg("FALHA %s: %s", p$codigo, res)
  if (falhas_seguidas >= cfg$max_falhas_seguidas) {
    msg("%d falhas seguidas — abortando para não insistir contra o servidor. Rode de novo mais tarde (retoma de onde parou).",
        falhas_seguidas)
    break
  }
  if (i %% 100 == 0) msg("%d/%d", i, nrow(pendentes))
  Sys.sleep(max(0, cfg$intervalo - as.numeric(difftime(Sys.time(), ini, units = "secs"))))
}
log_coleta <- rbindlist(log_coleta)
if (nrow(log_coleta))
  fwrite(log_coleta, file.path(dir_bruto, sprintf("_log_%s.csv", format(t0, "%H%M%S"))))

# ------------------------------------------------------------------------------
# 3. Tabelas normalizadas (a partir dos JSON brutos do dia)
# ------------------------------------------------------------------------------
TIPO_DIA <- c("0" = "U", "1" = "S", "2" = "D")
nz <- function(x, v = NA) if (is.null(x) || length(x) == 0) v else x

parse_linha <- function(cod, cdpj, arq) {
  j <- fromJSON(ler_gz(arq), simplifyVector = FALSE)
  if (is.character(j))   # a API às vezes responde só uma mensagem de erro (texto)
    return(list(linhas = data.table(codigo = cod, CdPjOID = cdpj, erro_api = paste(j, collapse = " "))))
  if (!length(j) || !is.null(j$codigoErro)) {
    return(list(linhas = data.table(codigo = cod, CdPjOID = cdpj,
                                    erro_api = as.character(nz(j$codigoErro, "resposta_vazia")))))
  }

  # --- partidas
  partidas <- rbindlist(lapply(c(Ida = 0L, Volta = 1L), function(s) {
    pp <- j[[paste0("partidas", c("Ida", "Volta")[s + 1])]]
    rbindlist(lapply(pp, function(f) {
      h <- f$horariosProgramados
      if (!length(h)) return(NULL)
      data.table(sentido = s, tipo_dia = TIPO_DIA[as.character(f$tipoDia)],
                 faixa_horaria = as.integer(f$faixaHoraria),
                 hora = vapply(h, function(x) nz(x$horario, NA_character_), ""),
                 veiculo_acessivel = vapply(h, function(x) isTRUE(x$veiculoAcessivel), TRUE))
    }))
  }))
  if (nrow(partidas)) partidas[, ordem := seq_len(.N), by = .(sentido, tipo_dia)]
  else partidas <- data.table(sentido = integer(), tipo_dia = character(), faixa_horaria = integer(),
                              hora = character(), veiculo_acessivel = logical(), ordem = integer())

  # --- itinerários e eventos na via
  eventos <- list()
  itinerarios <- rbindlist(lapply(0:1, function(s) {
    it <- j[[paste0("itinerarios", c("Ida", "Volta")[s + 1])]]
    if (!length(it)) return(NULL)
    d <- data.table(sentido = s,
                    tipo_dia = TIPO_DIA[as.character(vapply(it, function(x) nz(x$tipoDia, NA), 0))],
                    via = vapply(it, function(x) nz(x$nomeVia, NA_character_), ""),
                    numeral = vapply(it, function(x) nz(x$numVia, NA_character_), ""),
                    n_eventos = vapply(it, function(x) length(x$eventos), 0L))
    d[, ordem := seq_len(.N), by = tipo_dia]
    for (k in which(d$n_eventos > 0)) {
      ev <- rbindlist(lapply(it[[k]]$eventos, function(e)
        as.data.table(lapply(e, function(v) if (is.null(v)) NA else as.character(v)))), fill = TRUE)
      ev[, `:=`(sentido = s, tipo_dia = d$tipo_dia[k], ordem = d$ordem[k], via = d$via[k])]
      eventos[[length(eventos) + 1]] <<- ev
    }
    d
  }))
  eventos <- rbindlist(eventos, fill = TRUE)

  # --- tempos estimados (inicial* = sentido 0, final* = sentido 1)
  tempos <- rbindlist(lapply(j$temposEstimados, function(t) {
    data.table(tipo_dia = TIPO_DIA[as.character(t$tipoDia)],
               sentido = rep(0:1, each = 3),
               periodo = rep(c("manha", "entrepico", "tarde"), 2),
               minutos = as.integer(c(nz(t$inicialManha), nz(t$inicialEntrePico), nz(t$inicialTarde),
                                      nz(t$finalManha), nz(t$finalEntrePico), nz(t$finalTarde))))
  }))
  # a página mostra "-" quando o valor é <= 1
  if (nrow(tempos)) tempos[!(minutos > 1), minutos := NA_integer_]

  # --- horário de operação (primeira-última partida) por tipo de dia
  horarios <- rbindlist(lapply(j$horarios, function(h) data.table(
    tipo_dia = TIPO_DIA[as.character(h$tipoDia)],
    operacao_ponto_inicial = nz(h$horarioPontoInicial, NA_character_),
    operacao_ponto_final = nz(h$horarioPontoFinal, NA_character_))))

  # --- metadados da linha
  n_ida <- partidas[sentido == 0, .N]; n_volta <- partidas[sentido == 1, .N]
  # mesma regra da página para exibir a linha como "Circular" (um só sentido)
  td_ref <- if (partidas[tipo_dia == "U", .N]) "U" else if (partidas[tipo_dia == "S", .N]) "S" else "D"
  s_ref <- if (n_volta > 0 && n_ida == 0) 1L else 0L
  it_ref <- if (nrow(itinerarios) && "via" %in% names(itinerarios)) itinerarios[sentido == s_ref & tipo_dia == td_ref, via] else character()
  tempo_final_tarde <- vapply(j$temposEstimados, function(t) isTRUE(nz(t$finalTarde, 0) > 0), TRUE)
  circular <- (n_ida + n_volta) > 0 && length(it_ref) > 0 && it_ref[1] == it_ref[length(it_ref)] &&
    !any(tempo_final_tarde) && !identical(as.integer(nz(j$area, -1)), 99L)

  linhas <- data.table(
    codigo = cod, CdPjOID = cdpj, codigo_linha = nz(j$codigoLinha, NA_character_),
    nome = nz(j$nomeLinha, NA_character_), area = as.integer(nz(j$area, NA)),
    consorcio = nz(j$consorcio, NA_character_), empresa = nz(j$empresa, NA_character_),
    letreiro_ida = nz(j$letreiroIda, NA_character_), letreiro_volta = nz(j$letreiroVolta, NA_character_),
    circular = circular,
    opera_U = partidas[tipo_dia == "U", .N] > 0, opera_S = partidas[tipo_dia == "S", .N] > 0,
    opera_D = partidas[tipo_dia == "D", .N] > 0,
    n_partidas_U = partidas[tipo_dia == "U", .N], n_partidas_S = partidas[tipo_dia == "S", .N],
    n_partidas_D = partidas[tipo_dia == "D", .N],
    erro_api = NA_character_)

  add <- function(d) if (nrow(d)) d[, `:=`(codigo = cod)][] else d
  list(linhas = linhas, partidas = add(partidas), itinerarios = add(itinerarios),
       eventos = add(eventos), tempos = add(tempos), horarios_operacao = add(horarios))
}

msg("Processando JSON brutos...")
baixados <- alvo[file.exists(arquivo)]
partes <- lapply(seq_len(nrow(baixados)), function(i) {
  tryCatch(parse_linha(baixados$codigo[i], baixados$CdPjOID[i], baixados$arquivo[i]),
           error = function(e) list(linhas = data.table(codigo = baixados$codigo[i],
                                                        CdPjOID = baixados$CdPjOID[i],
                                                        erro_api = paste("parse:", conditionMessage(e)))))
})
tabelas <- c("linhas", "partidas", "itinerarios", "eventos", "tempos", "horarios_operacao")
dts <- setNames(lapply(tabelas, function(tb) rbindlist(lapply(partes, `[[`, tb), fill = TRUE)), tabelas)

# Linhas "-1" (e 8700-24) são atendimentos auxiliares que a página da SPTrans
# exibe fundidos à linha principal (ex.: 8000-1 aparece dentro de 8000-10).
# Aqui cada uma fica com o próprio código; a coluna indica a linha que a exibe.
cat_aux <- catalogo[, .(codigo, letreiro_site = letreiro, area_vigente = AreCodVig)]
dts$linhas <- cat_aux[dts$linhas, on = "codigo"]
dts$linhas[, exibida_em := fifelse(grepl("-1$", codigo), paste0(codigo, "0"),
                           fifelse(codigo == "8700-24", "8700-21", NA_character_))]
dts$linhas[, data_coleta := as.Date(cfg$data_coleta)]

# Gravação: uma tabela por tipo de informação, um arquivo parquet por coleta
#   <saida>/<tabela>/data_coleta=AAAA-MM-DD/part-0.parquet
for (tb in tabelas) {
  d <- dts[[tb]]
  if (!nrow(d)) next
  if ("data_coleta" %in% names(d)) d[, data_coleta := NULL]
  setcolorder(d, intersect(c("codigo", "sentido", "tipo_dia", "ordem"), names(d)))
  dd <- file.path(cfg$saida, tb, paste0("data_coleta=", cfg$data_coleta))
  dir.create(dd, recursive = TRUE, showWarnings = FALSE)
  write_parquet(d, file.path(dd, "part-0.parquet"))
}

msg("Concluído em %.1f min: %d linhas (%d com erro na API), %d partidas, %d trechos de itinerário, %d eventos.",
    as.numeric(difftime(Sys.time(), t0, units = "mins")), nrow(dts$linhas),
    dts$linhas[!is.na(erro_api), .N], nrow(dts$partidas), nrow(dts$itinerarios), nrow(dts$eventos))
