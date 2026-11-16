############################################################
# run_pipeline.R — execução completa, na ordem
#
#   Rscript run_pipeline.R            (a partir da pasta projeto_onibus)
#
# Etapa 1 (reconstrução): 01 normalização/QC -> 02 itinerários -> 03 map
#   matching -> 04 viagens -> 05 passagens por ponto -> 08 indicadores
# Etapa 2 (auditoria):    06 programação -> 07 associação/IPP
# Validação:              09 sensibilidade + verificações automáticas
#
# Unidade de processamento = linha (todas as datas dos arquivos de
# entrada). Linhas rodam em paralelo (cfg$n_cores). Saídas detalhadas são
# gravadas como datasets Parquet particionados por mes=/linha=, para que
# o acúmulo de todas as linhas e meses possa ser consultado por arrow/duckdb.
############################################################

t_inicio <- Sys.time()
args <- commandArgs(trailingOnly = FALSE)
f_arg <- sub("^--file=", "", args[grep("^--file=", args)])
PROJ_DIR <- if (length(f_arg)) normalizePath(dirname(f_arg)) else normalizePath(".")

suppressPackageStartupMessages({
  library(data.table); library(arrow); library(sf); library(lubridate); library(ggplot2)
})
source(file.path(PROJ_DIR, "R", "00_config.R"))
Rcpp::sourceCpp(file.path(PROJ_DIR, "src", "mapmatch.cpp"), cacheDir = file.path(PROJ_DIR, "src", ".cache"))
for (f in c("01_normalizacao_qc.R", "02_referencia.R", "03_mapmatching.R", "04_viagens.R",
            "05_passagens.R", "06_programacao.R", "07_auditoria.R", "08_indicadores.R",
            "09_sensibilidade.R", "10_exportacao.R", "12_frota.R", "13_icv.R"))
  source(file.path(PROJ_DIR, "R", f))

OUT <- cfg$saida_dir
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)
data.table::setDTthreads(max(1L, floor(parallel::detectCores() / cfg$n_cores)))

# ------------------------------------------------------------- inventário
message("[1] Inventário dos arquivos")
inv <- inventario_arquivos()
fwrite(inv, file.path(OUT, "inventario_dados.csv"))

# período efetivamente coberto pela coleta (UTC, segundos)
periodo_coleta <- as.numeric(as.POSIXct(c(min(inv$ta_min_utc), max(inv$ta_max_utc)),
                                        format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))
ds <- abrir_posicoes()
linhas <- cfg$linhas %||% listar_linhas(ds)
message("    linhas: ", paste(linhas, collapse = ", "))

# ------------------------------------------------------------- referências
message("[2] GTFS e raspagens")
gtfs_list <- lapply(cfg$entrada$gtfs_dirs, ler_gtfs, linhas = linhas)
pt_all <- ler_partidas_raspagem(linhas)

# ------------------------------------------------------------- por linha
processar_linha <- function(l) {
  t0 <- Sys.time()
  dt <- qc_posicoes(ler_posicoes_linha(ds, l))
  qual <- resumo_qualidade(dt, l)
  esc <- escolher_ciclo(gtfs_list, l, dt[valido == TRUE, x], dt[valido == TRUE, y])
  ciclo <- esc$ciclo
  if (is.null(ciclo)) return(list(linha = l, qual = qual, gtfs = esc$diag, erro = "sem_itinerario_gtfs"))

  v <- mapmatch_linha(dt, ciclo)
  ex <- extrair_viagens(v, ciclo)
  tr <- classificar_viagens(ex$viagens, ciclo, l)
  pas <- estimar_passagens(tr, ex$obs, ciclo)
  traj <- resumir_trajetorias(v, l)
  vd <- classificar_veiculo_dia(v, tr, l)
  ih <- indicadores_horarios_linha(v, tr, l)
  vh <- veiculos_por_hora_linha(v, tr, l)
  dsl <- diagnostico_sl(v, ciclo)

  datas <- sort(unique(dt$service_date))
  prog_l <- montar_programacao(l, datas, pt_all[linha == l])
  lac <- lacunas_em_servico(v)
  aud <- auditar_linha(prog_l, tr, ex$obs, l, lacunas = lac, periodo_coleta = periodo_coleta)
  sens <- if (isTRUE(cfg$rodar_sensibilidade)) sensibilidade_linha(dt, ciclo, prog_l, l, periodo_coleta) else NULL

  # base normalizada (todos os registros, válidos ou não) + resultado do map matching
  mmcols <- c("source_row_id", "s", "d_rota", "chain", "u_mono", "sentido_inferido", "quebra", "na_rota")
  norm <- v[, ..mmcols][dt, on = "source_row_id"]
  norm[, c("x", "y", "tnum") := NULL]

  fr <- diagnosticar_frota(norm, tr, aud$auditoria, ciclo$sentidos, ciclo$L, l)
  rc <- reconciliar_status(aud$auditoria, fr$saidas)
  aud$auditoria <- rc$au; fr$saidas <- rc$sd
  icv <- icv_faixas(aud$auditoria, tr, l, ciclo$circular)

  gravar_linha(l, list(diagnostico_saidas = fr$saidas, lacunas_veiculos = fr$lacunas,
                       veiculos_dia_frota = fr$veiculos, icv_faixas = icv,
                       posicoes_normalizadas = norm, viagens_reconstruidas = tr,
                       passagens_pontos = pas, trajetorias_reconstruidas = traj,
                       auditoria_saidas = aud$auditoria, programacao_normalizada = prog_l))
  gravar_tracado(l, ciclo, format(datas, "%Y-%m"))
  ref_paradas <- ciclo$paradas[, .(line_id = linha, gtfs_versao = ciclo$gtfs_versao, direction_id, stop_id,
                                   stop_sequence, stop_name, stop_lat, stop_lon, s_ciclo, dist_tracado_m)]
  message(sprintf("    %s: %d registros, %d viagens, %d passagens, %d saídas programadas (%.1f s)",
                  l, nrow(dt), nrow(tr), nrow(pas), nrow(aud$auditoria),
                  as.numeric(Sys.time() - t0, units = "secs")))
  list(linha = l, qual = qual, gtfs = esc$diag, sl = dsl, vd = vd, ih = ih, vh = vh,
       sem_assoc = aud$sem_assoc, sens = sens, paradas = ref_paradas,
       sentidos = ciclo$sentidos[, .(line_id = l, direction_id, a_m = a, b_m = b, headsign, trip_id,
                                     shape_id, L_m = ciclo$L, circular = ciclo$circular)],
       viagens_resumo = tr[, .(line_id, service_date, direction_id, trajectory_status, start_num, end_num,
                               vehicle_id, duration_minutes, origem_ok, destino_ok)],
       auditoria = aud$auditoria, icv = icv)
}

message("[3] Processando linhas (", cfg$n_cores, " núcleos)")
res <- if (cfg$n_cores > 1 && .Platform$OS.type == "unix") {
  parallel::mclapply(linhas, function(l) tryCatch(processar_linha(l), error = function(e) list(linha = l, erro = conditionMessage(e))),
                     mc.cores = cfg$n_cores, mc.preschedule = FALSE)
} else lapply(linhas, function(l) tryCatch(processar_linha(l), error = function(e) list(linha = l, erro = conditionMessage(e))))
names(res) <- linhas
erros <- Filter(function(r) !is.null(r$erro), res)
if (length(erros)) for (e in erros) warning("Linha ", e$linha, ": ", e$erro)

# ------------------------------------------------------------- consolidação
message("[4] Consolidação, verificações e exportação")
exportar_consolidados(res, inv)
message(sprintf("Pipeline concluído em %.1f min", as.numeric(Sys.time() - t_inicio, units = "mins")))
