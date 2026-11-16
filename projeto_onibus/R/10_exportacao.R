############################################################
# 10_exportacao.R — gravação das tabelas, verificações e gráficos
############################################################

# Grava as tabelas detalhadas de uma linha em datasets particionados
# <saida>/<tabela>/mes=AAAA-MM/linha=<linha>/part-0.parquet (sobrescreve)
gravar_linha <- function(linha, tabelas, out = cfg$saida_dir) {
  for (nm in names(tabelas)) {
    d <- tabelas[[nm]]
    if (is.null(d) || !nrow(d)) next
    meses <- format(d$service_date, "%Y-%m")
    for (m in unique(meses)) {
      dd <- file.path(out, nm, paste0("mes=", m), paste0("linha=", linha))
      dir.create(dd, recursive = TRUE, showWarnings = FALSE)
      dm <- d[meses == m]
      setattr(dm, "sorted", NULL); setattr(dm, "index", NULL)   # não grava chave que a reordenação invalidaria
      # ordem (dia, veículo, instante) + row groups pequenos: leitores (arrow/duckdb)
      # pulam os blocos de outros dias ao filtrar por service_date
      ord <- intersect(c("service_date", "vehicle_id", "timestamp_utc"), names(dm))
      if (length(ord)) setorderv(dm, ord)
      arrow::write_parquet(dm, file.path(dd, "part-0.parquet"),
                           chunk_size = if (nrow(dm) > 2e5) 5e4 else NULL)
    }
  }
}

# Traçado (vértices do ciclo) em lat/lon, para mapas e para o app de auditoria.
# <saida>/referencia_tracados/mes=AAAA-MM/linha=<linha>/part-0.parquet
gravar_tracado <- function(linha, ciclo, meses, out = cfg$saida_dir) {
  v <- ciclo$vert
  ll <- sf::st_coordinates(sf::st_transform(sf::st_as_sf(v, coords = c("x", "y"), crs = cfg$crs_metrico), 4326))
  dir_v <- v$direction_id
  dir_v[dir_v < 0] <- dir_v[length(dir_v) - 1L]   # vértice de fechamento pertence ao último sentido
  tb <- data.table(line_id = linha, gtfs_versao = ciclo$gtfs_versao, circular = ciclo$circular,
                   direction_id = dir_v, seq = seq_len(nrow(v)), s_ciclo = v$cum,
                   lon = ll[, 1], lat = ll[, 2])
  for (m in unique(meses)) {
    dd <- file.path(out, "referencia_tracados", paste0("mes=", m), paste0("linha=", linha))
    dir.create(dd, recursive = TRUE, showWarnings = FALSE)
    arrow::write_parquet(tb, file.path(dd, "part-0.parquet"))
    pr <- ciclo$paradas[, .(line_id = linha, gtfs_versao = ciclo$gtfs_versao, direction_id, stop_id,
                            stop_sequence, stop_name, stop_lat, stop_lon, s_ciclo)]
    dp <- file.path(out, "referencia_paradas", paste0("mes=", m), paste0("linha=", linha))
    dir.create(dp, recursive = TRUE, showWarnings = FALSE)
    arrow::write_parquet(pr, file.path(dp, "part-0.parquet"))
  }
}

.achatar_cfg <- function(x, prefix = "") {
  rbindlist(lapply(names(x), function(n) {
    v <- x[[n]]; key <- if (nzchar(prefix)) paste0(prefix, ".", n) else n
    if (is.list(v)) .achatar_cfg(v, key)
    else data.table(parametro = key, valor = paste(format(v), collapse = ", "))
  }))
}

verificacoes <- function(vg, au) {
  chk <- function(nome, ok, det = "") data.table(verificacao = nome, resultado = if (isTRUE(ok)) "OK" else "FALHA", detalhe = det)
  rbindlist(list(
    chk("viagens: inicio <= fim", all(vg$start_num <= vg$end_num, na.rm = TRUE),
        sprintf("%d violações", sum(vg$start_num > vg$end_num, na.rm = TRUE))),
    chk("viagens: duracao coerente com timestamps",
        all(abs(vg$duration_minutes - (vg$end_num - vg$start_num) / 60) < 1e-6, na.rm = TRUE)),
    chk("viagens: distancias nao negativas", all(vg$distance_km >= 0, na.rm = TRUE)),
    chk("viagens: trip_id unico", !anyDuplicated(vg$trip_id)),
    chk("viagens: parciais nao contadas como completas",
        !any(vg$trajectory_status %in% c("viagem_confirmada", "viagem_provavel") & (!vg$origem_ok | !vg$destino_ok))),
    chk("auditoria: scheduled_trip_id unico", !anyDuplicated(au$scheduled_trip_id)),
    chk("auditoria: uma classificacao por saida", !anyNA(au$audit_status)),
    chk("auditoria: viagem associada a no maximo uma saida",
        !anyDuplicated(na.omit(au$observed_trip_id)),
        sprintf("%d duplicadas", sum(duplicated(na.omit(au$observed_trip_id))))),
    chk("auditoria: associacoes apenas em programacao valida",
        !any(au$audit_status %in% c("programacao_invalida", "fora_do_periodo_coletado") & !is.na(au$observed_trip_id)))
  ))
}

exportar_consolidados <- function(res, inv, out = cfg$saida_dir) {
  ok <- Filter(function(r) is.null(r$erro), res)
  pega <- function(k) rbindlist(lapply(ok, `[[`, k), fill = TRUE)
  fwrite(rbindlist(lapply(res, `[[`, "qual"), fill = TRUE), file.path(out, "qualidade_dados.csv"))
  fwrite(rbindlist(lapply(res, `[[`, "gtfs"), fill = TRUE), file.path(out, "gtfs_escolha_por_linha.csv"))
  fwrite(data.table(linha = names(res), erro = vapply(res, function(r) r$erro %||% "", "")),
         file.path(out, "linhas_processadas.csv"))
  fwrite(pega("sl"), file.path(out, "diagnostico_sentido_sl.csv"))
  fwrite(pega("paradas"), file.path(out, "referencia_paradas.csv"))
  fwrite(pega("sentidos"), file.path(out, "referencia_sentidos.csv"))
  fwrite(pega("vd"), file.path(out, "veiculos_dia.csv"))
  ih <- pega("ih"); ih[, tipo_dia := tipo_dia(service_date)]
  fwrite(ih, file.path(out, "indicadores_horarios.csv"))
  ihm <- ih[, lapply(.SD, mean), by = .(line_id, tipo_dia, hour_local),
            .SDcols = c("vehicles_observed", "vehicles_with_movement", "vehicles_with_confirmed",
                        "vehicles_with_probable", "vehicles_inconclusive", "active_vehicles_est",
                        "trips_started", "trips_completed", "trips_partial", "observation_coverage")]
  fwrite(ihm, file.path(out, "indicadores_horarios_media_por_tipo_dia.csv"))
  vh <- pega("vh")
  fwrite(vh[, .N, by = .(line_id, service_date, hour_local, classificacao)][order(line_id, service_date, hour_local)],
         file.path(out, "veiculos_por_hora.csv"))

  vg <- pega("viagens_resumo")
  au <- pega("auditoria")
  if (isTRUE(cfg$gravar_arquivos_unicos)) {
    vfull <- arrow::open_dataset(file.path(out, "viagens_reconstruidas")) |>
      dplyr::filter(line_id %in% names(ok)) |> dplyr::collect()
    arrow::write_parquet(vfull, file.path(out, "viagens_reconstruidas.parquet"))
    arrow::write_parquet(au, file.path(out, "auditoria_saidas.parquet"))
  }
  # ---- auditoria: indicadores
  au[, hora_prog := as.integer(substr(scheduled_departure_local, 12, 13))]
  au[, mes := format(service_date, "%Y-%m")]
  fwrite(indicadores_ipp(au, c("line_id", "service_date", "tipo_dia", "direction_id")),
         file.path(out, "ipp_diario_por_sentido.csv"))
  fwrite(indicadores_ipp(au, c("line_id", "service_date", "tipo_dia")), file.path(out, "ipp_diario.csv"))
  rk <- indicadores_ipp(au, c("line_id", "mes"))
  nviag <- vg[, .(viagens_reconstruidas = .N,
                  viagens_completas = sum(trajectory_status %in% c("viagem_confirmada", "viagem_provavel"))),
              by = .(line_id, mes = format(service_date, "%Y-%m"))]
  rk <- nviag[rk, on = .(line_id, mes)]
  fwrite(rk[order(-possivelmente_nao_realizadas)], file.path(out, "ranking_linhas.csv"))
  fwrite(indicadores_ipp(au, c("line_id", "mes", "direction_id")), file.path(out, "ipp_mensal_por_sentido.csv"))
  fwrite(indicadores_ipp(au, c("line_id", "tipo_dia", "hora_prog")), file.path(out, "ranking_linhas_horario.csv"))
  fwrite(indicadores_ipp(au, "mes"), file.path(out, "indicadores_gerais.csv"))
  cols_s <- c("scheduled_trip_id", "service_date", "line_id", "direction_id", "scheduled_departure_local",
              "audit_status", "confidence_level", "data_coverage_status", "frac_min_dados", "evidence_summary")
  fwrite(au[audit_status == "possivelmente_nao_realizada", ..cols_s], file.path(out, "saidas_suspeitas.csv"))
  fwrite(au[audit_status %in% c("inconclusiva", "partida_sem_viagem_completa", "viagem_quase_completa"), ..cols_s],
         file.path(out, "saidas_inconclusivas.csv"))
  fwrite(pega("sem_assoc"), file.path(out, "viagens_sem_associacao.csv"))
  # ICV mensal por linha (R/13_icv.R)
  fx <- pega("icv")
  if (nrow(fx)) {
    fx[, mes := format(service_date, "%Y-%m")]
    im <- fx[, {r <- icv_resumo(.SD); .(ICV_pct = round(100 * r$mes, 2), ICV_ampliado_pct = round(100 * r$mes_amp, 2),
                                         programadas = sum(programadas), realizadas_lim = sum(realizadas_lim))},
             by = .(line_id, mes)]
    fwrite(im, file.path(out, "icv_mensal.csv"))
  }
  sens <- pega("sens")
  if (nrow(sens)) fwrite(sens, file.path(out, "validacao_sensibilidade.csv"))

  # ---- parâmetros, versões, verificações
  fwrite(.achatar_cfg(cfg), file.path(out, "parametros_execucao.csv"))
  pk <- c("data.table", "arrow", "sf", "lubridate", "ggplot2", "Rcpp", "dplyr")
  fwrite(rbind(data.table(pacote = "R", versao = R.version.string),
               data.table(pacote = pk, versao = vapply(pk, function(x) as.character(packageVersion(x)), ""))),
         file.path(out, "versoes_pacotes.csv"))
  vf <- verificacoes(vg, au)
  pc <- unlist(rk[, .(IPP_pct, realizadas_pct, nao_realizadas_pct, inconclusivas_pct)])
  vf <- rbind(vf, data.table(verificacao = "auditoria: percentuais entre 0 e 100",
                             resultado = if (all(pc >= 0 & pc <= 100, na.rm = TRUE)) "OK" else "FALHA", detalhe = ""),
              data.table(verificacao = "auditoria: soma das categorias == total programado",
                         resultado = if (all(rk[, no_horario + com_atraso + antecipadas + partida_sem_viagem_completa + quase_completas +
                                                  possivelmente_nao_realizadas + inconclusivas == programadas_validas])) "OK" else "FALHA",
                         detalhe = ""))
  qual <- rbindlist(lapply(res, `[[`, "qual"), fill = TRUE)
  tot_inv <- sum(inv$registros); tot_q <- qual[indicador == "registros_total", sum(n)]
  vf <- rbind(vf, data.table(verificacao = "registros: inventario == soma por linha",
                             resultado = if (!is.null(cfg$linhas)) "N/A (subconjunto de linhas)" else if (tot_inv == tot_q) "OK" else "FALHA",
                             detalhe = sprintf("inventario %d, linhas %d", tot_inv, tot_q)))
  fwrite(vf, file.path(out, "verificacoes_consistencia.csv"))
  print(vf)

  # ---- gráficos
  gdir <- file.path(out, "graficos"); dir.create(gdir, showWarnings = FALSE)
  long <- melt(ihm[tipo_dia == "U"], id.vars = c("line_id", "tipo_dia", "hour_local"),
               measure.vars = c("vehicles_observed", "vehicles_with_movement", "vehicles_with_confirmed", "active_vehicles_est"))
  long[, variable := factor(variable, labels = c("observados", "em movimento", "com viagem confirmada", "ativos em rota (média 5 min)"))]
  g <- ggplot(long, aes(hour_local, value, colour = variable)) + geom_line(linewidth = .8) +
    facet_wrap(~line_id, scales = "free_y") + scale_x_continuous(breaks = seq(0, 23, 3)) +
    labs(x = "hora local", y = "veículos (média dos dias úteis)", colour = NULL,
         title = "Frota por hora — dias úteis") + theme_minimal() + theme(legend.position = "bottom")
  ggsave(file.path(gdir, "frota_por_hora_dias_uteis.png"), g, width = 10, height = 4.5, dpi = 120)
  ipd <- indicadores_ipp(au[!audit_status %in% c("programacao_invalida", "fora_do_periodo_coletado")], c("line_id", "service_date"))[programadas_validas >= 10]
  g <- ggplot(ipd, aes(service_date, IPP_pct)) + geom_col(fill = "#3b6ea5") + geom_hline(yintercept = 97, linetype = 2) +
    facet_wrap(~line_id, ncol = 1) + labs(x = NULL, y = "IPP (%)", title = "IPP diário (tolerância ±3 min; viagem completa exigida)",
                                          subtitle = "Linha tracejada: referência SPTrans de 97%") + theme_minimal()
  ggsave(file.path(gdir, "ipp_diario.png"), g, width = 10, height = 5, dpi = 120)
  d <- au[!is.na(departure_difference_minutes) & !audit_status %in% c("partida_sem_viagem_completa", "viagem_quase_completa")]
  g <- ggplot(d, aes(departure_difference_minutes)) + geom_histogram(binwidth = 1, fill = "#3b6ea5") +
    annotate("rect", xmin = -3, xmax = 3, ymin = 0, ymax = Inf, alpha = .12, fill = "darkgreen") +
    facet_wrap(~line_id, scales = "free_y") + coord_cartesian(xlim = c(-15, 40)) +
    labs(x = "partida observada − programada (min)", y = "saídas", title = "Desvio das partidas associadas") + theme_minimal()
  ggsave(file.path(gdir, "desvio_partidas.png"), g, width = 10, height = 4, dpi = 120)
  invisible(vf)
}
