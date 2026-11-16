############################################################
# 07_auditoria.R — associação programada x observada e IPP
#
# Associação global por linha x sentido x dia de serviço: alinhamento
# ordenado (programação dinâmica, C++) entre partidas programadas e
# partidas observadas, de modo que uma viagem real justifica no máximo
# uma saída programada e a ordem temporal é preservada. Uma saída
# "pulada" (sem ônibus) fica sem par em vez de deslocar as demais.
#
# IPP (SPTrans IQT.GSO.002.03): partidas pontuais (|dif| <= 3 min) /
# partidas programadas x 100. Aqui a partida só conta como realizada se a
# viagem reconstruída for confirmada ou provável (percurso cumprido).
############################################################

# Fração de minutos com dado GPS em rota da linha, por janela
cobertura_linha <- function(obs) {
  m <- floor(obs$tnum / 60)
  cnt <- data.table(m, veh = obs$vehicle_id)[, .(nveh = uniqueN(veh)), by = m]
  rng <- range(m)
  full <- data.table(m = seq(rng[1], rng[2]))
  full <- cnt[full, on = "m"][is.na(nveh), nveh := 0L]
  full[, cum := cumsum(nveh > 0)]
  full
}

frac_minutos_com_dados <- function(cobl, t_ini, t_fim) {
  a <- floor(t_ini / 60); b <- floor(t_fim / 60)
  m0 <- cobl$m[1]; mN <- cobl$m[nrow(cobl)]
  ca <- cobl$cum[pmin(pmax(a - m0 + 1, 1), nrow(cobl))]
  cb <- cobl$cum[pmin(pmax(b - m0 + 1, 1), nrow(cobl))]
  out <- (cb - ca) / pmax(1, b - a)
  out[b < m0 | a > mN] <- 0
  out
}

# Lacunas de transmissão de veículos EM SERVIÇO: intervalo entre dois
# registros consecutivos, ambos na rota, com dt > lacuna_min (e < 3 h).
# Uma saída programada sem par que cai dentro de uma dessas lacunas pode ter
# sido realizada sem ser observada -> inconclusiva, não "não realizada".
#
# Também entram as lacunas "abertas": o último registro do veículo no dia de
# serviço estava no itinerário e ele não transmitiu mais. Pode ter seguido
# operando sem GPS; a lacuna vai do último sinal até janela_veiculo_sumiu_h depois.
lacunas_em_servico <- function(v, lacuna_min_s = cfg$ipp$lacuna_veiculo_s,
                               janela_h = cfg$ipp$janela_veiculo_sumiu_h) {
  r <- v[(valido), .(vehicle_id, service_date, tnum, na_rota)]
  setorder(r, vehicle_id, tnum)
  r[, `:=`(t_ant = shift(tnum), rota_ant = shift(na_rota)), by = vehicle_id]
  fech <- r[!is.na(t_ant) & na_rota & rota_ant & (tnum - t_ant) > lacuna_min_s & (tnum - t_ant) < 3 * 3600,
            .(vehicle_id, g_ini = t_ant, g_fim = tnum, aberta = FALSE)]
  ult <- r[, .SD[.N], by = .(vehicle_id, service_date)][na_rota == TRUE]
  abt <- ult[, .(vehicle_id, g_ini = tnum, g_fim = tnum + janela_h * 3600, aberta = TRUE)]
  # o veículo voltou a transmitir (no dia seguinte, por ex.) antes do fim da janela: corta
  prox <- r[, .(vehicle_id, tnum)]
  if (nrow(abt)) {
    nx <- prox[abt, on = .(vehicle_id, tnum > g_ini), mult = "first", .(vehicle_id, g_ini = i.g_ini, t_prox = x.tnum)]
    abt[nx, t_prox := i.t_prox, on = .(vehicle_id, g_ini)]
    abt[!is.na(t_prox), g_fim := pmin(g_fim, t_prox)]
    abt[, t_prox := NULL]
  }
  rbind(fech, abt)
}

auditar_linha <- function(prog, viagens, obs, linha, p = cfg$ipp, lacunas = NULL, periodo_coleta = NULL) {
  pr <- prog[line_id == linha & !is.na(scheduled_trip_id)]
  if (!nrow(pr)) return(list(auditoria = data.table(), sem_assoc = data.table()))
  # partidas elegíveis: qualquer viagem candidata com partida evidenciada no
  # terminal (mesmo que depois o GPS falhe: a partida é evidência real)
  elig_status <- c("viagem_confirmada", "viagem_provavel", "trajetoria_parcial", "observacao_inconclusiva")
  ob <- if (nrow(viagens)) viagens[origem_ok == TRUE & trajectory_status %in% elig_status & !is.na(t_partida)] else data.table()
  # partida espúria: viagem incompleta que cobre só um trecho mínimo do traçado (o GPS do
  # ônibus parado no terminal é associado a um viário próximo e "volta"). Não é evidência
  # de partida: sai da associação e fica registrada para a saída que ela ocuparia.
  esp <- data.table()
  if (nrow(ob)) {
    ob[, espuria := !trajectory_status %in% c("viagem_confirmada", "viagem_provavel") &
         frac_observada < p$parcial_espuria_max]
    esp <- ob[espuria == TRUE]
    ob <- ob[espuria == FALSE]
  }
  pr[, `:=`(observed_trip_id = NA_character_, s_min = as.numeric(scheduled_departure_utc) / 60)]
  # saídas fora do período coberto pela coleta não são avaliadas
  pr[, fora_coleta := FALSE]
  if (!is.null(periodo_coleta))
    pr[, fora_coleta := s_min * 60 < periodo_coleta[1] | s_min * 60 > periodo_coleta[2]]
  pr[fora_coleta == TRUE, schedule_valid := FALSE]
  if (nrow(ob)) {
    ob[, o_min := t_partida / 60]
    setorder(ob, service_date, direction_id, o_min)
  }
  grupos <- unique(pr[schedule_valid == TRUE, .(service_date, direction_id)])
  for (g in seq_len(nrow(grupos))) {
    sd <- grupos$service_date[g]; di <- grupos$direction_id[g]
    ip <- pr[, which(schedule_valid & service_date == sd & direction_id == di)]
    if (!nrow(ob)) next
    io <- ob[, which(service_date == sd & direction_id == di)]
    if (!length(io)) next
    m <- align_departures(pr$s_min[ip], ob$o_min[io], p$janela_antecipacao_min, p$janela_atraso_min,
                          p$custo_nao_assoc_prog, p$custo_nao_assoc_obs)
    set(pr, ip[!is.na(m)], "observed_trip_id", ob$trip_id[io[m[!is.na(m)]]])
  }
  # atributos da viagem associada
  vcols <- c("trip_id", "vehicle_id", "t_partida", "trajectory_status", "confidence_level",
             "route_match_status", "observation_coverage", "ev_partida", "quality_flags", "t_chegada",
             "frac_observada")
  au <- if (nrow(ob)) ob[, ..vcols][pr, on = c(trip_id = "observed_trip_id")] else
    cbind(pr, as.data.table(setNames(rep(list(NA), length(vcols)), vcols)))
  if (nrow(ob)) setnames(au, "trip_id", "observed_trip_id")
  au[, observed_departure_utc := as.POSIXct(t_partida, origin = "1970-01-01", tz = "UTC")]
  au[, observed_departure_local := format(observed_departure_utc, "%Y-%m-%d %H:%M:%S", tz = cfg$tz_local)]
  au[, departure_difference_minutes := round(t_partida / 60 - s_min, 2)]

  # detectabilidade: a linha tinha GPS em rota na janela?
  if (nrow(obs)) {
    cobl <- cobertura_linha(obs)
    au[, frac_min_dados := frac_minutos_com_dados(cobl, s_min * 60 - p$janela_detect_antes_min * 60,
                                                  s_min * 60 + p$janela_detect_depois_min * 60)]
  } else au[, frac_min_dados := 0]
  au[, data_coverage_status := fcase(frac_min_dados >= 0.8, "boa", frac_min_dados >= 0.5, "parcial",
                                     default = "insuficiente")]
  # lacuna de transmissão de algum veículo em serviço cobrindo o horário
  au[, `:=`(veic_lacuna = NA_character_, desc_lacuna = NA_character_, so_aberta = NA)]
  if (!is.null(lacunas) && nrow(lacunas)) {
    if (!"aberta" %in% names(lacunas)) lacunas[, aberta := FALSE]
    q <- au[is.na(observed_trip_id), .(scheduled_trip_id, t = s_min * 60)]
    if (nrow(q)) {
      hit <- lacunas[q, on = .(g_ini <= t, g_fim >= t), nomatch = 0,
                     .(scheduled_trip_id = i.scheduled_trip_id, vehicle_id = x.vehicle_id,
                       aberta = x.aberta, g_ini = x.g_ini)]
      hit[, txt := fifelse(aberta, sprintf("%s (parou de transmitir às %s e não voltou)", vehicle_id,
                                           format(as.POSIXct(g_ini, origin = "1970-01-01", tz = "UTC"), "%H:%M", tz = cfg$tz_local)),
                           vehicle_id)]
      hit <- hit[, .(veic_lacuna = paste(unique(vehicle_id), collapse = ","),
                     desc_lacuna = paste(unique(txt), collapse = ", "), so_aberta = all(aberta)), by = scheduled_trip_id]
      au[hit, `:=`(veic_lacuna = i.veic_lacuna, desc_lacuna = i.desc_lacuna, so_aberta = i.so_aberta),
         on = "scheduled_trip_id"]
    }
  }
  au[!is.na(veic_lacuna) & data_coverage_status != "insuficiente", data_coverage_status := "lacuna_gps_veiculo"]
  # partida espúria descartada que ocuparia esta saída (mesmo dia/sentido, dentro da janela)
  au[, partida_descartada := NA_character_]
  if (nrow(esp)) {
    q <- au[is.na(observed_trip_id), .(scheduled_trip_id, service_date, direction_id,
                                       lo = s_min * 60 - p$janela_antecipacao_min * 60,
                                       hi = s_min * 60 + p$janela_atraso_min * 60)]
    hx <- esp[q, on = .(service_date, direction_id, t_partida >= lo, t_partida <= hi), nomatch = 0,
              .(scheduled_trip_id = i.scheduled_trip_id, vehicle_id = x.vehicle_id, tp = x.t_partida,
                fo = x.frac_observada, km = x.distance_km)]
    hx <- hx[, .(partida_descartada = paste(sprintf("partida espúria descartada: %s às %s percorreu %.0f%% do traçado (%.1f km)",
                                                    vehicle_id, format(as.POSIXct(tp, origin = "1970-01-01", tz = "UTC"),
                                                                       "%H:%M:%S", tz = cfg$tz_local), 100 * fo, km),
                                            collapse = "; ")), by = scheduled_trip_id]
    au[hx, partida_descartada := i.partida_descartada, on = "scheduled_trip_id"]
  }
  tol <- p$tolerancia_min
  completa <- quote(trajectory_status %in% c("viagem_confirmada", "viagem_provavel"))
  quase <- quote(frac_observada >= p$quase_completa_min & observation_coverage >= p$quase_completa_cob_min)
  au[, audit_status := fcase(
    fora_coleta, "fora_do_periodo_coletado",
    !schedule_valid, "programacao_invalida",
    !is.na(observed_trip_id) & !eval(completa) & eval(quase), "viagem_quase_completa",
    !is.na(observed_trip_id) & !eval(completa), "partida_sem_viagem_completa",
    !is.na(observed_trip_id) & abs(departure_difference_minutes) <= tol, "realizada_no_horario",
    !is.na(observed_trip_id) & departure_difference_minutes > tol, "realizada_com_atraso",
    !is.na(observed_trip_id) & departure_difference_minutes < -tol, "realizada_antecipadamente",
    is.na(observed_trip_id) & data_coverage_status %in% c("boa", "parcial"), "possivelmente_nao_realizada",
    default = "inconclusiva")]
  au[, audit_confidence := fcase(
    audit_status %in% c("programacao_invalida", "fora_do_periodo_coletado"), NA_character_,
    audit_status %in% c("realizada_no_horario", "realizada_com_atraso", "realizada_antecipadamente") &
      trajectory_status == "viagem_confirmada" & ev_partida == "observado", "alta",
    audit_status %in% c("realizada_no_horario", "realizada_com_atraso", "realizada_antecipadamente"), "media",
    audit_status == "possivelmente_nao_realizada" & data_coverage_status == "boa", "media",
    default = "baixa")]
  au[, evidence_summary := fcase(
    audit_status == "fora_do_periodo_coletado", "horario fora do periodo coberto pelos arquivos de GPS",
    audit_status == "programacao_invalida", paste0("tabela invalida: ", schedule_quality_flag),
    is.na(observed_trip_id) & !is.na(veic_lacuna), sprintf("sem partida observada; veiculo(s) %s em servico sem transmitir no horario", desc_lacuna),
    !is.na(observed_trip_id), sprintf("viagem %s (%s, %.0f%% do traçado, cobertura %.0f%%, partida %s), dif %+.1f min",
                                      observed_trip_id, trajectory_status, 100 * frac_observada, 100 * observation_coverage,
                                      ev_partida, departure_difference_minutes),
    default = sprintf("sem partida observada compativel; %.0f%% dos minutos da janela com GPS da linha",
                      100 * frac_min_dados))]
  au[!is.na(partida_descartada), evidence_summary := paste0(evidence_summary, "; ", partida_descartada)]
  au[, `:=`(observed_vehicle_id = vehicle_id, origin_terminal_id = paste0(linha, "_sentido", direction_id),
            audit_rule_version = cfg$audit_rule_version, tolerancia_min = tol)]
  out <- au[, .(scheduled_trip_id, service_date, line_id, direction_id, tipo_dia, origin_terminal_id,
                scheduled_departure_local, observed_departure_local, departure_difference_minutes,
                observed_trip_id, observed_vehicle_id, audit_status, confidence_level = audit_confidence,
                trip_status = trajectory_status, data_coverage_status, frac_min_dados,
                route_match_status, evidence_summary, schedule_quality_flag, schedule_source,
                pct_tracado = round(100 * frac_observada, 1), pct_cobertura = round(100 * observation_coverage, 1),
                veiculos_sem_gps = veic_lacuna, so_lacuna_aberta = so_aberta, partida_descartada,
                audit_rule_version, tolerancia_min, scheduled_departure_utc, observed_departure_utc)]
  # viagens observadas sem saída programada associada
  sem <- if (nrow(viagens)) viagens[!trip_id %in% out$observed_trip_id &
                                      trajectory_status %in% elig_status] else data.table()
  if (nrow(sem)) {
    dias_val <- unique(pr[schedule_valid == TRUE, service_date])
    sem[, motivo := fcase(trip_id %in% esp$trip_id, "partida_espuria_descartada",
                          !service_date %in% dias_val, "dia_sem_programacao_valida",
                          origem_ok == FALSE, "sem_evidencia_de_partida_no_terminal",
                          default = "sem_saida_programada_compativel")]
    sem <- sem[, .(trip_id, line_id, vehicle_id, direction_id, service_date, start_time_local,
                   end_time_local, trajectory_status, origin_evidence, motivo)]
  }
  list(auditoria = out, sem_assoc = sem)
}

# IPP e variantes (ver relatório):
#  IPP_pct   (a) oficial: partidas pontuais (±3 min) com viagem completa / programadas válidas
#  IPP_b_pct (b) + partidas pontuais (±3 min) que saíram e não completaram (partidas espúrias
#                já foram descartadas na associação) + inconclusivas (benefício da dúvida)
#  IPP_c_pct (c) como (b), com tolerância ampliada (±4 min)
indicadores_ipp <- function(au, by, tol_b = cfg$ipp$tolerancia_min, tol_c = cfg$ipp$tolerancia_ampliada_min) {
  saiu <- c("realizada_no_horario", "realizada_com_atraso", "realizada_antecipadamente",
            "partida_sem_viagem_completa", "viagem_quase_completa")
  au[, .(programadas_total = .N,
         programadas_validas = sum(!audit_status %in% c("programacao_invalida", "fora_do_periodo_coletado")),
         no_horario = sum(audit_status == "realizada_no_horario"),
         com_atraso = sum(audit_status == "realizada_com_atraso"),
         antecipadas = sum(audit_status == "realizada_antecipadamente"),
         quase_completas = sum(audit_status == "viagem_quase_completa"),
         partida_sem_viagem_completa = sum(audit_status == "partida_sem_viagem_completa"),
         possivelmente_nao_realizadas = sum(audit_status == "possivelmente_nao_realizada"),
         inconclusivas = sum(audit_status == "inconclusiva"),
         pontuais_b = sum(audit_status %in% saiu & abs(departure_difference_minutes) <= tol_b, na.rm = TRUE) +
           sum(audit_status == "inconclusiva"),
         pontuais_c = sum(audit_status %in% saiu & abs(departure_difference_minutes) <= tol_c, na.rm = TRUE) +
           sum(audit_status == "inconclusiva")), by = by][
    , `:=`(IPP_pct = round(100 * no_horario / programadas_validas, 2),
           IPP_b_pct = round(100 * pontuais_b / programadas_validas, 2),
           IPP_c_pct = round(100 * pontuais_c / programadas_validas, 2),
           realizadas_pct = round(100 * (no_horario + com_atraso + antecipadas) / programadas_validas, 2),
           nao_realizadas_pct = round(100 * possivelmente_nao_realizadas / programadas_validas, 2),
           inconclusivas_pct = round(100 * inconclusivas / programadas_validas, 2))][]
}
