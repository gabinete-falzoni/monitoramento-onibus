############################################################
# 12_frota.R — lacunas de GPS por veículo e diagnóstico de frota
#
# Objetivo: para cada saída inconclusiva, não realizada ou sem viagem
# completa, responder "algum ônibus da linha poderia tê-la feito sem ser
# visto?". Este módulo acrescenta
# evidência para a checagem manual (ou externa, via bilhetagem) e, em
# reconciliar_status(), ajusta não realizada ↔ inconclusiva pela viabilidade.
#
# Modelo (deliberadamente otimista, para que "nenhum veículo poderia" seja
# uma afirmação forte):
#   - cada lacuna de transmissão de um veículo em serviço é um intervalo
#     [t0, t1] em que ele sumiu na posição s0 do ciclo e reapareceu em s1;
#   - o ônibus anda ao longo do ciclo com a velocidade de uma viagem rápida
#     do dia (quantil `q_duracao` das durações das viagens completas), pode
#     esperar no terminal e não precisa de tempo de descanso;
#   - a saída S (sentido d, horário h) é "possível" para o veículo se ele
#     consegue chegar à origem de d até h + janela de atraso, partir não
#     antes de h − janela de antecipação, cumprir o percurso até o destino e
#     ainda chegar a s1 até t1 + folga.
############################################################

.fwd <- function(de, ate, L) (ate - de) %% L          # distância andando para a frente no ciclo

# Lacunas > lacuna_min_s entre registros válidos e no itinerário de um mesmo veículo.
# Lacuna "aberta": o último registro válido do veículo no dia estava no itinerário e ele
# não transmitiu mais; vai de t0 até janela_veiculo_sumiu_h depois (ou até o próximo sinal).
lacunas_veiculos <- function(pos, L, p = cfg$frota, janela_h = cfg$ipp$janela_veiculo_sumiu_h) {
  r <- pos[valido == TRUE & quebra != 4L & !is.na(s) & na_rota %in% TRUE,
           .(vehicle_id, service_date, t = as.numeric(timestamp_utc), s)]
  setorder(r, vehicle_id, t)
  r[, `:=`(t_ant = shift(t), s_ant = shift(s), d_ant = shift(service_date)), by = vehicle_id]
  g <- r[!is.na(t_ant) & (t - t_ant) > p$lacuna_min_s & (t - t_ant) < p$lacuna_max_h * 3600 & d_ant == service_date,
         .(vehicle_id, service_date, t0 = t_ant, t1 = t, s0 = s_ant, s1 = s, aberta = FALSE)]
  # último registro válido de cada veículo × dia (no itinerário ou não)
  todos <- pos[valido == TRUE, .(vehicle_id, service_date, t = as.numeric(timestamp_utc),
                                 rota = na_rota %in% TRUE & !is.na(s) & quebra != 4L, s)]
  setorder(todos, vehicle_id, t)
  todos[, t_prox := shift(t, -1), by = vehicle_id]
  ult <- todos[, .SD[.N], by = .(vehicle_id, service_date)][rota == TRUE]
  if (nrow(ult)) {
    ab <- ult[, .(vehicle_id, service_date, t0 = t, t1 = pmin(t + janela_h * 3600, fifelse(is.na(t_prox), Inf, t_prox)),
                  s0 = s, s1 = NA_real_, aberta = TRUE)]
    g <- rbind(g, ab)
  }
  if (!nrow(g)) return(g)
  g[, dur_min := (t1 - t0) / 60]
  g[, avanco_min_km := fifelse(aberta, 0, .fwd(s0, s1, L) / 1000)]
  g[aberta == FALSE & avanco_min_km * 1000 > L - p$tol_pos_m, avanco_min_km := 0]   # reapareceu praticamente no mesmo ponto
  setorder(g, vehicle_id, t0)
  g[, gid := .I][]
}

# Velocidade "rápida" ao longo do ciclo (m/s) por dia de serviço; fallback = mês
velocidade_ciclo <- function(vg, L, p = cfg$frota) {
  ok <- vg[trajectory_status %in% c("viagem_confirmada", "viagem_provavel") & is.finite(duration_minutes) &
             duration_minutes > 5]
  if (!nrow(ok)) return(data.table(service_date = as.Date(character()), v_ms = numeric()))
  ok[, dist_m := (b - a)]
  ok[, vel := dist_m / (duration_minutes * 60)]
  geral <- quantile(ok$vel, 1 - p$q_duracao, na.rm = TRUE)
  out <- ok[, .(v_ms = if (.N >= 5) quantile(vel, 1 - p$q_duracao, na.rm = TRUE) else geral), by = service_date]
  attr(out, "geral") <- geral
  out
}

# Diagnóstico de frota por saída programada. Retorna list(saidas, lacunas, veiculos)
diagnosticar_frota <- function(pos, vg, au, sentidos, L, linha, p = cfg$frota) {
  vazio <- list(saidas = data.table(), lacunas = data.table(), veiculos = data.table())
  if (!nrow(au) || !nrow(pos)) return(vazio)
  sen <- sentidos[, .(direction_id, a_d = a, b_d = b)]
  g <- lacunas_veiculos(pos, L, p)
  vel <- velocidade_ciclo(vg, L, p)
  v_geral <- attr(vel, "geral") %||% (L / 3600)
  tol_ant <- cfg$ipp$janela_antecipacao_min * 60; tol_atr <- cfg$ipp$janela_atraso_min * 60
  if (nrow(g)) {
    g <- vel[g, on = "service_date"][is.na(v_ms), v_ms := v_geral]
    # quantas voltas completas extras cabem na lacuna (além do avanço mínimo) e quantas
    # viagens completas (origem→destino de algum sentido) cabem no percurso s0 → s1
    g[, voltas_max := pmax(0, floor((dur_min * 60 * v_ms - avanco_min_km * 1000) / L + 1e-9))]
    g[, viagens_max := mapply(function(s0, av, n) {
      fim <- s0 + av * 1000 + n * L + p$tol_pos_m
      k <- 0:(n + 1)
      sum(vapply(seq_len(nrow(sen)), function(j) sum(sen$a_d[j] + k * L >= s0 - p$tol_pos_m &
                                                     sen$b_d[j] + k * L <= fim), 0))
    }, s0, avanco_min_km, voltas_max)]
    g[, tipo := fcase(aberta, "parou_de_transmitir",
                      viagens_max == 0 & avanco_min_km * 1000 < p$tol_pos_m, "parado_sem_sinal",
                      viagens_max == 0, "deslocou_sem_sinal",
                      default = "cabe_viagem_sem_sinal")]
  }

  alvo_status <- c("inconclusiva", "possivelmente_nao_realizada", "partida_sem_viagem_completa", "viagem_quase_completa")
  sa <- au[audit_status %in% alvo_status,
           .(scheduled_trip_id, service_date, direction_id, audit_status, observed_trip_id,
             observed_vehicle_id, frac_min_dados, ts = as.numeric(scheduled_departure_utc))]
  sa <- sen[sa, on = "direction_id"]

  # estado da frota no horário: veículos ativos, em viagem observada, em lacuna
  pv <- pos[valido == TRUE, .(vehicle_id, service_date, t = as.numeric(timestamp_utc))]
  ativos <- pv[, .(ini = min(t), fim = max(t)), by = .(vehicle_id, service_date)]
  emv <- vg[!is.na(start_time_utc), .(vehicle_id, service_date, vi = as.numeric(start_time_utc),
                                       vf = as.numeric(end_time_utc))]
  conta <- function(d, t) {
    at <- ativos[service_date == d & ini <= t & fim >= t, vehicle_id]
    ev <- unique(emv[service_date == d & vi <= t & vf >= t, vehicle_id])
    gl <- if (nrow(g)) unique(g[service_date == d & t0 <= t & t1 >= t, vehicle_id]) else character()
    list(n_ativos = length(at), n_em_viagem = length(intersect(at, ev)), n_em_lacuna = length(gl))
  }

  res <- vector("list", nrow(sa))
  for (i in seq_len(nrow(sa))) {
    r <- sa[i]; ts <- r$ts; res_det <- NA_character_
    fr <- conta(r$service_date, ts)
    cand <- if (nrow(g)) g[service_date == r$service_date & t0 <= ts + tol_atr & t1 >= ts - tol_ant] else g
    poss <- character(); no_hor <- character(); poss_id <- character(); gids <- integer()
    if (nrow(cand)) {
      cand[, d1 := .fwd(s0, r$a_d, L)]
      cand[d1 > L - p$tol_pos_m, d1 := 0]
      cand[, t_cheg := t0 + d1 / v_ms]
      cand[, t_par := pmax(t_cheg, ts - tol_ant)]
      cand[, d2 := (r$b_d - r$a_d) + {x <- .fwd(r$b_d, s1, L); x[x > L - p$tol_pos_m | is.na(x)] <- 0; x}]
      # lacuna aberta: basta conseguir partir na janela (não há ponto de reaparecimento a cumprir)
      cand[, ok := t_par <= ts + tol_atr & (aberta | t_par + d2 / v_ms <= t1 + p$folga_min * 60)]
      cand[, ok_hor := ok & t_cheg <= ts + cfg$ipp$tolerancia_min * 60]
      # veículo que parou de transmitir: identificado no texto
      cand[, rot := fifelse(aberta, sprintf("%s (sem sinal desde %s)", vehicle_id,
                                            format(as.POSIXct(t0, origin = "1970-01-01", tz = cfg$tz_local), "%H:%M")),
                            vehicle_id)]
      poss <- cand[ok == TRUE, rot]; no_hor <- cand[ok_hor == TRUE, rot]; poss_id <- cand[ok == TRUE, vehicle_id]
      gids <- cand[ok == TRUE, gid]
    }
    # conclusão da viagem parcial dentro de uma lacuna logo após o último registro
    concl <- NA
    parcial <- r$audit_status %in% c("partida_sem_viagem_completa", "viagem_quase_completa")
    if (parcial && !is.na(r$observed_trip_id) && nrow(g)) {
      tv <- vg[trip_id == r$observed_trip_id]
      gg <- g[vehicle_id == r$observed_vehicle_id & t0 >= as.numeric(tv$end_time_utc) - 120 &
                t0 <= as.numeric(tv$end_time_utc) + 600][order(t0)][1]
      if (nrow(gg) && !is.na(gg$t0)) {
        d_rest <- .fwd(gg$s0, r$b_d, L); if (d_rest > L - p$tol_pos_m) d_rest <- 0
        d_vol <- if (gg$aberta) 0 else .fwd(r$b_d, gg$s1, L); if (d_vol > L - p$tol_pos_m) d_vol <- 0
        concl <- gg$t0 + (d_rest + d_vol) / gg$v_ms <= gg$t1 + p$folga_min * 60
        gids <- c(gids, gg$gid)
        res_det <- if (gg$aberta)
          sprintf("GPS do veículo %s parou no km %.1f às %s e não voltou no dia",
                  r$observed_vehicle_id, gg$s0 / 1000, format(as.POSIXct(gg$t0, origin = "1970-01-01", tz = cfg$tz_local), "%H:%M"))
        else sprintf("GPS do veículo %s parou no km %.1f às %s e voltou %0.f min depois no km %.1f",
                     r$observed_vehicle_id, gg$s0 / 1000,
                     format(as.POSIXct(gg$t0, origin = "1970-01-01", tz = cfg$tz_local), "%H:%M"),
                     gg$dur_min, gg$s1 / 1000)
      }
    }
    diag <- fcase(
      r$audit_status == "viagem_quase_completa", "viagem_quase_completa",
      parcial & isTRUE(concl), "conclusao_compativel_com_lacuna_gps",
      parcial & isFALSE(concl), "lacuna_gps_curta_demais_para_concluir",
      parcial, "viagem_interrompida_sem_lacuna",
      r$frac_min_dados < 0.5, "sem_dados_da_linha",
      length(poss) == 0, "nenhum_veiculo_sem_gps_poderia_fazer",
      length(no_hor) > 0, "possivel_viagem_sem_gps_no_horario",
      default = "possivel_viagem_sem_gps_com_atraso")
    det <- fcase(
      diag %in% c("conclusao_compativel_com_lacuna_gps", "lacuna_gps_curta_demais_para_concluir"), res_det,
      diag == "viagem_quase_completa", "o ônibus percorreu ao menos 95% do traçado; o GPS salta para o destino no fim",
      diag == "viagem_interrompida_sem_lacuna", "o veículo continuou transmitindo, mas não completou o percurso",
      diag == "sem_dados_da_linha", sprintf("só %.0f%% dos minutos da janela tiveram GPS de algum ônibus da linha",
                                            100 * r$frac_min_dados),
      diag == "nenhum_veiculo_sem_gps_poderia_fazer",
      sprintf("%d veículos transmitindo, %d em viagem observada; nenhuma lacuna de GPS comporta esta viagem e nenhum ônibus que parou de transmitir poderia chegar a tempo",
              fr$n_ativos, fr$n_em_viagem),
      default = sprintf("poderia(m) ter feito sem GPS: %s%s", paste(unique(poss), collapse = ", "),
                        if (length(no_hor)) sprintf(" (no horário: %s)", paste(unique(no_hor), collapse = ", ")) else ""))
    res[[i]] <- data.table(scheduled_trip_id = r$scheduled_trip_id, service_date = r$service_date,
                           line_id = linha, audit_status = r$audit_status, diagnostico_frota = diag,
                           detalhe_frota = det, veiculos_possiveis = paste(unique(poss_id), collapse = ","),
                           n_veiculos_ativos = fr$n_ativos, n_em_viagem = fr$n_em_viagem,
                           n_em_lacuna = fr$n_em_lacuna, lacunas_ids = paste(unique(gids), collapse = ","))
  }
  sd <- rbindlist(res)

  # saídas que disputam a mesma lacuna (uma lacuna com k voltas não serve a mais de k saídas)
  if (nrow(g) && nrow(sd)) {
    ln <- sd[nzchar(lacunas_ids), .(gid = as.integer(strsplit(lacunas_ids, ",")[[1]])), by = scheduled_trip_id]
    ln <- merge(ln, sd[, .(scheduled_trip_id, diagnostico_frota)], by = "scheduled_trip_id")
    ln <- ln[grepl("^possivel", diagnostico_frota)]
    cnt <- ln[, .(n_saidas = .N, saidas = paste(sub(".*_(\\d)_(\\d{2})(\\d{2})$", "S\\1 \\2:\\3", scheduled_trip_id),
                                                collapse = " ")), by = gid]
    g <- cnt[g, on = "gid"][is.na(n_saidas), n_saidas := 0L]
    # avisa, na saída, quando a lacuna que a explicaria é disputada por mais saídas do que comporta
    disp <- merge(ln[, .(scheduled_trip_id, gid)], g[, .(gid, n_saidas, viagens_max, vehicle_id)], by = "gid")
    disp <- disp[n_saidas > viagens_max, .(aviso = paste(sprintf("lacuna de %s comporta %d viagem(ns) e é disputada por %d saídas",
                                                                vehicle_id, viagens_max, n_saidas), collapse = "; ")),
                 by = scheduled_trip_id]
    if (nrow(disp)) sd[disp, detalhe_frota := paste0(detalhe_frota, " · ", i.aviso), on = "scheduled_trip_id"]
  } else if (nrow(g)) g[, `:=`(n_saidas = 0L, saidas = NA_character_)]

  # resumo por veículo × dia
  vd <- vg[, .(viagens = .N, completas = sum(trajectory_status %in% c("viagem_confirmada", "viagem_provavel")),
               parciais = sum(!trajectory_status %in% c("viagem_confirmada", "viagem_provavel"))),
           by = .(vehicle_id, service_date)]
  # todos os veículos que transmitiram no dia (com ou sem viagem reconstruída)
  sinais <- pos[valido == TRUE, .(primeiro_sinal = min(timestamp_utc), ultimo_sinal = max(timestamp_utc),
                                  registros = .N, registros_em_rota = sum(na_rota %in% TRUE)),
                by = .(vehicle_id, service_date)]
  vd <- merge(sinais, vd, by = c("vehicle_id", "service_date"), all = TRUE)
  if (nrow(g)) {
    # uma lacuna explica no máximo `viagens_max` saídas
    gv <- g[, .(lacunas_10min = sum(!aberta), min_sem_gps = round(sum(dur_min[!aberta])),
                lacunas_que_cabem_viagem = sum(tipo == "cabe_viagem_sem_sinal"),
                lacunas_parado = sum(tipo == "parado_sem_sinal"),
                parou_de_transmitir = sum(aberta),
                saidas_explicaveis = sum(pmin(n_saidas, viagens_max))), by = .(vehicle_id, service_date)]
    vd <- merge(vd, gv, by = c("vehicle_id", "service_date"), all = TRUE)
  }
  for (k in c("viagens", "completas", "parciais", "lacunas_10min", "min_sem_gps", "lacunas_que_cabem_viagem",
              "lacunas_parado", "saidas_explicaveis", "parou_de_transmitir"))
    if (k %in% names(vd)) set(vd, which(is.na(vd[[k]])), k, 0L) else vd[, (k) := 0L]
  vd[, ultimo_sinal_local := format(ultimo_sinal, "%H:%M", tz = cfg$tz_local)]
  vd[, primeiro_sinal_local := format(primeiro_sinal, "%H:%M", tz = cfg$tz_local)]
  vd[, padrao := fcase(lacunas_que_cabem_viagem >= 2 & saidas_explicaveis >= 2, "gps_intermitente_recorrente",
                       lacunas_10min >= 1, "lacuna_isolada", default = "")]
  vd[, line_id := linha]

  if (nrow(g)) {
    tz <- cfg$tz_local
    g[, `:=`(line_id = linha,
             inicio_local = format(as.POSIXct(t0, origin = "1970-01-01", tz = tz), "%H:%M:%S", tz = tz),
             fim_local = fifelse(aberta, NA_character_, format(as.POSIXct(t1, origin = "1970-01-01", tz = tz), "%H:%M:%S", tz = tz)),
             s0_km = round(s0 / 1000, 2), s1_km = round(s1 / 1000, 2), dur_min = round(dur_min, 1),
             avanco_min_km = round(avanco_min_km, 2))]
    g <- g[, .(line_id, vehicle_id, service_date, gid, inicio_local, fim_local, dur_min, s0_km, s1_km,
               avanco_min_km, voltas_max, viagens_max, tipo, aberta, n_saidas, saidas, t0_utc = as.POSIXct(t0, origin = "1970-01-01", tz = "UTC"),
               t1_utc = as.POSIXct(t1, origin = "1970-01-01", tz = "UTC"))]
  }
  list(saidas = sd, lacunas = g, veiculos = vd)
}

# Ajuste final do status com o diagnóstico de frota (regra ipp-0.2.0):
#  - "não realizada" para a qual algum ônibus sem GPS poderia ter feito a viagem
#    (lacuna que cobre a janela −15/+40 min, ou veículo que parou de transmitir) passa a
#    "inconclusiva";
#  - "inconclusiva" cujo único motivo é um veículo que parou de transmitir e não voltou,
#    mas que não conseguiria chegar à origem a tempo, volta a "não realizada".
# Inconclusivas por lacuna fechada ou por falta de dados da linha não mudam.
reconciliar_status <- function(au, sd) {
  if (!nrow(au) || !nrow(sd)) return(list(au = au, sd = sd))
  au <- copy(au)
  x <- sd[, .(scheduled_trip_id, diagnostico_frota, veiculos_possiveis)][au, on = "scheduled_trip_id"]
  up <- x[audit_status == "possivelmente_nao_realizada" & grepl("^possivel_viagem", diagnostico_frota), scheduled_trip_id]
  dn <- x[audit_status == "inconclusiva" & so_lacuna_aberta %in% TRUE & data_coverage_status == "lacuna_gps_veiculo" &
            diagnostico_frota == "nenhum_veiculo_sem_gps_poderia_fazer", scheduled_trip_id]
  au[x[scheduled_trip_id %in% up], veiculos_sem_gps := i.veiculos_possiveis, on = "scheduled_trip_id"]
  au[scheduled_trip_id %in% up, `:=`(audit_status = "inconclusiva", data_coverage_status = "lacuna_gps_veiculo",
                                     evidence_summary = paste0(evidence_summary, "; frota: ônibus sem GPS poderia ter feito a viagem"))]
  au[scheduled_trip_id %in% dn, `:=`(audit_status = "possivelmente_nao_realizada",
                                     data_coverage_status = fifelse(frac_min_dados >= 0.8, "boa", "parcial"),
                                     evidence_summary = paste0(evidence_summary, "; frota: nenhum desses ônibus chegaria à origem a tempo"))]
  sd <- copy(sd)
  sd[au, audit_status := i.audit_status, on = "scheduled_trip_id"]
  list(au = au, sd = sd, n_up = length(up), n_dn = length(dn))
}

ROTULO_DIAG <- c(
  sem_dados_da_linha = "Sem dados da linha (falha geral de GPS/coleta)",
  nenhum_veiculo_sem_gps_poderia_fazer = "Nenhum ônibus sem GPS poderia tê-la feito",
  possivel_viagem_sem_gps_no_horario = "Possível viagem sem GPS (no horário)",
  possivel_viagem_sem_gps_com_atraso = "Possível viagem sem GPS (só com atraso)",
  conclusao_compativel_com_lacuna_gps = "Saiu; conclusão compatível com lacuna de GPS",
  lacuna_gps_curta_demais_para_concluir = "Saiu; lacuna de GPS curta demais para concluir",
  viagem_interrompida_sem_lacuna = "Saiu e interrompeu (GPS ativo)",
  viagem_quase_completa = "Quase completa (≥95% do traçado)")
