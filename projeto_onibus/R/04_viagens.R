############################################################
# 04_viagens.R — segmentação das cadeias em viagens e classificação
#
# Uma viagem candidata é a passagem de uma cadeia pelo intervalo de
# progresso [nL + a_k, nL + b_k] do sentido k na volta n. Os instantes
# são obtidos por interpolação linear do progresso monotônico (u_mono)
# entre posições consecutivas; nenhum horário programado é usado aqui.
############################################################

# Prepara índice global monotônico: chave = chain * BIG + u_mono
.BIG <- 1e9

indexar_obs <- function(v, p = cfg$viagem) {
  o <- v[(na_rota), .(vehicle_id, chain, tnum, u = u_mono, direction_sl, vel_kmh, dist_m, d_rota, x, y)]
  setorder(o, chain, tnum)
  o[, key := chain * .BIG + u]
  o[, `:=`(prox_mesma = c(chain[-1] == chain[-.N], FALSE))]
  o[, dt_prox := fifelse(prox_mesma, shift(tnum, -1) - tnum, NA_real_)]
  o[, du_prox := fifelse(prox_mesma, shift(u, -1) - u, 0)]
  o[, ok_prox := prox_mesma & dt_prox <= p$lacuna_interp_s]
  o[, cob_cum := cumsum(c(0, (du_prox * ok_prox)[-.N]))]   # cobertura acumulada até o ponto
  o[]
}

# Instante (interpolado) em que a cadeia cruza o progresso `thr`
cruzamento <- function(o, chain, thr) {
  k <- chain * .BIG + thr
  i <- findInterval(k, o$key)
  ok <- i >= 1 & i < nrow(o)
  ok[ok] <- o$chain[i[ok]] == chain[ok] & o$prox_mesma[i[ok]] & o$u[i[ok] + 1] > thr[ok]
  t <- dtp <- rep(NA_real_, length(thr))
  j <- i[ok]
  f <- (thr[ok] - o$u[j]) / (o$u[j + 1] - o$u[j])
  t[ok] <- o$tnum[j] + f * (o$tnum[j + 1] - o$tnum[j])
  dtp[ok] <- o$dt_prox[j]
  list(t = t, dt_par = dtp, i = ifelse(ok, i, NA_integer_))
}

# Evento de partida/chegada no limiar `thr` (ver cfg$viagem). Devolve o
# instante estimado e o tipo de evidência.
evento_terminal <- function(o, chain, thr, ch_ini, ch_fim, tipo, p = cfg$viagem) {
  n <- length(thr)
  i <- findInterval(chain * .BIG + thr, o$key)
  t <- rep(NA_real_, n); dtp <- rep(NA_real_, n); ev <- rep("nao_observado", n)
  vref <- p$vel_ref_terminal_ms; jan <- p$janela_terminal_m
  dentro <- i >= ch_ini & i < ch_fim
  antes <- i < ch_ini
  depois <- i >= ch_fim
  # par (j, j+1) que contém o limiar
  j <- i[dentro]
  if (length(j)) {
    u0 <- o$u[j]; u1 <- o$u[j + 1]; t0 <- o$tnum[j]; t1 <- o$tnum[j + 1]; th <- thr[dentro]
    ti <- t0 + (th - u0) / (u1 - u0) * (t1 - t0)
    d <- t1 - t0
    obs <- d <= p$lacuna_evento_s
    if (tipo == "partida") {
      perto <- !obs & (u1 - th) <= jan
      te <- pmax(t0, t1 - (u1 - th) / vref)
    } else {
      perto <- !obs & (th - u0) <= jan
      te <- pmin(t1, t0 + (th - u0) / vref)
    }
    t[dentro] <- fifelse(obs, ti, fifelse(perto, te, ti))
    ev[dentro] <- fifelse(obs, "observado", fifelse(perto, "estimado_terminal", "interpolado_lacuna"))
    dtp[dentro] <- d
  }
  if (tipo == "partida" && any(antes)) {
    uf <- o$u[ch_ini[antes]]; tf <- o$tnum[ch_ini[antes]]
    ok <- (uf - thr[antes]) <= jan
    t[antes] <- fifelse(ok, tf - (uf - thr[antes]) / vref, NA_real_)
    ev[antes] <- fifelse(ok, "estimado_inicio_cadeia", "nao_observado")
  }
  if (tipo == "chegada" && any(depois)) {
    ul <- o$u[ch_fim[depois]]; tl <- o$tnum[ch_fim[depois]]
    ok <- (thr[depois] - ul) <= jan
    t[depois] <- fifelse(ok, tl + pmax(0, thr[depois] - ul) / vref, NA_real_)
    ev[depois] <- fifelse(ok, "estimado_fim_cadeia", "nao_observado")
  }
  list(t = t, dt_par = dtp, ev = ev)
}

# Cobertura acumulada (m de progresso com pares dt <= lacuna) até `thr`
cobertura_ate <- function(o, chain, thr, ch_ini, ch_fim) {
  k <- chain * .BIG + thr
  i <- findInterval(k, o$key)
  i <- pmin(pmax(i, ch_ini), ch_fim)
  extra <- pmin(pmax(thr - o$u[i], 0), o$du_prox[i]) * o$ok_prox[i]
  o$cob_cum[i] + extra
}

extrair_viagens <- function(v, ciclo, p = cfg$viagem) {
  o <- indexar_obs(v, p)
  L <- ciclo$L; sen <- ciclo$sentidos
  o[, i := .I]
  ch <- o[, .(vehicle_id = vehicle_id[1], u_min = u[1], u_max = u[.N], ch_ini = i[1], ch_fim = i[.N]),
          by = chain]
  # voltas candidatas por cadeia x sentido
  tr <- ch[, .(volta = seq(floor(u_min / L) - 1, floor(u_max / L))), by = chain][
    , .(direction_id = sen$direction_id, a = sen$a, b = sen$b), by = .(chain, volta)][ch, on = "chain", nomatch = 0]
  tr[, `:=`(lo = volta * L + a, hi = volta * L + b)]
  tr[, sobrep := pmin(hi, u_max) - pmax(lo, u_min)]
  tr <- tr[sobrep >= p$sobreposicao_min_m]
  if (!nrow(tr)) return(list(viagens = data.table(), obs = o))

  # eventos de partida/chegada
  dep <- evento_terminal(o, tr$chain, tr$lo + p$offset_partida_m, tr$ch_ini, tr$ch_fim, "partida", p)
  arr <- evento_terminal(o, tr$chain, tr$hi - p$offset_chegada_m, tr$ch_ini, tr$ch_fim, "chegada", p)
  tr[, `:=`(t_partida = dep$t, dt_par_partida = dep$dt_par, ev_partida = dep$ev,
            t_chegada = arr$t, dt_par_chegada = arr$dt_par, ev_chegada = arr$ev)]
  # extremos observados dentro do intervalo
  lo_obs <- pmax(tr$lo, tr$u_min); hi_obs <- pmin(tr$hi, tr$u_max)
  ini <- cruzamento(o, tr$chain, lo_obs); fim <- cruzamento(o, tr$chain, hi_obs)
  # quando o extremo coincide com o início/fim da cadeia, usar o tempo do ponto
  t_ini <- fifelse(is.na(ini$t), o$tnum[tr$ch_ini], ini$t)
  t_fim <- fifelse(is.na(fim$t), o$tnum[tr$ch_fim], fim$t)
  tr[, `:=`(t_ini_obs = t_ini, t_fim_obs = t_fim)]
  # cobertura
  cob <- cobertura_ate(o, tr$chain, tr$hi, tr$ch_ini, tr$ch_fim) -
         cobertura_ate(o, tr$chain, tr$lo, tr$ch_ini, tr$ch_fim)
  tr[, `:=`(cobertura_m = cob, observation_coverage = pmin(1, cob / (hi - lo)),
            frac_observada = (hi_obs - lo_obs) / (hi - lo))]
  # índices dos pontos dentro do intervalo
  i_lo <- findInterval(tr$chain * .BIG + tr$lo, o$key) + 1L
  i_hi <- findInterval(tr$chain * .BIG + tr$hi, o$key)
  i_lo <- pmax(i_lo, tr$ch_ini); i_hi <- pmin(i_hi, tr$ch_fim)
  tr[, `:=`(i_lo = i_lo, i_hi = i_hi, n_obs = pmax(0L, i_hi - i_lo + 1L))]
  # pares que tocam o intervalo: de i_lo-1 até i_hi
  pa <- pmax(i_lo - 1L, tr$ch_ini); pb <- pmin(i_hi, tr$ch_fim - 1L)
  tr[, lacuna_max_s := range_max(o$dt_prox, pa, pb)]
  tr[, c("vel_mediana_kmh", "vel_p95_kmh", "dist_gps_km", "d_rota_mediana_m", "frac_d_rota_50m",
         "frac_sl_moda") := NA_real_]
  tr[, sl_moda := NA_integer_]
  # métricas pontuais dentro do intervalo (agregação por faixa de índices)
  rows <- which(tr$n_obs > 0)
  ix <- data.table(rid = rep(seq_along(rows), tr$n_obs[rows]))
  ix[, i := tr$i_lo[rows][rid] + rowid(rid) - 1L]
  if (nrow(ix)) {
    ix[, `:=`(vel = o$vel_kmh[i], dm = o$dist_m[i], d = o$d_rota[i], sl = o$direction_sl[i])]
    ag <- ix[, .(vel_mediana_kmh = median(vel[vel > 0], na.rm = TRUE),
                 vel_p95_kmh = as.numeric(quantile(vel, .95, na.rm = TRUE, names = FALSE)),
                 dist_gps_km = sum(dm[-1], na.rm = TRUE) / 1000,
                 d_rota_mediana_m = median(d, na.rm = TRUE),
                 frac_d_rota_50m = mean(d <= 50, na.rm = TRUE),
                 sl_moda = as.integer(names(which.max(table(sl)))),
                 frac_sl_moda = max(table(sl)) / .N), by = rid]
    tr[rows[ag$rid], `:=`(vel_mediana_kmh = ag$vel_mediana_kmh, vel_p95_kmh = ag$vel_p95_kmh,
                          dist_gps_km = ag$dist_gps_km, d_rota_mediana_m = ag$d_rota_mediana_m,
                          frac_d_rota_50m = ag$frac_d_rota_50m, sl_moda = ag$sl_moda,
                          frac_sl_moda = ag$frac_sl_moda)]
  }
  list(viagens = tr, obs = o)
}

classificar_viagens <- function(tr, ciclo, linha, p = cfg$viagem) {
  if (!nrow(tr)) return(tr)
  tz <- cfg$tz_local
  ok_ev <- c("observado", "estimado_terminal", "estimado_inicio_cadeia", "estimado_fim_cadeia")
  tr[, origem_ok := ev_partida %in% ok_ev]
  tr[, destino_ok := ev_chegada %in% ok_ev]
  # sem evidência de partida, o instante "interpolado sobre lacuna" não é usado
  tr[origem_ok == FALSE, t_partida := NA_real_]
  tr[destino_ok == FALSE, t_chegada := NA_real_]
  tr[, start_num := fifelse(!is.na(t_partida), t_partida, t_ini_obs)]
  tr[, end_num := fifelse(!is.na(t_chegada), t_chegada, t_fim_obs)]
  tr[, duration_minutes := (end_num - start_num) / 60]
  tr[, distance_km := (pmin(hi, u_max) - pmax(lo, u_min)) / 1000]
  tr[, vel_media_kmh := fifelse(duration_minutes > 0, distance_km / (duration_minutes / 60), NA_real_)]
  vel_ok <- quote(!is.na(vel_media_kmh) & vel_media_kmh >= p$vel_media_min_kmh & vel_media_kmh <= p$vel_media_max_kmh)
  tr[, trajectory_status := fcase(
    origem_ok & destino_ok & observation_coverage >= p$cobertura_confirmada &
      lacuna_max_s <= p$lacuna_max_confirmada_s & eval(vel_ok), "viagem_confirmada",
    origem_ok & destino_ok & observation_coverage >= p$cobertura_provavel, "viagem_provavel",
    frac_observada >= p$cobertura_parcial | observation_coverage >= p$cobertura_parcial, "trajetoria_parcial",
    default = "observacao_inconclusiva")]
  tr[, confidence_level := fcase(trajectory_status == "viagem_confirmada", "alta",
                                 trajectory_status == "viagem_provavel", "media",
                                 trajectory_status == "trajetoria_parcial", "baixa",
                                 default = "muito_baixa")]
  tr[, route_match_status := fcase(is.na(frac_d_rota_50m), NA_character_,
                                   frac_d_rota_50m >= 0.95, "aderente",
                                   frac_d_rota_50m >= 0.80, "aderente_com_ruido",
                                   default = "baixa_aderencia")]
  tr[, origin_evidence := fcase(ev_partida != "nao_observado", ev_partida,
                                u_min > lo + p$offset_partida_m, "iniciou_apos_origem",
                                default = "sem_evidencia")]
  tr[, destination_evidence := fcase(ev_chegada != "nao_observado", ev_chegada,
                                     u_max < hi - p$offset_chegada_m, "terminou_antes_destino",
                                     default = "sem_evidencia")]
  # motivos de incerteza / critérios
  tr[, quality_flags := trimws(paste(
    fifelse(!origem_ok, "sem_partida_confirmada", ""),
    fifelse(!destino_ok, "sem_chegada_confirmada", ""),
    fifelse(observation_coverage < p$cobertura_confirmada, sprintf("cobertura_%.0f%%", 100 * observation_coverage), ""),
    fifelse(!is.na(lacuna_max_s) & lacuna_max_s > cfg$viagem$lacuna_interp_s, sprintf("lacuna_max_%.0fs", lacuna_max_s), ""),
    fifelse(!eval(vel_ok), "velocidade_media_implausivel", ""),
    fifelse(!is.na(route_match_status) & route_match_status == "baixa_aderencia", "baixa_aderencia_rota", "")
  ))]
  tr[, quality_flags := gsub("\\s+", ";", quality_flags)]
  tr[, `:=`(line_id = linha,
            start_time_utc = as.POSIXct(start_num, origin = "1970-01-01", tz = "UTC"),
            end_time_utc = as.POSIXct(end_num, origin = "1970-01-01", tz = "UTC"),
            departure_time_utc = as.POSIXct(t_partida, origin = "1970-01-01", tz = "UTC"),
            arrival_time_utc = as.POSIXct(t_chegada, origin = "1970-01-01", tz = "UTC"))]
  tr[, `:=`(start_time_local = format(start_time_utc, "%Y-%m-%d %H:%M:%S", tz = tz),
            end_time_local = format(end_time_utc, "%Y-%m-%d %H:%M:%S", tz = tz),
            service_date = as.Date(format(start_time_utc - cfg$hora_corte_dia * 3600, "%Y-%m-%d", tz = tz)))]
  setorder(tr, vehicle_id, start_num)
  tr[, trip_id := paste(line_id, vehicle_id, format(start_time_utc, "%Y%m%dT%H%M%S", tz = tz),
                        direction_id, sep = "_")]
  tr[, trip_id := make.unique(trip_id, sep = "_")]
  tr[, `:=`(circular = ciclo$circular, gtfs_versao = ciclo$gtfs_versao,
            algorithm_version = cfg$algorithm_version)]
  tr[]
}

# Resumo das cadeias (trajetórias reconstruídas) e classificação de
# veículo-dia (inclui as categorias D/E/F do roteiro)
resumir_trajetorias <- function(v, linha) {
  vv <- v[, .(vehicle_id, chain, tnum, na_rota, u_mono, dt_s, dist_m, vel_kmh, quebra, service_date)]
  traj <- vv[(na_rota), .(
    vehicle_id = vehicle_id[1], service_date = service_date[1],
    inicio_utc = as.POSIXct(min(tnum), origin = "1970-01-01", tz = "UTC"),
    fim_utc = as.POSIXct(max(tnum), origin = "1970-01-01", tz = "UTC"),
    n_obs = .N, progresso_km = (max(u_mono) - min(u_mono)) / 1000,
    tempo_movimento_min = sum(dt_s[vel_kmh > 3 & dt_s <= 300], na.rm = TRUE) / 60,
    tempo_parado_min = sum(dt_s[vel_kmh <= 3 & dt_s <= 300], na.rm = TRUE) / 60,
    n_lacunas_5min = sum(dt_s > 300, na.rm = TRUE),
    lacunas_min = sum(dt_s[dt_s > 300], na.rm = TRUE) / 60,
    motivo_inicio = c("", "inicio_veiculo", "lacuna_longa", "transicao_implausivel")[quebra[1] + 1]
  ), by = chain]
  traj[, line_id := linha]
  traj[]
}

classificar_veiculo_dia <- function(v, viagens, linha) {
  vd <- v[(valido), .(n_obs = .N, frac_na_rota = mean(na_rota),
                      dist_gps_km = sum(dist_m[!is.na(vel_kmh) & vel_kmh <= 120], na.rm = TRUE) / 1000,
                      progresso_rota_km = sum(pmax(delta, 0), na.rm = TRUE) / 1000),
          by = .(vehicle_id, service_date)]
  if (nrow(viagens)) {
    vg <- viagens[, .(n_confirmadas = sum(trajectory_status == "viagem_confirmada"),
                      n_provaveis = sum(trajectory_status == "viagem_provavel"),
                      n_parciais = sum(trajectory_status == "trajetoria_parcial")),
                  by = .(vehicle_id, service_date)]
    vd <- vg[vd, on = .(vehicle_id, service_date)]
  } else vd[, c("n_confirmadas", "n_provaveis", "n_parciais") := NA_integer_]
  for (cc in c("n_confirmadas", "n_provaveis", "n_parciais")) set(vd, which(is.na(vd[[cc]])), cc, 0L)
  vd[, classificacao := fcase(
    n_confirmadas > 0, "viagem_confirmada",
    n_provaveis > 0, "viagem_provavel",
    n_parciais > 0, "trajetoria_parcial",
    n_obs < 20, "observacao_inconclusiva",
    dist_gps_km >= 2 & progresso_rota_km < 1, "movimento_sem_itinerario",
    dist_gps_km < 2, "sem_evidencia_de_operacao",
    default = "observacao_inconclusiva")]
  vd[, line_id := linha]
  vd[]
}
