############################################################
# 08_indicadores.R — indicadores de frota e viagens por hora
#
# Definições (hora local de São Paulo, dia de serviço com corte às 03h):
#  vehicles_observed        veículos distintos com ≥1 registro válido na hora
#  vehicles_with_movement   veículos distintos com ≥1 deslocamento > 5 km/h e > 50 m
#  vehicles_with_confirmed  veículos com viagem confirmada em curso na hora
#  vehicles_with_probable   idem, viagem provável (e nenhuma confirmada)
#  vehicles_inconclusive    observados sem viagem confirmada/provável em curso
#  active_vehicles_est      média, nos 12 blocos de 5 min da hora, do nº de
#                           veículos com ≥1 posição EM ROTA no bloco (observação
#                           efetiva; lacunas não são preenchidas)
#  trips_started/completed  viagens (confirmadas+prováveis+parciais) iniciadas /
#                           (confirmadas+prováveis) concluídas na hora
#  observation_coverage     fração dos minutos da hora com ≥1 registro da linha
############################################################

.hora_local <- function(tnum) as.integer(format(as.POSIXct(tnum, origin = "1970-01-01", tz = "UTC"), "%H", tz = cfg$tz_local))

indicadores_horarios_linha <- function(v, viagens, linha) {
  r <- v[(valido), .(vehicle_id, service_date, tnum, hora = .hora_local(tnum),
                     mov = !is.na(vel_kmh) & vel_kmh > 5 & dist_m > 50, na_rota)]
  base <- r[, .(vehicles_observed = uniqueN(vehicle_id),
                vehicles_with_movement = uniqueN(vehicle_id[mov]),
                observation_coverage = uniqueN(floor(tnum / 60)) / 60),
            by = .(service_date, hour_local = hora)]
  # pares (bloco de 5 min, veículo) com ≥1 ponto em rota / 12 blocos
  ab <- r[(na_rota), .(n = uniqueN(vehicle_id)), by = .(service_date, hora, bloco = floor((tnum %% 3600) / 300))][
    , .(active_vehicles_est = sum(n) / 12), by = .(service_date, hour_local = hora)]
  base <- ab[base, on = .(service_date, hour_local)]
  if (nrow(viagens)) {
    tv <- viagens[, .(vehicle_id, service_date, trajectory_status, s = start_num, e = end_num)]
    tv[, `:=`(h0 = floor(s / 3600), h1 = floor(e / 3600))]
    ex <- tv[, .(hh = seq(h0, h1)), by = .(vehicle_id, service_date, trajectory_status, rid = seq_len(nrow(tv)))]
    ex[, hour_local := .hora_local(hh * 3600)]
    vh <- ex[, .(conf = any(trajectory_status == "viagem_confirmada"),
                 prov = any(trajectory_status == "viagem_provavel")),
             by = .(service_date, hour_local, vehicle_id)]
    vh2 <- vh[, .(vehicles_with_confirmed = sum(conf), vehicles_with_probable = sum(prov & !conf)),
              by = .(service_date, hour_local)]
    tv[, `:=`(hs = .hora_local(s), he = .hora_local(e))]
    ts <- tv[trajectory_status != "observacao_inconclusiva",
             .(trips_started = .N, trips_partial = sum(trajectory_status == "trajetoria_parcial")),
             by = .(service_date, hour_local = hs)]
    tc <- tv[trajectory_status %in% c("viagem_confirmada", "viagem_provavel"),
             .(trips_completed = .N), by = .(service_date, hour_local = he)]
    base <- Reduce(function(a, b) merge(a, b, by = c("service_date", "hour_local"), all = TRUE),
                   list(base, vh2, ts, tc))
  }
  # grade completa de 24 h por dia
  grade <- CJ(service_date = unique(r$service_date), hour_local = 0:23)
  base <- base[grade, on = .(service_date, hour_local)]
  num <- setdiff(names(base), c("service_date", "hour_local"))
  for (cc in num) set(base, which(is.na(base[[cc]])), cc, 0)
  base[, vehicles_inconclusive := pmax(0, vehicles_observed - vehicles_with_confirmed - vehicles_with_probable)]
  base[, distinct_lines_operating := as.integer(vehicles_with_confirmed + vehicles_with_probable > 0)]
  base[, data_quality_flag := fcase(vehicles_observed == 0, "sem_dados",
                                    observation_coverage < 0.8, "cobertura_baixa", default = "ok")]
  base[, line_id := linha]
  setcolorder(base, c("line_id", "service_date", "hour_local"))
  base[]
}

veiculos_por_hora_linha <- function(v, viagens, linha) {
  r <- v[(valido), .(mov = any(!is.na(vel_kmh) & vel_kmh > 5 & dist_m > 50)),
         by = .(service_date, hour_local = .hora_local(tnum), vehicle_id)]
  if (nrow(viagens)) {
    tv <- viagens[, .(vehicle_id, trajectory_status, h0 = floor(start_num / 3600), h1 = floor(end_num / 3600))]
    ex <- tv[, .(hh = seq(h0, h1)), by = .(vehicle_id, trajectory_status, rid = seq_len(nrow(tv)))]
    ex[, `:=`(hour_local = .hora_local(hh * 3600),
              service_date = as.Date(format(as.POSIXct(hh * 3600 - cfg$hora_corte_dia * 3600, origin = "1970-01-01", tz = "UTC"), "%Y-%m-%d", tz = cfg$tz_local)))]
    rk <- c(viagem_confirmada = 1, viagem_provavel = 2, trajetoria_parcial = 3, observacao_inconclusiva = 4)
    best <- ex[, .(melhor = names(rk)[min(rk[trajectory_status])]), by = .(service_date, hour_local, vehicle_id)]
    r <- best[r, on = .(service_date, hour_local, vehicle_id)]
  } else r[, melhor := NA_character_]
  r[, classificacao := fcase(!is.na(melhor) & melhor != "observacao_inconclusiva", melhor,
                             mov, "movimento_sem_viagem_identificada",
                             default = "sem_evidencia_de_operacao")]
  r[, line_id := linha]
  r[, .(line_id, service_date, hour_local, vehicle_id, classificacao)]
}
