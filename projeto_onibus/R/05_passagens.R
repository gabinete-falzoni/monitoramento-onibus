############################################################
# 05_passagens.R — horário estimado de passagem em cada ponto
#
# Para cada viagem candidata e cada parada do seu sentido, o instante
# em que o progresso monotônico cruza a posição da parada no ciclo
# (nL + s_ciclo) é interpolado entre as duas posições que o cercam.
#
# Tabela "longa" e enxuta, gravada particionada por mês/linha, com
# stop_id como chave: depois de rodar para todas as linhas e meses,
# as passagens de um ponto se obtêm com
#   open_dataset(".../passagens") |> filter(stop_id == "...")
############################################################

estimar_passagens <- function(viagens, obs, ciclo, p = cfg$passagem) {
  if (!nrow(viagens)) return(data.table())
  par <- ciclo$paradas[, .(direction_id, stop_id, stop_sequence, s_ciclo)]
  L <- ciclo$L
  # viagens x paradas do sentido
  vv <- viagens[, .(trip_id, vehicle_id, direction_id, volta, chain, u_min, u_max, ch_ini, ch_fim,
                    trajectory_status)]
  x <- vv[par, on = "direction_id", allow.cartesian = TRUE, nomatch = 0]
  x[, thr := volta * L + s_ciclo]
  x <- x[thr >= u_min & thr <= u_max]
  if (!nrow(x)) return(data.table())
  k <- x$chain * .BIG + x$thr
  i <- findInterval(k, obs$key)
  i <- pmin(pmax(i, x$ch_ini), x$ch_fim)
  j <- pmin(i + 1L, x$ch_fim)
  u0 <- obs$u[i]; u1 <- obs$u[j]; t0 <- obs$tnum[i]; t1 <- obs$tnum[j]
  f <- fifelse(u1 > u0, (x$thr - u0) / (u1 - u0), 0)
  x[, t := t0 + pmin(pmax(f, 0), 1) * (t1 - t0)]
  x[, dt_par_s := as.integer(round(t1 - t0))]
  x[, metodo := fifelse(dt_par_s <= p$lacuna_max_s, "interpolado", "interpolado_sobre_lacuna")]
  out <- x[, .(trip_id, line_id = ciclo$linha, vehicle_id, direction_id,
               stop_id, stop_sequence = as.integer(stop_sequence),
               passage_time_utc = as.POSIXct(t, origin = "1970-01-01", tz = "UTC"),
               dt_par_s, metodo, trajectory_status)]
  out[, passage_time_local := format(passage_time_utc, "%Y-%m-%d %H:%M:%S", tz = cfg$tz_local)]
  out[, service_date := viagens$service_date[match(trip_id, viagens$trip_id)]]
  setorder(out, trip_id, stop_sequence)
  out[]
}
