############################################################
# 02_referencia.R — itinerários (GTFS) como "ciclo" da linha
#
# Ideia central: toda linha é representada por um CICLO fechado em metros.
#   - Linha com dois sentidos (ex. 701A-10): ciclo = shape do sentido 0
#     (TP -> TS) seguido do shape do sentido 1 (TS -> TP). Comprimento L.
#     Sentido 0 ocupa [0, L0); sentido 1 ocupa [L0, L).
#   - Linha circular (ex. 7267-10, um único trip no GTFS): ciclo = o próprio
#     shape, que começa e termina no TP. Uma volta = uma viagem.
# Assim o ônibus apenas "avança" no ciclo, e a troca de sentido é inferida
# pela posição (cruzar L0 ou L), não pela marcação `sl` do motorista.
############################################################

ler_gtfs <- function(gtfs_dir, linhas = NULL) {
  message("GTFS: ", gtfs_dir)
  trips  <- fread(file.path(gtfs_dir, "trips.txt"), colClasses = "character")
  if (!is.null(linhas)) trips <- trips[route_id %in% linhas]
  st     <- fread(file.path(gtfs_dir, "stop_times.txt"), colClasses = "character",
                  select = c("trip_id", "stop_id", "stop_sequence"))
  st     <- st[trip_id %in% trips$trip_id]
  st[, stop_sequence := as.integer(stop_sequence)]
  stops  <- fread(file.path(gtfs_dir, "stops.txt"), colClasses = c(stop_id = "character"))
  shapes <- fread(file.path(gtfs_dir, "shapes.txt"), colClasses = c(shape_id = "character"))
  shapes <- shapes[shape_id %in% trips$shape_id]
  list(dir = gtfs_dir, versao = basename(gtfs_dir), trips = trips, stop_times = st,
       stops = stops, shapes = shapes)
}

# Converte lon/lat para o CRS métrico
para_metrico <- function(lon, lat, crs = cfg$crs_metrico) {
  xy <- sf::sf_project(from = "EPSG:4326", to = paste0("EPSG:", crs), cbind(lon, lat))
  list(x = xy[, 1], y = xy[, 2])
}

# Escolhe, para cada parada (em ordem), a posição ao longo do shape do sentido
# que mantém a ordem da sequência (essencial em laços que passam duas vezes
# pela mesma via).
projetar_paradas_monotono <- function(sx, sy, vx, vy, vcum, raio = 150) {
  cand <- as.data.table(mm_candidates(sx, sy, vx, vy, vcum, raio, 20, 10, FALSE))
  n <- length(sx); s_out <- rep(NA_real_, n); d_out <- rep(NA_real_, n)
  prev <- -Inf
  for (i in seq_len(n)) {
    ci <- cand[pt == i]
    if (!nrow(ci)) next
    ok <- ci[s >= prev - 30 & d <= min(d) + 15]
    if (!nrow(ok)) ok <- ci[s >= prev - 30]
    if (!nrow(ok)) ok <- ci[which.min(d)]
    j <- which.min(ok$s)
    s_out[i] <- ok$s[j]; d_out[i] <- ok$d[j]; prev <- s_out[i]
  }
  list(s = s_out, d = d_out)
}

# Monta o ciclo de uma linha a partir de uma versão do GTFS
montar_ciclo_linha <- function(gtfs, linha) {
  tr <- gtfs$trips[route_id == linha]
  if (!nrow(tr)) return(NULL)
  tr[, direction_id := as.integer(direction_id)]
  setorder(tr, direction_id)
  tr <- tr[, .SD[1], by = direction_id]           # um trip representativo por sentido
  circular <- nrow(tr) == 1L

  pecas <- lapply(seq_len(nrow(tr)), function(k) {
    sh <- gtfs$shapes[shape_id == tr$shape_id[k]][order(shape_pt_sequence)]
    xy <- para_metrico(sh$shape_pt_lon, sh$shape_pt_lat)
    data.table(direction_id = tr$direction_id[k], x = xy$x, y = xy$y)
  })
  vert <- rbindlist(pecas)
  # fecha o ciclo voltando ao primeiro vértice
  vert <- rbind(vert, vert[1][, direction_id := -1L])
  vert[, cum := c(0, cumsum(sqrt(diff(x)^2 + diff(y)^2)))]
  L <- vert[.N, cum]

  sentidos <- vert[direction_id >= 0, .(a = min(cum)), by = direction_id]
  setorder(sentidos, direction_id)
  sentidos[, b := shift(a, -1, fill = L)]
  sentidos[, `:=`(headsign = tr$trip_headsign, trip_id = tr$trip_id, shape_id = tr$shape_id)]
  fechamento_m <- sqrt(diff(vert[c(.N - 1, .N), x])^2 + diff(vert[c(.N - 1, .N), y])^2)

  # paradas, projetadas sentido a sentido, mantendo a ordem
  paradas <- rbindlist(lapply(seq_len(nrow(tr)), function(k) {
    st <- gtfs$stop_times[trip_id == tr$trip_id[k]][order(stop_sequence)]
    st <- merge(st, gtfs$stops[, .(stop_id, stop_name, stop_lat, stop_lon)], by = "stop_id",
                sort = FALSE)[order(stop_sequence)]
    pv <- vert[direction_id == tr$direction_id[k]]
    pv[, cum0 := c(0, cumsum(sqrt(diff(x)^2 + diff(y)^2)))]
    xy <- para_metrico(st$stop_lon, st$stop_lat)
    pr <- projetar_paradas_monotono(xy$x, xy$y, pv$x, pv$y, pv$cum0)
    st[, `:=`(direction_id = tr$direction_id[k], s_sentido = pr$s, dist_tracado_m = pr$d,
              s_ciclo = pr$s + sentidos$a[k])]
  }))
  paradas[, linha := linha]

  list(linha = linha, gtfs_versao = gtfs$versao, circular = circular, L = L,
       fechamento_m = fechamento_m, vert = vert, sentidos = sentidos, paradas = paradas)
}

# Mede quão bem um ciclo explica uma amostra de pontos GPS
ajuste_ciclo <- function(ciclo, px_m, py_m, raio = 50) {
  cand <- mm_candidates(px_m, py_m, ciclo$vert$x, ciclo$vert$y, ciclo$vert$cum, raio, 40, 1)
  length(unique(cand$pt)) / length(px_m)
}

# Monta o ciclo da linha em cada versão de GTFS e escolhe a de melhor
# ajuste aos pontos GPS daquela linha (amostra de até 20 mil pontos).
escolher_ciclo <- function(gtfs_list, linha, x, y, n_amostra = 20000) {
  ok <- is.finite(x) & is.finite(y)
  x <- x[ok]; y <- y[ok]
  if (length(x) > n_amostra) { set.seed(1); i <- sample.int(length(x), n_amostra); x <- x[i]; y <- y[i] }
  cands <- lapply(gtfs_list, montar_ciclo_linha, linha = linha)
  fits <- vapply(cands, function(ci) if (is.null(ci)) NA_real_ else ajuste_ciclo(ci, x, y), 0)
  diag <- data.table(line_id = linha, gtfs_versao = vapply(gtfs_list, `[[`, "", "versao"),
                     frac_pontos_ate_50m = round(fits, 4),
                     L_km = vapply(cands, function(ci) if (is.null(ci)) NA_real_ else round(ci$L / 1000, 3), 0),
                     circular = vapply(cands, function(ci) if (is.null(ci)) NA else ci$circular, NA))
  if (all(is.na(fits))) return(list(ciclo = NULL, diag = diag))
  best <- which.max(fits)   # empate -> primeira (ordem de preferência em cfg)
  diag[, escolhida := seq_len(.N) == best]
  list(ciclo = cands[[best]], diag = diag)
}
