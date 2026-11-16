############################################################
# 01_normalizacao_qc.R — inventário, leitura, padronização e QC
#
# Os arquivos brutos nunca são alterados. A leitura é seletiva (arrow
# dataset filtrado por linha), o que permite rodar linha a linha sobre
# um acervo com todas as linhas e meses.
############################################################

abrir_posicoes <- function(arquivos = cfg$entrada$posicoes) {
  arrow::open_dataset(arquivos)
}

# Inventário barato (via arrow, sem carregar tudo na memória)
inventario_arquivos <- function(arquivos = cfg$entrada$posicoes) {
  rbindlist(lapply(arquivos, function(f) {
    ds <- arrow::open_dataset(f)
    agg <- ds |> dplyr::summarise(n = dplyr::n(), ta_min = min(ta), ta_max = max(ta),
                                  n_veic = dplyr::n_distinct(p), n_linhas = dplyr::n_distinct(c)) |>
      dplyr::collect()
    data.table(arquivo = basename(f), tamanho_mb = round(file.size(f) / 2^20, 2),
               md5 = unname(tools::md5sum(f)),
               esquema = paste(names(ds$schema), vapply(ds$schema$fields, \(x) x$type$ToString(), ""),
                               sep = ":", collapse = ", "),
               registros = agg$n, veiculos = agg$n_veic, linhas = agg$n_linhas,
               ta_min_utc = agg$ta_min, ta_max_utc = agg$ta_max)
  }))
}

listar_linhas <- function(ds) {
  ds |> dplyr::distinct(c) |> dplyr::collect() |> dplyr::pull(c) |> sort()
}

# Lê e normaliza as posições de uma linha
ler_posicoes_linha <- function(ds, linha) {
  raw <- ds |> dplyr::filter(c == linha) |>
    dplyr::mutate(source_file = arrow::add_filename()) |>
    dplyr::collect() |> as.data.table()
  raw[, source_file := basename(source_file)]
  # `ta` vem como texto ISO 8601 em UTC ("...Z"); aceitar também POSIXct
  if (is.character(raw$ta)) {
    raw[, timestamp_utc := as.POSIXct(ta, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")]
  } else {
    raw[, timestamp_utc := lubridate::with_tz(ta, "UTC")]
  }
  dt <- raw[, .(source_file, vehicle_id = as.character(p), line_id = c, cl = as.integer(cl),
                direction_sl = as.integer(sl), timestamp_utc,
                latitude = as.numeric(py), longitude = as.numeric(px))]
  setorder(dt, vehicle_id, timestamp_utc, source_file)
  dt[, source_row_id := paste(source_file, vehicle_id, format(timestamp_utc, "%Y%m%dT%H%M%S"), sep = "|")]
  dt[, timestamp_local := lubridate::with_tz(timestamp_utc, cfg$tz_local)]
  # dia de serviço: corte às `hora_corte_dia` locais
  dt[, service_date := as.Date(format(timestamp_local - cfg$hora_corte_dia * 3600, "%Y-%m-%d"))]
  dt[]
}

# Sinalizações de qualidade por registro (nada é excluído silenciosamente)
qc_posicoes <- function(dt) {
  bb <- cfg$bbox_sp
  dt[, qc_prefixo_ausente := is.na(vehicle_id) | vehicle_id == ""]
  dt[, qc_linha_ausente := is.na(line_id) | line_id == ""]
  dt[, qc_ts_invalido := is.na(timestamp_utc)]
  dt[, qc_coord_invalida := is.na(latitude) | is.na(longitude) |
       longitude < bb[["lon_min"]] | longitude > bb[["lon_max"]] |
       latitude < bb[["lat_min"]] | latitude > bb[["lat_max"]]]
  dt[, qc_dup_exato := duplicated(dt, by = c("vehicle_id", "timestamp_utc", "latitude", "longitude"))]
  # mesmo veículo e instante, coordenada diferente (mantém o primeiro)
  dt[, qc_dup_instante := !qc_dup_exato & duplicated(dt, by = c("vehicle_id", "timestamp_utc"))]
  dt[, valido := !(qc_prefixo_ausente | qc_linha_ausente | qc_ts_invalido | qc_coord_invalida |
                     qc_dup_exato | qc_dup_instante)]

  # métricas entre posições válidas consecutivas do mesmo veículo
  # (dt já está ordenado por veículo e tempo; usa-se aritmética vetorial
  #  com máscara de "mesmo veículo" em vez de agrupamentos, por desempenho)
  dt[, c("x", "y") := NA_real_]
  dt[(valido), c("x", "y") := para_metrico(longitude, latitude)]
  dt[, tnum := as.numeric(timestamp_utc)]
  iv <- which(dt$valido)
  vv <- dt$vehicle_id[iv]
  mesmo <- c(FALSE, vv[-1] == vv[-length(vv)])
  tt <- dt$tnum[iv]; xx <- dt$x[iv]; yy <- dt$y[iv]
  dts <- c(NA, diff(tt)); dts[!mesmo] <- NA
  dm <- c(NA, sqrt(diff(xx)^2 + diff(yy)^2)); dm[!mesmo] <- NA
  dt[, c("dt_s", "dist_m") := NA_real_]
  set(dt, iv, "dt_s", dts); set(dt, iv, "dist_m", dm)
  dt[, vel_kmh := fifelse(dt_s > 0, dist_m / dt_s * 3.6, NA_real_)]
  dt[, qc_salto_espacial := valido & !is.na(vel_kmh) & vel_kmh > 120 & dist_m > 500]
  dt[, qc_lacuna_5min := valido & !is.na(dt_s) & dt_s > 300]
  # coordenada travada: mesma coordenada exata por > 30 min
  run <- rleid(vv, dt$latitude[iv], dt$longitude[iv])
  rr <- data.table(run, tt)[, .(dur = max(tt) - min(tt)), by = run]
  dt[, qc_coord_travada := FALSE]
  set(dt, iv, "qc_coord_travada", rr$dur[run] > 1800)
  sl <- dt$direction_sl[iv]
  dt[, qc_troca_sl := FALSE]
  set(dt, iv, "qc_troca_sl", mesmo & c(FALSE, sl[-1] != sl[-length(sl)]))
  dt[]
}

resumo_qualidade <- function(dt, linha) {
  qcs <- grep("^qc_", names(dt), value = TRUE)
  rbind(
    data.table(linha = linha, indicador = "registros_total", n = nrow(dt)),
    data.table(linha = linha, indicador = qcs, n = vapply(qcs, \(q) sum(dt[[q]], na.rm = TRUE), 0)),
    data.table(linha = linha, indicador = "registros_validos", n = sum(dt$valido)),
    data.table(linha = linha, indicador = c("dt_p10_s", "dt_mediana_s", "dt_p90_s", "dt_p99_s"),
               n = round(quantile(dt$dt_s, c(.1, .5, .9, .99), na.rm = TRUE), 1))
  )
}
