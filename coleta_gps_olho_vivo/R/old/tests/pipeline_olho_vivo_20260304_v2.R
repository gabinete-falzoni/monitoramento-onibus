############################################################
# Monitoramento Ôlho Vivo – pipeline 2026‑03‑04
#
# Objetivos principais:
# 1. Ler todos os arquivos .parquet de posições, remover duplicados
#    e focar no dia 2026‑03‑04.
# 2. Construir objetos GTFS (paradas, shapes, tempos relativos).
# 3. Para uma linha/direção (ex.: c == "478P-10" & sl == 2):
#    - Estimar horário em que cada ônibus passou em cada parada.
#    - Estimar horários de início de cada viagem (ida/volta) por carro.
#    - Detectar problemas de transmissão de GPS (buracos longos).
#
# O código é organizado em etapas e funções para facilitar
# reprodução (ex.: em RStudio). Você pode generalizar para
# outras linhas/direções alterando os parâmetros na seção
# "Parâmetros principais".
############################################################

## ------------------------------------------------------------------
## Pacotes
## ------------------------------------------------------------------

library(tidyverse)
library(tidylog)
library(arrow)
library(sf)
library(lubridate)
library(data.table)
library(mapview)


## ------------------------------------------------------------------
## Utilitários geométricos (sem st_line_locate_point)
## ------------------------------------------------------------------

# Projetar pontos em um LINESTRING e retornar a distância (m) ao longo da linha.
# - `line` é um sfc LINESTRING de comprimento 1 (em CRS métrico, ex.: 31983)
# - `pts`  é um sfc POINT (vetor) no mesmo CRS do line
# Retorno:
# - numeric: distância ao longo do trajeto (em metros) para cada ponto
line_project_distance_m <- function(line, pts, block_size = 20000) {
  line_geom <- sf::st_geometry(line)
  if (length(line_geom) != 1) {
    stop("`line` deve ter exatamente 1 geometria (LINESTRING).")
  }

  line_xy <- sf::st_coordinates(line_geom)[, c("X", "Y"), drop = FALSE]
  if (nrow(line_xy) < 2) {
    stop("`line` precisa ter pelo menos 2 vértices.")
  }

  pts_xy <- sf::st_coordinates(pts)[, c("X", "Y"), drop = FALSE]
  n_pts <- nrow(pts_xy)

  x0 <- line_xy[-nrow(line_xy), 1]
  y0 <- line_xy[-nrow(line_xy), 2]
  x1 <- line_xy[-1, 1]
  y1 <- line_xy[-1, 2]

  dx <- x1 - x0
  dy <- y1 - y0
  seg_len2 <- dx^2 + dy^2
  seg_len <- sqrt(seg_len2)

  # Comprimento acumulado antes de cada segmento
  cum_len_before <- c(0, cumsum(seg_len))[seq_along(seg_len)]

  out <- numeric(n_pts)
  if (n_pts == 0) return(out)

  idx_start <- seq(1, n_pts, by = block_size)
  for (s in idx_start) {
    e <- min(n_pts, s + block_size - 1)
    px <- pts_xy[s:e, 1]
    py <- pts_xy[s:e, 2]

    best_d2 <- rep(Inf, length(px))
    best_dist <- rep(NA_real_, length(px))

    for (i in seq_along(seg_len)) {
      if (!is.finite(seg_len2[i]) || seg_len2[i] == 0) next

      t <- ((px - x0[i]) * dx[i] + (py - y0[i]) * dy[i]) / seg_len2[i]
      t <- pmax(0, pmin(1, t))

      projx <- x0[i] + t * dx[i]
      projy <- y0[i] + t * dy[i]

      d2 <- (px - projx)^2 + (py - projy)^2
      better <- d2 < best_d2

      if (any(better)) {
        best_d2[better] <- d2[better]
        best_dist[better] <- cum_len_before[i] + t[better] * seg_len[i]
      }
    }

    out[s:e] <- best_dist
  }

  out
}

## ------------------------------------------------------------------
## Parâmetros principais – ajuste conforme necessário
## ------------------------------------------------------------------

# Caminho base do projeto (ajuste se o seu for diferente)
pasta_base    <- "/media/livre/Expansion/projetos/2026_Monitoramento_Olho_Vivo"
pasta_parquet <- file.path(pasta_base, "parquet_posicao")
pasta_gtfs    <- file.path(pasta_base, "cittamobi_gtfs")
pasta_testes  <- file.path(pasta_base, "testes")

# Data de interesse (dia completo)
data_ref      <- as.Date("2026-03-04")

# Linha/direção foco inicial (pode ser generalizado depois)
linha_foco    <- "478P-10"  # coluna c nas posições
sentido_sl    <- 2          # coluna sl nas posições (1 ou 2); direction_id = sl - 1

## ------------------------------------------------------------------
## 1. Ler posições em .parquet, remover duplicados e filtrar data
## ------------------------------------------------------------------

carregar_posicoes_dia <- function(data_ref,
                                  pasta_parquet,
                                  pasta_testes = NULL) {
  message("Lendo posições de ", pasta_parquet, " para o dia ", as.character(data_ref))

  ds <- open_dataset(pasta_parquet)
  # ds <- open_dataset('/media/livre/Expansion/projetos/2026_Monitoramento_Olho_Vivo/testes/posicoes.parquet')

  # Coluna de tempo geralmente se chama "ta" (como nos seus scripts).
  # Convertendo para POSIXct e filtrando para o dia desejado.
  posicoes_dia <- ds %>%
    filter(c == linha_foco & sl == sentido_sl) %>%
    # mutate(ta = ymd_hms(ta)) %>%
    # filter(between(ta, ymd_hms('2026-03-04 04:00:00'), ymd_hms('2026-03-05 04:00:00')))  %>%
    collect() %>%
    distinct()

  posicoes_dia <- posicoes_dia %>% mutate(ta = ymd_hms(ta)) %>%
    filter(between(ta, ymd_hms('2026-03-04 04:00:00'), ymd_hms('2026-03-05 04:00:00'))) %>%
    distinct()

  # if (!is.null(pasta_testes)) {
  #   dir.create(pasta_testes, showWarnings = FALSE, recursive = TRUE)
  #   out_path <- file.path(
  #     pasta_testes,
  #     paste0("posicoes_478p_", format(data_ref, "%Y%m%d"), ".parquet")
  #   )
  #   message("Gravando posições filtradas em: ", out_path)
  #   write_parquet(posicoes_dia, out_path)
  # }

  posicoes_dia
}

## ------------------------------------------------------------------
## 2. Ler GTFS e construir estruturas auxiliares
## ------------------------------------------------------------------

carregar_gtfs <- function(pasta_gtfs) {
  message("Lendo GTFS de ", pasta_gtfs)

  stops <- read_delim(
    file.path(pasta_gtfs, "stops.txt"),
    delim = ",",
    col_types = "cccdd"
  )

  stop_times <- read_delim(
    file.path(pasta_gtfs, "stop_times.txt"),
    delim = ",",
    col_types = "cttci"
  )

  trips <- read_delim(
    file.path(pasta_gtfs, "trips.txt"),
    delim = ",",
    col_types = cols()
  )

  routes <- read_delim(
    file.path(pasta_gtfs, "routes.txt"),
    delim = ",",
    col_types = cols()
  )

  shapes <- read_delim(
    file.path(pasta_gtfs, "shapes.txt"),
    delim = ",",
    col_types = cols()
  )

  frequencies <- read_delim(
    file.path(pasta_gtfs, "frequencies.txt"),
    delim = ",",
    col_types = cols()
  )

  # Tempo relativo acumulado por viagem
  stop_times_rel <- stop_times %>%
    group_by(trip_id) %>%
    mutate(
      relative_arrival_time = lead(arrival_time) - arrival_time,
      relative_arrival_time = as.difftime(
        cumsum(replace_na(as.numeric(relative_arrival_time), 0)),
        units = "secs"
      )
    ) %>%
    ungroup()

  # gtfs <-
  list(
    stops = stops,
    stop_times = stop_times_rel,
    trips = trips,
    routes = routes,
    shapes = shapes,
    frequencies = frequencies
  )
}

## ------------------------------------------------------------------
## 3. Construir paradas (sf) e linha de rota para uma linha/direção
## ------------------------------------------------------------------

montar_paradas_linha <- function(gtfs,
                                 linha_c,   # ex.: "478P-10"
                                 sentido_sl # ex.: 2 -> direction_id 1
) {
  direction_id <- sentido_sl - 1L

  # Muitos feeds Cittamobi usam trip_id no formato "<linha>-<direction_id>"
  trip_id_alvo <- paste0(linha_c, "-", direction_id)

  # -------------------------------------------------------------------
  # 3.1 Paradas da viagem (usando stop_times + stops)
  # -------------------------------------------------------------------

  prds <- gtfs$stops %>%
    select(stop_id, stop_lon, stop_lat) %>%
    st_as_sf(coords = c("stop_lon", "stop_lat"), crs = 4326)

  prds_times <- gtfs$stop_times %>%
    filter(trip_id == trip_id_alvo) %>%
    select(trip_id, stop_id, stop_sequence, relative_arrival_time)

  paradas_linha <- prds %>%
    left_join(prds_times, by = "stop_id") %>%
    filter(!is.na(trip_id)) %>%
    arrange(trip_id, stop_sequence) %>%
    st_transform(31983)
  # mapview(paradas_linha)

  # -------------------------------------------------------------------
  # 3.2 Linha de rota a partir de shapes.txt (mapa de fato da linha)
  # -------------------------------------------------------------------
  #
  # Escolhemos um shape_id representativo para essa linha+sentido:
  #   - filtra trips que começam com "<linha_c>-" e têm o direction_id desejado
  #   - escolhe o shape_id mais frequente (caso haja mais de um).

  trips_linha <- gtfs$trips %>% filter(trip_id == trip_id_alvo)

  if (nrow(trips_linha) == 0) {
    stop("Nenhum trip em gtfs$trips para linha ", linha_c,
         " e direction_id ", direction_id, ".")
  }

  shape_pts <- gtfs$shapes %>%
    filter(shape_id == trips_linha$shape_id) %>%
    arrange(shape_pt_sequence) %>%
    st_as_sf(coords = c("shape_pt_lon", "shape_pt_lat"), crs = 4326) %>%
    st_transform(31983)

  # Construir LINESTRING representando o trajeto físico da viagem
  route_line <- shape_pts %>%
    summarise(geometry = st_combine(geometry)) %>%
    st_cast("LINESTRING")
  # mapview(route_line)

  # -------------------------------------------------------------------
  # 3.3 Projetar paradas na linha de rota (map‑matching das paradas)
  # -------------------------------------------------------------------

  paradas_linha <- paradas_linha %>%
    mutate(
      stop_dist = line_project_distance_m(route_line, geometry)
    )

  # rota <-
  list(
    paradas_linha = paradas_linha,
    route_line = route_line,
    trip_id_alvo = trip_id_alvo
  )
}

## ------------------------------------------------------------------
## 4. Preparar GPS (sf) para uma linha/direção em um dia
## ------------------------------------------------------------------

preparar_gps_linha <- function(posicoes_dia,
                               linha_c,
                               sentido_sl,
                               route_line) {
  # 1) Filtrar apenas a linha/sentido desejados neste dia.
  # linha_c <- linha_foco
  gps <- posicoes_dia %>%
    filter(c == linha_c, sl == sentido_sl) %>%
    arrange(c, p, ta) %>%
    # 2) Converter para sf em WGS84 e, em seguida, para um CRS métrico (UTM 23S),
    #    para que distâncias sejam em metros.
    st_as_sf(coords = c("px", "py"), crs = 4326, remove = FALSE) %>%
    st_transform(31983)
  # mapview(gps)

  # Começo de rota sentido terminal secundário > principal, para referência
  # gps %>% filter(p == '52047' & ta > ymd_hm('2026-03-04 11:00')) %>% head(20) %>% select(py, px) %>% mutate(py = as.character(py), px = as.character(px))
  gps <- gps %>% filter(p == '52047' & ta > ymd_hm('2026-03-04 11:00'))
  route_line <- rota$route_line

  # # Linha da rota está começando e terminando onde?
  # coords <- st_coordinates(route_line)
  # start_pt <- coords[1, c("X","Y")]
  # end_pt   <- coords[nrow(coords), c("X","Y")]
  # plot(st_geometry(route_line))
  # points(start_pt["X"], start_pt["Y"], col="red", pch=16)
  # points(end_pt["X"], end_pt["Y"], col="blue", pch=16)

  # 3) Projetar cada ponto no eixo 1D da linha:
  # - `gps_dist` é a distância acumulada (em metros) ao longo do trajeto
  #    onde o ponto é projetado.
  # - Se o ônibus estiver além do último vértice da linha, o valor é
  #   saturado em ~comprimento total da linha (todos os pontos ficam
  #   muito próximos de route_length).
  gps <- gps %>%
    mutate(
      gps_dist = line_project_distance_m(route_line, geometry)
    ) %>%
    arrange(c, sl, p, ta) %>%
    group_by(c, sl, p) %>%
    mutate(
      # gps_dist = cummax(gps_dist),
      # primeiro ponto do veículo ganha um "segmento" 0 -> gps_dist
      # (permite cruzar as primeiras paradas)
      prev_dist = dplyr::lag(gps_dist, default = 0),
      prev_time = dplyr::lag(ta, default = first(ta)),
      # dx = lead(st_coordinates(geometry)[, 1]) - st_coordinates(geometry)[, 1],
      # dy = lead(st_coordinates(geometry)[, 2]) - st_coordinates(geometry)[, 2],
      # heading = (atan2(dy, dx) * 180 / pi) %% 360
    ) %>%
    ungroup()

  # gps_sf <-
  gps
}

## ------------------------------------------------------------------
## 5. Estimar hora de passagem em cada parada (linha/direção)
## ------------------------------------------------------------------

estimar_passagens_paradas <- function(gps_sf,
                                      paradas_linha_sf) {

  # GPS sem geometria (cada linha = ponto, mas com prev_dist/prev_time já calculados)
  gps_dt <- gps_sf %>%
    st_drop_geometry() %>%
    as.data.table()
  tibble(gps_dt) %>% select(gps_dist, prev_dist)

  # Segmentos válidos: precisa ter movimento ao longo da rota (senão divide por 0)
  gps_seg <- gps_dt[
    !is.na(prev_dist) &
      is.finite(prev_dist) &
      is.finite(gps_dist) &
      gps_dist > prev_dist &
      !is.na(prev_time) &
      !is.na(ta)
  ]
  tibble(gps_seg) %>% select(prev_dist, gps_dist)

  # Paradas sem geometria; stop_dist é a posição (m) ao longo do shape
  stops_dt <- paradas_linha_sf %>%
    st_drop_geometry() %>%
    as.data.table()

  stops_dt <- stops_dt[!is.na(stop_dist) & is.finite(stop_dist)]

  # foverlaps faz "interval join" de forma eficiente:
  # - cada segmento do GPS é um intervalo [seg_start, seg_end]
  # - cada parada é um "intervalo degenerado" [stop_dist, stop_dist]
  gps_seg[, `:=`(seg_start = prev_dist, seg_end = gps_dist)]
  stops_dt[, `:=`(stop_start = stop_dist, stop_end = stop_dist)]



  # ------------------------------------------------------------------

  # Step 1 — ensure ordering
  setkey(gps_seg, gps_dist)
  setkey(stops_dt, stop_dist)

  # Step 2 — rolling join (next GPS point)
  # Meaning: for each stop_dist, find the first gps_dist >= stop_dist. So the
  # matched row gives the segment end.
  stop_passages <- gps_seg[
    stops_dt,
    on = .(gps_dist = stop_dist),
    roll = -Inf
  ]

  # Step 3 — get segment start
  # Since gps_seg already has prev_dist:
  stop_passages[
    ,
    `:=`(
      seg_start = prev_dist,
      seg_end   = gps_dist
    )
  ]
  # Now every stop is mapped to its GPS segment.
  # Visual meaning
  # GPS distances:
  # 0 ---- 7 ---- 84 ---- 234 ---- 355 ---- 522
  #
  # Stop: 329
  #
  # Rolling join result:
  # stop -> gps_dist 355
  # segment = [234 , 355]

  # Optional: interpolate timestamp of stop passage - This is often the real
  # goal, to have estimated bus arrival time at each stop.
  stop_passages[
    ,
    frac := (stop_dist - prev_dist) / (gps_dist - prev_dist)
  ]

  stop_passages[
    ,
    stop_time := prev_time + frac * (ta - prev_time)
  ]


  # ------------------------------------------------------------------

















  # Keys nos intervalos (necessário para foverlaps)
  setkey(gps_seg, seg_start, seg_end)
  setkey(stops_dt, stop_start, stop_end)

  # Join: retorna todas as combinações (parada, segmento) onde stop_dist está no segmento
  stop_passages <- foverlaps(
    stops_dt,
    gps_seg,
    by.x = c("stop_start", "stop_end"),
    by.y = c("seg_start", "seg_end"),
    type = "within",
    nomatch = 0L
  )

  if (nrow(stop_passages) == 0L) {
    return(stop_passages[])
  }

  # Interpolação do tempo de passagem:
  # frac = (stop_dist - prev_dist) / (gps_dist - prev_dist)
  stop_passages[, frac := (stop_dist - seg_start) / (seg_end - seg_start)]
  stop_passages[, frac := pmin(1, pmax(0, frac))]
  stop_passages[, observed_time := prev_time + frac * (ta - prev_time)]

  # Uma parada pode aparecer em vários segmentos (ex.: jitter, pontos repetidos).
  # Mantemos a primeira passagem por veículo e parada.
  stop_passages <- stop_passages[
    order(c, sl, p, stop_sequence, observed_time)
  ][
    ,
    .SD[1],
    by = .(c, sl, p, stop_id)
  ]

  stop_passages[]
}



## ------------------------------------------------------------------
## 6. Estimar horários de início de cada viagem por carro
## ------------------------------------------------------------------

estimar_partidas <- function(stop_passages,
                             paradas_linha_sf) {
  # Consideramos o "primeiro" ponto da rota (menor stop_sequence)
  primeiro_stop <- paradas_linha_sf %>%
    st_drop_geometry() %>%
    arrange(stop_sequence) %>%
    slice(1) %>%
    pull(stop_id)

  partidas <- stop_passages %>%
    as.data.table() %>%
    .[stop_id == primeiro_stop] %>%
    .[order(c, sl, p, observed_time)] %>%
    .[, .(
      partida_time = observed_time
    ), by = .(c, sl, p)]

  partidas[]
}

## ------------------------------------------------------------------
## 7. Detectar buracos de GPS (problemas de transmissão)
## ------------------------------------------------------------------

detectar_buracos_gps <- function(posicoes_dia,
                                 linha_c,
                                 sentido_sl,
                                 limite_segundos = 5 * 60) {
  gaps <- posicoes_dia %>%
    filter(c == linha_c, sl == sentido_sl) %>%
    arrange(c, p, ta) %>%
    group_by(c, sl, p) %>%
    mutate(
      lag_ta = lag(ta),
      gap_sec = as.numeric(difftime(ta, lag_ta, units = "secs"))
    ) %>%
    ungroup() %>%
    filter(!is.na(gap_sec), gap_sec > limite_segundos)

  gaps
}

## ------------------------------------------------------------------
## 8. Execução da pipeline para 478P-10 / sl == 2 em 2026‑03‑04
## ------------------------------------------------------------------

executar_pipeline_478P10_sl2 <- function() {
  # 1) Ler posições do dia e limpar duplicados
  posicoes_dia <- carregar_posicoes_dia(
    data_ref = data_ref,
    pasta_parquet = pasta_parquet,
    pasta_testes = pasta_testes
  )

  # 2) Ler GTFS
  gtfs <- carregar_gtfs(pasta_gtfs)

  # 3) Construir paradas e linha da rota para a linha/direção
  rota <- montar_paradas_linha(
    gtfs = gtfs,
    linha_c = linha_foco,
    sentido_sl = sentido_sl
  )
  # mapview(rota$route_line)

  # 4) Preparar GPS (linha/direção) já com distância ao longo da rota
  gps_sf <- preparar_gps_linha(
    posicoes_dia = posicoes_dia,
    linha_c = linha_foco,
    sentido_sl = sentido_sl,
    route_line = rota$route_line
  )
  # mapview(gps_sf)
  # mapview(rota$paradas_linha)

  # 5) Estimar passagens em cada parada
  stop_passages <- estimar_passagens_paradas(
    gps_sf = gps_sf,
    paradas_linha_sf = rota$paradas_linha
  )
  # tibble(stop_passages) %>%
  #   # filter(p == 52049) %>%
  #   # group_by(stop_id) %>% tally() %>% tail(20)
  #   filter(stop_id %in% c('610016842', '6100463', '630016850', '4400566', '440016921')) %>%
  #   select(c, sl, p, stop_id, ta, relative_arrival_time:observed_time)

  # 6) Estimar horários de partida (início de cada viagem por carro)
  partidas <- estimar_partidas(
    stop_passages = stop_passages,
    paradas_linha_sf = rota$paradas_linha
  )
  # tibble(partidas) %>% arrange(partida_time) %>%
  #   mutate(dif = lead(partida_time) - partida_time)

  # 7) Detectar buracos de GPS
  gaps <- detectar_buracos_gps(
    posicoes_dia = posicoes_dia,
    linha_c = linha_foco,
    sentido_sl = sentido_sl
  )

  # 8) Gravar intermediários em pasta de testes para reuso
  dir.create(pasta_testes, showWarnings = FALSE, recursive = TRUE)

  st_write(posicoes_dia %>% st_as_sf(coords = c('px', 'py'), crs = 4326),
           file.path(pasta_testes, paste0("posicoes_todas.gpkg")),
                     driver = 'GPKG', append = FALSE, delete_layer = TRUE)
  st_write(rota$route_line,
           file.path(pasta_testes, paste0("rota_478p.gpkg")),
           driver = 'GPKG', append = FALSE, delete_layer = TRUE)

  write_parquet(
    posicoes_dia,
    file.path(pasta_testes, paste0("posicoes_", format(data_ref, "%Y%m%d"), "_", linha_foco, "_sl", sentido_sl, ".parquet"))
  )

  write_parquet(
    as_tibble(stop_passages),
    file.path(pasta_testes, paste0("stop_passages_", format(data_ref, "%Y%m%d"), "_", linha_foco, "_sl", sentido_sl, ".parquet"))
  )

  write_parquet(
    as_tibble(partidas),
    file.path(pasta_testes, paste0("partidas_", format(data_ref, "%Y%m%d"), "_", linha_foco, "_sl", sentido_sl, ".parquet"))
  )

  write_parquet(
    gaps,
    file.path(pasta_testes, paste0("gps_gaps_", format(data_ref, "%Y%m%d"), "_", linha_foco, "_sl", sentido_sl, ".parquet"))
  )

  list(
    posicoes_dia = posicoes_dia,
    paradas_linha = rota$paradas_linha,
    gps_sf = gps_sf,
    stop_passages = stop_passages,
    partidas = partidas,
    gaps = gaps
  )
}

## ------------------------------------------------------------------
## Chamada principal (descomente para rodar direto em R/RStudio)
## ------------------------------------------------------------------

# resultados <- executar_pipeline_478P10_sl2()
# str(resultados$stop_passages)
# head(resultados$partidas)
# head(resultados$gaps)

