library(tidyverse)
library(tidylog)
library(arrow)
library(sf)
library(lubridate)

pasta_base  <- "/mnt/fern/Dados/projetos/2026_Monitoramento_Olho_Vivo"
pasta_parquet  <- file.path(pasta_base, "parquet_posicao")
pasta_testes <- sprintf('%s/testes', pasta_base)

lala <- list.files(pasta_parquet, pattern = '.parquet', full.names = TRUE, recursive = FALSE)
lala <- open_dataset(lala)

posicoes <- lala %>% select(-timestamp) %>% collect()
posicoes <- posicoes %>% distinct()

ver_leaflet <- function(df) {

  df <- df %>%
    mutate(row_id = row_number())

  pal <- colorNumeric(
    palette = "viridis",
    domain  = df$row_id
  )

  leaflet(df) %>%
    addProviderTiles("CartoDB.Positron", group = "Positron") %>%
    addProviderTiles("OpenStreetMap", group = "OSM") %>%
    addCircleMarkers(
      radius = 3,
      color = ~pal(row_id),
      fillOpacity = 0.8,
      stroke = FALSE
    ) %>%
    addLegend(
      position = "bottomright",
      pal      = pal,
      values   = ~row_id,
      title    = "Row order",
      labFormat = labelFormat(transform = function(x) round(x))
    ) %>%
    addLayersControl(
      baseGroups = c("Positron", "OSM"),
      options = layersControlOptions(collapsed = FALSE)
    )
}

# posicoes %>% filter(c == '478P-10' & p == 52400) %>% st_as_sf(coords = c('px', 'py'), crs = 4326) %>% ver_leaflet()
#
# # Quais são os veículos?
# posicoes %>% filter(c == '478P-10') %>% group_by(p) %>% tally()
# posicoes %>% filter(c == '478P-10' & p == 52049) %>% st_as_sf(coords = c('px', 'py'), crs = 4326) %>% ver_leaflet()
#
# posicoes_geo <- posicoes %>%  filter(c == '478P-10') %>% st_as_sf(coords = c('px', 'py'), crs = 4326)
# st_write(posicoes_geo, sprintf('%s/posicoes_478p10.gpkg', pasta_testes), driver = 'GPKG', append = FALSE, delete_layer = TRUE)


# ------------------------------------------------------------------------------
# Estimativa de passagem dos ônibus nos pontos
# ------------------------------------------------------------------------------

posicoes <- posicoes %>% filter(c == '478P-10' & p == 52049) %>% arrange(ta)
paradas <- paradas_geo_out %>% filter(trip_id == '478P-10-1')
shape <- shape_geo %>% filter(shape_id == '79295')


posicoes <- posicoes %>% st_as_sf(coords = c('px', 'py'), crs = 4326) %>% st_transform(31983)
paradas <- paradas_geo_out %>% filter(trip_id == '478P-10-1') %>% st_transform(31983)


# Distância até o ponto seguinte e headway
gps <- posicoes %>%
  # head(20) %>%
  mutate(
    dist_next_gps_point = as.numeric(st_distance(geometry, lead(geometry), by_element = TRUE)),
    # Calcular direção do ônibus (em graus)
    dx = lead(st_coordinates(geometry)[,1]) - st_coordinates(geometry)[,1],
    dy = lead(st_coordinates(geometry)[,2]) - st_coordinates(geometry)[,2],
    # heading is bus movement from this point to the next - Last row will have NA
    heading = (atan2(dy, dx) * 180 / pi) %% 360
  ) %>%
  select(-dx, -dy) %>%
  mutate(
    # Index no shape de paradas de qual parada está mais perto do ponto
    idx = st_nearest_feature(., paradas),
    # A partir do index, qual a distância até ele?
    dist_nearest_stop = as.numeric(st_distance(., paradas[idx,], by_element = TRUE)),
    # Qual é este ponto?
    nearest_stop_id = paradas$stop_id[idx]
    ) %>%
  select(-idx)

# Juntar geometria dos pontos ao mesmo shape
idx <- match(gps$nearest_stop_id, paradas$stop_id)
gps$stop_geometry <- paradas$geometry[idx]

# Calcular direção dos pontos GPS frente aos pontos mais próximos
gps <- gps %>%
  mutate(
    # Direção com relação ao ponto
    dx_stop = st_coordinates(stop_geometry)[,1] - st_coordinates(geometry)[,1],
    dy_stop = st_coordinates(stop_geometry)[,2] - st_coordinates(geometry)[,2],
    bearing_to_stop = (atan2(dy_stop, dx_stop) * 180 / pi) %% 360
  ) %>%
  select(-c(stop_geometry, dx_stop, dy_stop)) %>%
  # angle_diff_stop	Meaning
  # ≈ 0°	Bus moving directly toward stop
  # < 45°	Strongly approaching
  # < 90°	Stop ahead
  # > 90°	Stop behind
  # ≈ 180°	Moving directly away
  mutate(
    angle_diff = abs(heading - bearing_to_stop) %% 360,
    angle_diff_stop = ifelse(angle_diff > 180, 360 - angle_diff, angle_diff)
  ) %>%
  select(-angle_diff)


# Estimar tempo que o ônibus passou no ponto
gps %>%
  # head(20) %>% select(5:ncol(.)) %>%
  mutate(ta = ymd_hms(ta),
         tempo_entre_pontos = lead(ta) - ta,
         xxxx = dist_nearest_stop + lead(dist_nearest_stop),
         prop_x_y = tempo_entre_pontos * dist_nearest_stop / xxxx,
         posicao_ponto = ifelse(between(angle_diff_stop, 0, 90), 'antes', 'depois'),
         tempo_ponto_estimado = if_else(posicao_ponto == 'antes' & lead(posicao_ponto) == 'depois', ta + seconds(prop_x_y), as_datetime(NA_real_)),
         # tempo_ponto = ymd_hms(tempo_ponto)
         ) %>%
  st_drop_geometry() %>%
  # head(20)
  select(nearest_stop_id, ta, tempo_ponto_estimado) %>%
  mutate(
    margem_antes = tempo_ponto_estimado - ta,
    margem_depois = lead(ta) - tempo_ponto_estimado
  ) %>%
  filter(!is.na(tempo_ponto_estimado)) %>%
  mutate(
    tempo_entre_pontos = lead(tempo_ponto_estimado) - tempo_ponto_estimado
  )
# nearest_stop_id ta                  tempo_ponto_estimado margem_antes   margem_depois  tempo_entre_pontos
# <chr>           <dttm>              <dttm>               <drtn>         <drtn>         <drtn>
# 1 610016842       2026-03-02 19:38:20 2026-03-02 19:38:36  16.483512 secs 28.516488 secs 3.864806 mins
# 2 6100463         2026-03-02 19:42:01 2026-03-02 19:42:28  27.371850 secs 17.628150 secs 3.168296 mins
# 3 630016850       2026-03-02 19:44:58 2026-03-02 19:45:38  40.469611 secs  3.530389 secs 2.211064 mins
# 4 4400566         2026-03-02 19:47:11 2026-03-02 19:47:51  40.133423 secs  3.866577 secs 5.984523 mins
# 5 440016921       2026-03-02 19:53:08 2026-03-02 19:53:50  42.204778 secs  7.795222 secs 5.790362 mins
# 6 440016923       2026-03-02 19:59:35 2026-03-02 19:59:37   2.626509 secs  2.373491 secs 3.872964 mins
# 7 920016731       2026-03-02 20:03:16 2026-03-02 20:03:30  14.004340 secs  8.995660 secs       NA mins





























# ------------------------------------------------------------------------------
# Sandbox
# ------------------------------------------------------------------------------



x <- posicoes %>% st_as_sf(coords = c('px', 'py'), crs = 4326) %>% st_transform(31983)
y <- st_transform(this, 31983)
#
# idx <- st_nearest_feature(x, y)
#
# x %>%
#   mutate(trip_id = str_c(c, sl - 1, sep = '-')) %>%
#   filter(trip_id == '478P-10-1' & p == 52049) %>%
#   mutate(
#     nearest_id = y$stop_id[idx],
#     dist_m = as.numeric(st_distance(x, y[idx, ], by_element = TRUE))
#     # dist_m = as.numeric(st_distance(geometry, lag(geometry), by_element = TRUE))
#     # dist_m = as.numeric(st_distance(this, that[idx, ], by_element = TRUE))
#   )
#   st_join(this, join = st_nearest_feature)


x_sub <- x %>%
  mutate(trip_id = str_c(c, sl - 1, sep = '-')) %>%
  filter(trip_id == "478P-10-1", p == 52049)

y_sub <- y %>%
  filter(trip_id == "478P-10-1")

idx <- st_nearest_feature(x_sub, y_sub)

x_sub <- x_sub %>%
  mutate(
    nearest_stop_id = y_sub$stop_id[idx],
    dist_m = as.numeric(
      st_distance(geometry, y_sub$geometry[idx], by_element = TRUE)
    )
  )

x_sub <- x_sub %>%
  mutate(idx = st_nearest_feature(., y_sub),
         dist_m = as.numeric(st_distance(., y_sub[idx,], by_element = TRUE)),
         nearest_stop_id = y_sub$stop_id[idx]) %>%
  select(-idx)



# ------------------------------------------------------------------------------
# Estimativa de passagem dos ônibus nos pontos
# ------------------------------------------------------------------------------


x <- posicoes %>% st_as_sf(coords = c('px', 'py'), crs = 4326) %>% st_transform(31983)
this <- paradas_geo_out %>% filter(trip_id == '478P-10-1')
y <- st_transform(this, 31983)

gps <- x %>%
  mutate(
    trip_id = str_c(c, sl - 1, sep = "-"),
    ta = ymd_hms(ta)
  ) %>%
  filter(trip_id == "478P-10-1", p == 52049) %>%
  arrange(ta)

stops <- y %>%
  filter(trip_id == "478P-10-1") %>%
  arrange(stop_sequence)

# Compute the bus heading
gps <- gps %>%
  mutate(idx = st_nearest_feature(., stops),
         dist_m_nearest_stop = as.numeric(st_distance(., stops[idx,], by_element = TRUE)),
         nearest_stop_id = stops$stop_id[idx]) %>%
  select(-c(idx, trip_id)) %>%
  # head(20) %>%
  mutate(
    dx = lead(st_coordinates(geometry)[,1]) - st_coordinates(geometry)[,1],
    dy = lead(st_coordinates(geometry)[,2]) - st_coordinates(geometry)[,2],
    # heading is bus movement from this point to the next - Last row will have NA
    heading = (atan2(dy, dx) * 180 / pi) %% 360
  )

# Compute bearing to each stop
gps_coords <- st_coordinates(gps)
stops_coords <- st_coordinates(stops$geometry)

# Create a matrix of differences
dx_mat <- outer(stops_coords[,1], gps_coords[,1], FUN = "-")  # stop x - gps x
dy_mat <- outer(stops_coords[,2], gps_coords[,2], FUN = "-")  # stop y - gps y

# Compute bearings (degrees)
stop_bearing_mat <- (atan2(dy_mat, dx_mat) * 180 / pi) %% 360


# Compute angle difference with heading
# Repeat heading to match stops
heading_mat <- matrix(rep(gps$heading, each = nrow(stops)), nrow = nrow(stops))

angle_diff_mat <- abs(heading_mat - stop_bearing_mat) %% 360
angle_diff_mat <- ifelse(angle_diff_mat > 180, 360 - angle_diff_mat, angle_diff_mat)

#Filter only stops ahead
# Boolean: moving toward stop (e.g., within 90°)
moving_toward <- angle_diff_mat <= 90

# Combine with nearest stop distance
# For each GPS point, consider only stops that are ahead
dist_mat <- as.matrix(st_distance(gps$geometry, stops$geometry))  # meters

dist_mat[!moving_toward] <- NA  # ignore stops behind the bus

# Find nearest “ahead” stop per GPS point
nearest_stop_idx <- apply(dist_mat, 1, function(x) which.min(x))

gps$nearest_stop_id <- stops$stop_id[nearest_stop_idx]
gps$dist_m_nearest_stop <- dist_mat[cbind(1:nrow(gps), nearest_stop_idx)]

gps %>% st_drop_geometry() %>% select(ta, 6:ncol(.))







gps <- gps %>%
  filter()
  arrange(ta) %>%
  mutate(
    dx = lead(st_coordinates(geometry)[,1]) - st_coordinates(geometry)[,1],
    dy = lead(st_coordinates(geometry)[,2]) - st_coordinates(geometry)[,2],
    heading = (atan2(dy, dx) * 180 / pi) %% 360
  )

# Compute distance between consecutive GPS points - Now each row represents a
# segment to the next GPS point.
gps <- gps %>%
  mutate(
    dist_next = as.numeric(st_distance(geometry, lead(geometry), by_element = TRUE)),
    time_next = as.numeric(difftime(lead(ta), ta, units = "secs"))
  )

# For Each Stop, Find Nearest Segment - Instead of nearest point, we want nearest segment.
gps_segments <- gps %>%
  filter(!is.na(dist_next)) %>%
  mutate(
    geom_next = lead(geometry),
    segment = st_cast(st_union(geometry, geom_next), "LINESTRING")
  ) %>%
  st_as_sf()

# Now each row = one movement segment.
segments <- st_sf(
  ta_start = gps$ta[-nrow(gps)],
  ta_end   = gps$ta[-1],
  geom = st_sfc(
    mapply(function(p1, p2)
      st_linestring(rbind(st_coordinates(p1),
                          st_coordinates(p2))),
      gps$geometry[-nrow(gps)],
      gps$geometry[-1],
      SIMPLIFY = FALSE),
    crs = st_crs(gps)
  )
)

# Find Nearest Segment for Each Stop
idx <- st_nearest_feature(stops, segments)
stops$seg_id <- idx

# Interpolate Time Using Distance Proportion
results <- stops %>%
  rowwise() %>%
  mutate(
    seg_geom = segments$geom[seg_id],

    # fraction along the segment (0 to 1)
    prop = as.numeric(
      st_line_locate_point(seg_geom, geometry)
    ),

    # interpolate timestamp
    est_time = segments$ta_start[seg_id] +
      prop * as.numeric(
        difftime(segments$ta_end[seg_id],
                 segments$ta_start[seg_id],
                 units = "secs")
      )
  ) %>%
  ungroup()
