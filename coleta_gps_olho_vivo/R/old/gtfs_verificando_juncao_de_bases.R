library('tidyverse')
library('tidylog')
library('sf')
# library('mapview')
library('leaflet')

pasta_base <- '/mnt/fern/Dados/projetos/2026_Monitoramento_Olho_Vivo/'
pasta_gtfs <- sprintf('%s/cittamobi_gtfs', pasta_base)
pasta_testes <- sprintf('%s/testes', pasta_base)
dir.create(pasta_testes, recursive = TRUE, showWarnings = FALSE)


ver_leaflet <- function(df) {
  leaflet(df) %>%
    addProviderTiles("CartoDB.Positron", group = "Positron") %>%
    addProviderTiles("OpenStreetMap", group = "OSM") %>%
    addCircleMarkers(radius = 3,
                     color = "#2b8cbe",
                     fillOpacity = 0.8,
                     stroke = FALSE) %>%
    addLayersControl(
      baseGroups = c("Positron", "OSM"),
      options = layersControlOptions(collapsed = FALSE)
    )
}


bus <- '478P-10'
# bus <- '701A-10'

# Rotas - contêm as linhas de ônibus
rotas <- sprintf('%s/routes.txt', pasta_gtfs)
rotas <- read_delim(rotas, delim = ',', col_types = 'ciccicc')
rotas <- rotas %>% filter(route_id == bus)
# route_id agency_id route_short_name route_long_name     route_type route_color route_text_color
# <chr>        <int> <chr>            <chr>                    <int> <chr>       <chr>
# 1 478P-10          1 478P-10          Sacomã - Vl. Romana          3 006341      FFFFFF

# route_id agency_id route_short_name route_long_name                     route_type route_color route_text_color
# <chr>        <int> <chr>            <chr>                                    <int> <chr>       <chr>
# 1 701A-10          1 701A-10          Pq. Edu Chaves - Metrô Vl. Madalena          3 002F6C      FFFFFF

# Viagens - para cada route_id, viagens podem ser de ida ou volta, o que
# influencia no trip_id
# service_id > calendar.txt
# trip_id    > frequencies.txt
# shape_id   > shapes.txt
trip <- sprintf('%s/trips.txt', pasta_gtfs)
trip <- read_delim(trip, delim = ',', col_types = 'ccccic')
trip <- trip %>% filter(route_id == bus)
agency_id <- trip %>% filter(direction_id == 0)
# route_id service_id trip_id   trip_headsign direction_id shape_id
# <chr>    <chr>      <chr>     <chr>                <int> <chr>
# 1 478P-10  USD        478P-10-0 Vl. Romana               0 80048
# 2 478P-10  USD        478P-10-1 Sacomã                   1 79295

# route_id service_id trip_id   trip_headsign      direction_id shape_id
# <chr>    <chr>      <chr>     <chr>                     <int> <chr>
# 1 701A-10  USD        701A-10-0 Metrô Vl. Madalena            0 80407   701A-10  USD        701A-10-0 Metrô Vl. Madalena            0 80407

# Calendário
dates <- sprintf('%s/calendar.txt', pasta_gtfs)
dates <- read_delim(dates, delim = ',', col_types = 'ciiiiiiicc')
# service_id monday tuesday wednesday thursday friday saturday sunday start_date end_date
# <chr>       <int>   <int>     <int>    <int>  <int>    <int>  <int> <chr>      <chr>
# 1 USD             1       1         1        1      1        1      1 20231001   20260901
# 2 U__             1       1         1        1      1        0      0 20231001   20260901
# 3 US_             1       1         1        1      1        1      0 20231001   20260901
# 4 _SD             0       0         0        0      0        1      1 20231001   20260901
# 5 __D             0       0         0        0      0        0      1 20231001   20260901
# 6 _S_             0       0         0        0      0        1      0 20231001   20260901

# Shapes
shape <- sprintf('%s/shapes.txt', pasta_gtfs)
shape <- read_delim(shape, delim = ',', col_types = 'cddid')
# shape <- shape %>% filter(shape_id == agency_id$shape_id)
shape <- shape %>% filter(shape_id %in% trip$shape_id)
shape %>% st_as_sf(coords = c('shape_pt_lon', 'shape_pt_lat'), crs = 4326) %>% ver_leaflet()
shape_geo <- shape %>% st_as_sf(coords = c('shape_pt_lon', 'shape_pt_lat'), crs = 4326)
st_write(shape_geo, sprintf('%s/shape_478p10.gpkg', pasta_testes), driver = 'GPKG', append = FALSE, delete_layer = TRUE)
# shape_id shape_pt_lat shape_pt_lon shape_pt_sequence shape_dist_traveled
# <chr>           <dbl>        <dbl>             <int>               <dbl>
# 1 80048           -23.6        -46.6                 1                6.65
# 2 80048           -23.6        -46.6                 2               19.6
# 3 80048           -23.6        -46.6                 3               19.6

# Frequência
freq <- sprintf('%s/frequencies.txt', pasta_gtfs)
freq <- read_delim(freq, delim = ',', col_types = 'ctti')
# freq <- freq %>% filter(str_starts(trip_id, bus))
# freq <- freq %>% filter(trip_id == '478P-10-0')
freq <- freq %>% filter(trip_id == agency_id$trip_id) %>% mutate(headway_min = headway_secs / 60)
# trip_id   start_time end_time headway_secs headway_min
# <chr>     <time>     <time>          <int>       <dbl>
# 1 478P-10-0 04:00      04:59            1800          30
# 2 478P-10-0 05:00      05:59            1200          20
# 3 478P-10-0 06:00      06:59            1200          20

# trip_id   start_time end_time headway_secs headway_min
# <chr>     <time>     <time>          <int>       <dbl>
# 1 701A-10-0 04:00      04:59            1200          20
# 2 701A-10-0 05:00      05:59             900          15
# 3 701A-10-0 06:00      06:59             720          12


# Paradas (horários)
# stop_id > stops.txt
paradas <- sprintf('%s/stop_times.txt', pasta_gtfs)
paradas <- read_delim(paradas, delim = ',', col_types = 'cttci')
# Todos os departure_time são iguais aos arrival_time
# paradas %>% filter(arrival_time != departure_time)
paradas <- paradas %>% filter(trip_id == trip$trip_id)
# trip_id   arrival_time departure_time stop_id   stop_sequence
# <chr>     <time>       <time>         <chr>             <int>
# 1 478P-10-0 14:00:00     14:00:00       6905469               1
# 2 478P-10-0 14:01:52     14:01:52       6905446               2
# 3 478P-10-0 14:03:44     14:03:44       6902208               3

# trip_id   arrival_time departure_time stop_id   stop_sequence
# <chr>     <time>       <time>         <chr>             <int>
# 1 701A-10-0 14:01:58     14:01:58       3812744               2
# 2 701A-10-0 14:05:54     14:05:54       3812778               4
# 3 701A-10-0 14:09:50     14:09:50       3812820               6


# No geral, todas as paradas possuem a mesma previsão de tempo de espera, exceto
# a última, em que o tempo é maior. Os valores padrão serão guardados na coluna
# stop_interval e o da última parada na coluna last_stop_interval
intervalo_paradas <- paradas %>%
  group_by(trip_id) %>%
  mutate(stop_interval = lead(departure_time) - departure_time) %>%
  ungroup() %>%
  # slice(31:50)
  # Valores entre um grupo e outro serão NA - remover
  filter(!is.na(stop_interval)) %>%
  # Valores padrão são os de todos os pontos, exceto último, em que n = 1 no tally()
  group_by(trip_id, stop_interval) %>%
  tally() %>%
  ungroup() %>%
  mutate(this = ifelse(n == 1, 'last_stop_interval', 'stop_interval')) %>%
  pivot_wider(id_cols = trip_id,
              names_from = this,
              values_from = stop_interval) %>%
  # filter(is.na(stop_interval) | is.na(last_stop_interval)) %>%
  mutate(stop_interval = ifelse(is.na(stop_interval), last_stop_interval, stop_interval),
         last_stop_interval = ifelse(is.na(last_stop_interval), stop_interval, last_stop_interval))
# trip_id   stop_interval last_stop_interval
# <chr>             <dbl>              <dbl>
# 1 1012-10-0            78                180
# 2 1012-21-0           103                212
# 3 1014-10-0            82                182
# 4 1015-10-0           131                271
write_parquet(intervalo_paradas, sprintf('%s/intervalo_paradas.parquet', pasta_testes))


# Paradas (dados)
prds <- sprintf('%s/stops.txt', pasta_gtfs)
prds <- read_delim(prds, delim = ',', col_types = 'cccdd')
# prds %>% select(stop_desc) %>% distinct()
prds <- prds %>% filter(stop_id %in% paradas$stop_id)
prds %>% st_as_sf(coords = c('stop_lon', 'stop_lat'), crs = 4326) %>% ver_leaflet()
# stop_id stop_name                           stop_desc                                            stop_lat stop_lon
# <chr>   <chr>                               <chr>                                                   <dbl>    <dbl>
# 1 706304  Av. Paulista, 900                   Ref.: Al Campinas                                       -23.6    -46.7
# 2 1405624 R. Clímaco Barbosa, 659             Ref.: Rua Alexandrino Da Silva Bueno                    -23.6    -46.6
# 3 1410086 Av. Lacerda Franco, 282             Ref.: Rua Álvares Ribeiro                               -23.6    -46.6

paradas_geo <- paradas %>%
  select(trip_id, stop_id, stop_sequence) %>%
  left_join(subset(prds, select = c(stop_id, stop_lat, stop_lon)), by = 'stop_id')
paradas_geo_out <- paradas_geo %>% st_as_sf(coords = c('stop_lon', 'stop_lat'), crs = 4326)
st_write(paradas_geo_out, sprintf('%s/paradas_478p10.gpkg', pasta_testes), driver = 'GPKG', append = FALSE, delete_layer = TRUE)
# trip_id   stop_id  stop_sequence stop_lat stop_lon
# <chr>     <chr>            <int>    <dbl>    <dbl>
# 1 1012-10-0 301790               1    -23.4    -46.8
# 2 1012-10-0 301764               2    -23.4    -46.8
# 3 1012-10-0 301724               3    -23.4    -46.8
