# Associar tipos de ônibus ao GTFS, via prefixo do veículo

library(tidyverse)
library(tidylog)
library(arrow)
library(readxl)
library(janitor)
library(sf)
# library(lubridate)

mes <- '202608'
pasta_base <- '/home/livre/zz_olho_vivo_parquet_gabinete'
pasta_gabi <- file.path(pasta_base, mes)
pasta_gtfs <- '/mnt/fern/Dados/projetos/2026_Monitoramento_Olho_Vivo/cittamobi_gtfs'


# Coleta dos GPS dos ônibus pelo Olho Vivo - Agosto 2026
arqs_gps <- list.files(pasta_gabi, pattern = '\\.parquet$', full.names = TRUE)
# # A tibble: 4,610 × 7
# c          cl    sl     p ta                      py    px
# <chr>   <int> <int> <int> <chr>                <dbl> <dbl>
# 1 407H-10 34965     2 48015 2026-08-01T02:59:40Z -23.6 -46.5
# 2 407H-10 34965     2 48806 2026-08-01T02:59:48Z -23.6 -46.5
# 3 8549-10   580     1 11785 2026-08-01T02:59:31Z -23.5 -46.7
# 4 8549-10   580     1 11789 2026-08-01T02:59:53Z -23.5 -46.7
# 5 8549-10   580     1 11784 2026-08-01T03:00:07Z -23.5 -46.6
# 6 8549-10   580     1 11744 2026-08-01T02:59:55Z -23.5 -46.7
# 7 2678-10   383     1 32143 2026-08-01T03:00:02Z -23.5 -46.6
# 8 2678-10   383     1 32193 2026-08-01T02:59:56Z -23.5 -46.4
# 9 2678-10   383     1 32520 2026-08-01T02:59:49Z -23.5 -46.4
# 10 2678-10   383     1 32501 2026-08-01T02:59:58Z -23.5 -46.5

arqs_gps <- open_dataset(arqs_gps) %>% select(p, c) %>% distinct() %>% collect()
arqs_gps <- arqs_gps %>% arrange(p) %>% mutate(p = str_trim(as.character(p)))


# Arquivo businfo - https://businfo.app.br/veiculos/dados-estaticos
businfo <- '/mnt/fern/Dados/projetos/2026_Monitoramento_Olho_Vivo/businfo/00_businfo_consolidado.csv'
businfo <- read_delim(businfo, delim = ';', col_types = cols(.default = "c"))
businfo <- businfo %>% mutate(p = str_extract(chave, '\\d+'), .before = 1)

# Dados businfo + linhas vindas do GPS
bus_linhas <- businfo %>% full_join(arqs_gps, by = 'p')


# # Frota por tipo de ônibus - LAI Drummond
# frota <- '/home/livre/zz_olho_vivo_parquet_gabinete/SPTrans - Frota cadastrada 2026-04-10.XLSX'
# frota <- read_excel(frota, skip = 1) %>% clean_names()
# frota <- frota %>% mutate(prefixo = str_trim(as.character(prefixo)))
# businfo_frota <- businfo %>% full_join(frota, by = c('p' = 'prefixo'))
# out_bus_linhas <- file.path(pasta_base, 'frota_cadastrada_businfo.csv')
# write_delim(businfo_frota, out_bus_linhas, delim = ';')


# GTFS
trip <- sprintf('%s/trips.txt', pasta_gtfs)
trip <- read_delim(trip, delim = ',', col_types = 'ccccic')
trip <- trip %>% select(c = route_id, shape_id)

shape <- sprintf('%s/shapes.txt', pasta_gtfs)
shape <- read_delim(shape, delim = ',', col_types = 'cddid')
shape <- shape %>% select(shape_id, shape_pt_lat, shape_pt_lon, shape_pt_sequence)


articulados_sp <- bus_linhas %>% filter(str_detect(desc_tipo, 'rticul') & cidade == 'SP')

bus_linhas %>% select(desc_tipo) %>% arrange(desc_tipo) %>% distinct() %>% tail(20)

shape_linhas_onibus <- articulados %>%
  # mutate(c = str_sub(c, 5, 7)) %>% select(c) %>% distinct()
  left_join(trip, by = 'c') %>%
  filter(!is.na(shape_id)) %>%
  left_join(shape, by = 'shape_id')

shape_out <- shape_linhas_onibus %>%
  arrange(c, shape_id, shape_pt_sequence) %>%
  group_by(c, shape_id) %>%
  summarise(
    p = first(p),
    # chave = first(chave),
    # cidade = first(cidade),
    # climatizado = first(climatizado),
    desc_tipo = first(desc_tipo),
    placa = first(placa),
    tipo = first(tipo),
    veiculo = first(veiculo),
    wkt = paste0(
      "LINESTRING (",
      paste(
        paste(shape_pt_lon, shape_pt_lat),
        collapse = ", "
      ),
      ")"
    ),
    .groups = "drop"
  ) %>%
  st_as_sf(
    wkt = "wkt",
    crs = 4326
  )

# shape_out <- shape_linhas_onibus %>%
#   arrange(c, shape_id, shape_pt_sequence) %>%
#   group_by(c, shape_id) %>%
#   summarise(
#     p = first(p),
#     tecnologia = first(tecnologia),
#     wkt = paste0(
#       "LINESTRING (",
#       paste(
#         paste(shape_pt_lon, shape_pt_lat),
#         collapse = ", "
#       ),
#       ")"
#     ),
#     .groups = "drop"
#   ) %>%
#   st_as_sf(
#     wkt = "wkt",
#     crs = 4326
#   )

st_write(shape_out, file.path(pasta_base, 'shape_linhas_onibus.gpkg'), driver = 'GPKG', append = FALSE, delete_layer = TRUE)
