library(tidyverse)
library(tidylog)
library(arrow)
# library(lubridate)

# pasta_base <- '/media/livre/Expansion/00_monitoramento_olho_vivo_parquet_all'
# pasta_gabi <- file.path(pasta_base, 'comp_gabinete')
# pasta_casa <- file.path(pasta_base, 'comp_casa')
# pasta_consolidado <- file.path(pasta_base, 'parquets_consolidados')
# dir.create(pasta_consolidado, showWarnings = FALSE, recursive = TRUE)


mes <- '202610'
pasta_base <- '/home/livre/zz_olho_vivo_parquet_gabinete'
pasta_gabi <- file.path(pasta_base, mes)
# pasta_casa <- file.path(pasta_base, 'c_', mes)
pasta_casa <- file.path('/mnt/fern/Dados/projetos/2026_Monitoramento_Olho_Vivo/collect_gps_onibus/bkps_coletas', mes)
pasta_consolidado <- '/mnt/fern/Dados/projetos/2026_Monitoramento_Olho_Vivo/collect_gps_onibus/parquets_consolidados'


# pastas_meses_gabi <- basename(list.dirs(pasta_gabi))[2:length(list.dirs(pasta_gabi))]
# pastas_meses_casa <- basename(list.dirs(pasta_casa))[2:length(list.dirs(pasta_casa))]


data_inicio <- as.Date(paste0(mes, "01"), format = "%Y%m%d")
data_final <- seq(data_inicio, length = 2, by = "1 month")[2] - 1
dias <- seq(data_inicio, data_final, by = "day")

for (dia in as.character(dias)) {
# for (dia in as.character(dias)[4:length(dias)]) {
  # dia <- as.character(dias[1])
  print(dia)

  out_file <- file.path(pasta_consolidado, sprintf('%s.parquet', dia))

  if (!file.exists(out_file)) {
    # arqs_gabi <- list.files(file.path(pasta_gabi, mes), pattern = str_c('^', str_remove_all(dia, '-'), sep = ''), full.names = TRUE)
    arqs_gabi <- list.files(pasta_gabi, pattern = str_c('^', str_remove_all(dia, '-'), sep = ''), full.names = TRUE)
    if (length(arqs_gabi) > 0) {
      arqs_gabi <- open_dataset(arqs_gabi) %>% collect()
    }

    # arqs_casa <- list.files(file.path(pasta_casa, mes), pattern = str_c('^', str_remove_all(dia, '-'), sep = ''), full.names = TRUE)
    arqs_casa <- list.files(pasta_casa, pattern = str_c('^', str_remove_all(dia, '-'), sep = ''), full.names = TRUE)
    if (length(arqs_casa) > 0) {
      arqs_casa <- open_dataset(arqs_casa) %>% collect()
    }

    if (length(arqs_gabi) > 0 & length(arqs_casa) > 0) {
      arqs_ambos <- bind_rows(arqs_gabi, arqs_casa) %>% arrange(ta, c) %>% distinct()
    } else if (length(arqs_gabi) > 0 & length(arqs_casa) == 0) {
      arqs_ambos <- arqs_gabi %>% arrange(ta, c) %>% distinct()
    } else if (length(arqs_gabi) == 0 & length(arqs_casa) > 0) {
      arqs_ambos <- arqs_casa %>% arrange(ta, c) %>% distinct()
    }

    if (exists('arqs_ambos')) {
      write_parquet(arqs_ambos, out_file)
    }
    rm(arqs_gabi, arqs_casa, arqs_ambos)
    gc(T)
  }
}


# arrow::write_dataset(
#   partidas,
#   path = file.path(pasta_base, 'linhas_collect/data/partidas'),
#   format = "parquet",
#   # data, c
#   partitioning = c("data_base", "linha", "sentido", "dia_tipo")
# )






# dias_gabi <- data.frame(this = list.files(pasta_gabi, pattern = '.parquet$'))
# dias_gabi <- dias_gabi %>% mutate(this = str_sub(this, 1, 8)) %>% distinct()
#
# dias_casa <- data.frame(this = list.files(pasta_casa, pattern = '.parquet$'))
# dias_casa <- dias_casa %>% mutate(this = str_sub(this, 1, 8)) %>% distinct()
#
# dias <- bind_rows(dias_gabi, dias_casa) %>% group_by(this) %>% tally() %>% ungroup()
# if (tail(dias, 1)$n == 1) {
#   dias <- dias %>% slice(1:nrow(dias) - 1)
# }
# rm(dias_casa, dias_gabi)
#
# for (dia in dias$this) {
#   # dia <- dias$this[10]
#   print(dia)
#
#   out_file <- file.path(pasta_consolidado, sprintf('%s.parquet', dia))
#
#   if (!file.exists(out_file)) {
#     arqs_gabi <- list.files(pasta_gabi, pattern = str_c('^', dia, sep = ''), full.names = TRUE)
#     arqs_gabi <- open_dataset(arqs_gabi) %>% collect()
#     arqs_gabi <- arqs_gabi %>% mutate(ta = ymd_hms(ta))
#
#     arqs_casa <- list.files(pasta_casa, pattern = str_c('^', dia, sep = ''), full.names = TRUE)
#     if (length(arqs_casa) > 0) {
#       arqs_casa <- open_dataset(arqs_casa) %>% collect()
#       arqs_casa <- arqs_casa %>% mutate(ta = ymd_hms(ta))
#       arqs_ambos <- bind_rows(arqs_gabi, arqs_casa) %>% distinct()
#     } else {
#       arqs_ambos <- arqs_gabi %>% distinct()
#     }
#
#     write_parquet(arqs_ambos, out_file)
#     rm(arqs_gabi, arqs_casa, arqs_ambos)
#   }
#
#
# }



