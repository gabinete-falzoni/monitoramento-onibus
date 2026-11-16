############################################################
# 06_programacao.R — saídas programadas (página de itinerários da SPTrans)
#
# Duas fontes, que podem ser combinadas:
#  - "api": coletas de coleta_horarios_programados/coletar_linhas_sptrans.R, em
#    <coletas_api_dir>/partidas/data_coleta=AAAA-MM-DD/part-0.parquet.
#    Tipo de dia vem do JSON da SPTrans (U/S/D corretos).
#  - "navegador": raspagens antigas (script v6, chromote), em
#    <raspagens_dir>/<data_base>/partidas/<data_base>_<linha>_<sentido>_<U|S|D>.csv
#    (sep ";"; tipo de dia lido do NOME do arquivo). A tabela D dessas
#    raspagens é cópia da U e é invalidada automaticamente (D_identica_U).
#
# Escolha da tabela: para cada data de serviço x linha x sentido, entre as
# coletas que têm a linha/sentido, usa-se a mais recente com data_base <= data
# cuja tabela do tipo de dia seja válida (ou cuja ausência indique que a linha
# não opera naquele tipo de dia); se não houver, a mais próxima posterior
# (sinalizada "raspagem_posterior_a_data"); se nenhuma for válida, a mais
# próxima, levando a marca de inválida.
#
# Semântica: os horários da página são PARTIDAS do terminal de origem do
# sentido (TP no sentido 0, TS no sentido 1; nas circulares, só TP).
############################################################

ler_partidas_navegador <- function(linhas, pastas = cfg$entrada$raspagens_pastas,
                                   base = cfg$entrada$raspagens_dir) {
  rbindlist(lapply(pastas, function(pp) {
    dir_p <- file.path(base, pp, "partidas")
    fs <- list.files(dir_p, pattern = "_[01]_[USD]\\.csv$", full.names = TRUE)
    lin_f <- sub("^[^_]+_(.+)_[01]_[USD]\\.csv$", "\\1", basename(fs))
    fs <- fs[lin_f %in% linhas]
    rbindlist(lapply(fs, function(f) {
      if (file.size(f) == 0) return(NULL)
      m <- regmatches(basename(f), regexec("^(.+?)_(.+)_([01])_([USD])\\.csv$", basename(f)))[[1]]
      d <- fread(f, sep = ";", colClasses = "character")
      if (!nrow(d)) return(NULL)
      data.table(data_base = as.Date(m[2]), linha = m[3], sentido = as.integer(m[4]),
                 tipo_dia = m[5], hora = d$hora, arquivo = basename(f), fonte = "navegador")
    }))
  }))
}

ler_partidas_api <- function(linhas, dir = cfg$entrada$coletas_api_dir,
                             datas = cfg$entrada$coletas_api_datas) {
  dp <- file.path(dir, "partidas")
  if (is.null(dir) || !dir.exists(dp)) return(data.table())
  disp <- sub("^data_coleta=", "", list.files(dp, pattern = "^data_coleta="))
  if (!is.null(datas)) disp <- intersect(disp, as.character(datas))
  rbindlist(lapply(disp, function(dc) {
    f <- file.path(dp, paste0("data_coleta=", dc), "part-0.parquet")
    d <- as.data.table(arrow::read_parquet(f, col_select = c("codigo", "sentido", "tipo_dia", "ordem", "hora")))
    d <- d[codigo %in% linhas]
    if (!nrow(d)) return(NULL)
    setorder(d, codigo, sentido, tipo_dia, ordem)
    d[, .(data_base = as.Date(dc), linha = codigo, sentido = as.integer(sentido), tipo_dia, hora,
          arquivo = paste0("api/partidas/data_coleta=", dc), fonte = "api")]
  }))
}

# Todas as fontes configuradas. Mantém o nome antigo (usado em run_pipeline.R).
ler_partidas_raspagem <- function(linhas) {
  pt <- rbindlist(list(ler_partidas_api(linhas), ler_partidas_navegador(linhas)), use.names = TRUE)
  if (nrow(pt) && pt[, uniqueN(fonte), by = data_base][V1 > 1, .N])
    stop("Há coleta da API e raspagem do navegador com a mesma data; remova uma delas da configuração.")
  pt
}

tipo_dia <- function(datas, feriados = cfg$ipp$feriados) {
  wd <- as.integer(format(datas, "%u"))
  fcase(datas %in% feriados | wd == 7L, "D", wd == 6L, "S", default = "U")
}

# Qualidade de cada tabela (linha x sentido x tipo x coleta)
qualidade_tabelas <- function(pt) {
  if (!"fonte" %in% names(pt)) pt <- copy(pt)[, fonte := "navegador"]
  q <- pt[, .(n = .N, assinatura = paste(sort(hora), collapse = ",")),
          by = .(data_base, fonte, linha, sentido, tipo_dia)]
  u <- q[tipo_dia == "U", .(data_base, linha, sentido, assin_U = assinatura)]
  q <- u[q, on = .(data_base, linha, sentido)]
  q[, schedule_quality_flag := fcase(
    tipo_dia %in% cfg$ipp$tipos_dia_invalidos, "tipo_dia_invalido_config",
    # só na fonte "navegador": na API, D == U ocorre legitimamente (linhas com a
    # mesma programação nos três dias)
    cfg$ipp$detectar_D_igual_U & fonte == "navegador" & tipo_dia == "D" &
      !is.na(assin_U) & assinatura == assin_U, "D_identica_U",
    default = "ok")]
  q[, c("assinatura", "assin_U", "fonte") := NULL]
  q[]
}

# Saídas programadas normalizadas para um conjunto de datas de serviço
montar_programacao <- function(linhas, datas, pt = NULL) {
  if (is.null(pt)) pt <- ler_partidas_raspagem(linhas)
  if (!nrow(pt)) return(data.table())
  qt <- qualidade_tabelas(pt)
  esc <- data.table(service_date = datas, tipo = tipo_dia(datas))
  # candidatas: toda coleta que traz a linha x sentido, para cada data de serviço
  ls <- unique(pt[, .(data_base, linha, sentido)])
  cand <- esc[, .(k = 1L, service_date, tipo)][ls[, k := 1L], on = "k", allow.cartesian = TRUE][, k := NULL]
  cand <- qt[, .(data_base, linha, sentido, tipo_dia, schedule_quality_flag)][
    cand, on = .(data_base, linha, sentido, tipo_dia = tipo)]
  # sem tabela para o tipo de dia numa coleta que tem a linha = não opera (válido)
  cand[, valida := is.na(schedule_quality_flag) | schedule_quality_flag == "ok"]
  cand[, posterior := data_base > service_date]
  cand[, dist := abs(as.numeric(service_date - data_base))]
  setorder(cand, linha, sentido, service_date, -valida, posterior, dist)
  escolha <- cand[, .SD[1], by = .(linha, sentido, service_date)]
  escolha[, fonte_flag := fifelse(posterior, "raspagem_posterior_a_data", "ok")]
  prog <- escolha[, .(service_date, tipo = tipo_dia, data_base, fonte_flag, linha, sentido,
                      schedule_quality_flag)]
  prog <- pt[, !"fonte"][prog, on = .(data_base, linha, sentido, tipo_dia = tipo),
                         allow.cartesian = TRUE, nomatch = NA]
  prog[is.na(hora), schedule_quality_flag := "sem_tabela_para_tipo_dia"]
  # horário -> instante local (horas < corte pertencem ao dia de serviço)
  hh <- as.integer(substr(prog$hora, 1, 2)); mm <- as.integer(substr(prog$hora, 4, 5))
  seg <- (hh + fifelse(hh < cfg$hora_corte_dia, 24L, 0L)) * 3600 + mm * 60
  base_local <- as.POSIXct(paste(prog$service_date, "00:00:00"), tz = cfg$tz_local)
  prog[, scheduled_departure_utc := lubridate::with_tz(base_local + seg, "UTC")]
  prog[, scheduled_departure_local := format(scheduled_departure_utc, "%Y-%m-%d %H:%M:%S", tz = cfg$tz_local)]
  prog[, schedule_quality_flag := fifelse(schedule_quality_flag == "ok" & fonte_flag != "ok",
                                          fonte_flag, schedule_quality_flag)]
  prog[, schedule_valid := schedule_quality_flag %in% c("ok", "raspagem_posterior_a_data")]
  setorder(prog, linha, service_date, sentido, scheduled_departure_utc)
  prog[, scheduled_trip_id := fifelse(is.na(hora), NA_character_,
                                      paste(linha, format(service_date, "%Y%m%d"), sentido,
                                            gsub(":", "", hora), sep = "_"))]
  prog[!is.na(scheduled_trip_id), scheduled_trip_id := make.unique(scheduled_trip_id, sep = "_")]
  prog[, .(scheduled_trip_id, service_date, line_id = linha, direction_id = sentido,
           tipo_dia, scheduled_departure_local, scheduled_departure_utc,
           scheduled_arrival_local = NA_character_,
           schedule_source = paste0("raspagem_sptrans_", data_base, ":", arquivo),
           schedule_quality_flag, schedule_valid, fonte_data_base = data_base)]
}
