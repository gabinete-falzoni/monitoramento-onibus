############################################################
# 13_icv.R — ICV, Índice de Cumprimento de Viagens (SPTrans IQT.GSO.001.05)
#
# Ficha técnica (Procedimento 3-2, Quadro 4 e anexo):
#  - viagem = deslocamento do terminal principal ao secundário (ou vice-versa);
#  - ICV calculado por faixa horária (hora) e sentido; o nº de viagens
#    realizadas fica limitado ao programado na OSO;
#  - as faixas são agrupadas nos períodos APM 03–05h, PM 06–08h, EP 09–16h,
#    PT 17–19h, PN 20–23h e PPN 00–02h;
#  - ICV diário por sentido = média dos períodos; ICV mensal = média
#    (ICV ida, ICV volta); linhas circulares sem distinção de sentido.
#
# Escolhas de implementação (a ficha não detalha):
#  - realizadas por faixa = viagens completas (confirmadas ou prováveis) da linha
#    cuja PARTIDA observada cai na hora, independentemente da associação a um
#    horário específico; limitadas a min(realizadas, programadas) na faixa;
#  - ICV do período = soma das realizadas limitadas / soma das programadas;
#  - ICV mensal do sentido = média dos ICV diários; só entram saídas
#    programadas avaliáveis (programação válida e dentro do período coletado).
#  - ICV ampliado: também conta como realizadas as viagens quase completas
#    (>= 95% do traçado) e as saídas inconclusivas (benefício da dúvida).
############################################################

PERIODOS_ICV <- data.table::data.table(
  periodo = c("APM", "PM", "EP", "PT", "PN", "PPN"),
  rotulo = c("Antes do pico da manhã (03–05h)", "Pico da manhã (06–08h)", "Entre picos (09–16h)",
             "Pico da tarde (17–19h)", "Noite (20–23h)", "Madrugada (00–02h)"))

.periodo_icv <- function(h) data.table::fcase(h >= 3 & h <= 5, "APM", h >= 6 & h <= 8, "PM",
                                             h >= 9 & h <= 16, "EP", h >= 17 & h <= 19, "PT",
                                             h >= 20 & h <= 23, "PN", default = "PPN")

# Faixas (dia × sentido × hora) com programadas e realizadas
icv_faixas <- function(au, vg, linha, circular, p = cfg$ipp) {
  tz <- cfg$tz_local
  hora <- function(t) as.integer(format(as.POSIXct(t, origin = "1970-01-01", tz = "UTC"), "%H", tz = tz))
  sent <- function(d) if (isTRUE(circular)) rep(0L, length(d)) else as.integer(d)
  pr <- au[!audit_status %in% c("programacao_invalida", "fora_do_periodo_coletado"),
           .(service_date, tipo_dia, sentido = sent(direction_id), hora = hora(as.numeric(scheduled_departure_utc)),
             inconcl = audit_status == "inconclusiva")]
  if (!nrow(pr)) return(data.table())
  fx <- pr[, .(programadas = .N, inconclusivas = sum(inconcl)), by = .(service_date, tipo_dia, sentido, hora)]
  if (nrow(vg)) {
    ok <- vg[origem_ok == TRUE & !is.na(t_partida)]
    ok[, `:=`(completa = trajectory_status %in% c("viagem_confirmada", "viagem_provavel"),
              quase = !trajectory_status %in% c("viagem_confirmada", "viagem_provavel") &
                frac_observada >= p$quase_completa_min & observation_coverage >= p$quase_completa_cob_min)]
    rv <- ok[completa | quase, .(realizadas = sum(completa), quase_completas = sum(quase)),
             by = .(service_date, sentido = sent(direction_id), hora = hora(t_partida))]
    fx <- rv[fx, on = .(service_date, sentido, hora)]
  } else fx[, `:=`(realizadas = 0L, quase_completas = 0L)]
  fx[is.na(realizadas), realizadas := 0L]
  fx[is.na(quase_completas), quase_completas := 0L]
  fx[, `:=`(realizadas_lim = pmin(realizadas, programadas),
            ampliadas_lim = pmin(realizadas + quase_completas + inconclusivas, programadas),
            periodo = .periodo_icv(hora), line_id = linha)]
  setcolorder(fx, c("line_id", "service_date", "tipo_dia", "sentido", "hora", "periodo"))
  fx[order(service_date, sentido, hora)]
}

# ICV por período e dia, médias por dia (sentido) e mês
icv_resumo <- function(fx) {
  if (!nrow(fx)) return(list(periodo_dia = data.table(), dia = data.table(), mes = NA_real_, mes_amp = NA_real_,
                             periodo_mes = data.table()))
  pd <- fx[, .(programadas = sum(programadas), realizadas = sum(realizadas_lim), ampliadas = sum(ampliadas_lim)),
           by = .(service_date, tipo_dia, sentido, periodo)]
  pd[, `:=`(icv = realizadas / programadas, icv_amp = ampliadas / programadas)]
  dd <- pd[, .(icv = mean(icv), icv_amp = mean(icv_amp), programadas = sum(programadas),
               realizadas = sum(realizadas), ampliadas = sum(ampliadas)), by = .(service_date, tipo_dia, sentido)]
  ds <- dd[, .(icv = mean(icv), icv_amp = mean(icv_amp)), by = sentido]
  # média dos ICV diários de cada período, por sentido (apresentação)
  pm <- pd[, .(programadas = sum(programadas), realizadas = sum(realizadas), ampliadas = sum(ampliadas),
               icv = mean(icv), icv_amp = mean(icv_amp), dias = .N), by = .(sentido, periodo)]
  # ICV diário da linha (média dos sentidos)
  dl <- dd[, .(icv = mean(icv), icv_amp = mean(icv_amp), programadas = sum(programadas),
               realizadas = sum(realizadas), ampliadas = sum(ampliadas)), by = .(service_date, tipo_dia)]
  list(periodo_dia = pd, dia_sentido = dd, dia = dl, sentido = ds, periodo_mes = pm,
       mes = mean(ds$icv), mes_amp = mean(ds$icv_amp))
}
