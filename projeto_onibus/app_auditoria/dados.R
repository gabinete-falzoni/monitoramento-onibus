############################################################
# dados.R — camada de consulta do app de auditoria
#
# Lê sob demanda os datasets particionados do pipeline
# (<saida>/<tabela>/mes=AAAA-MM/linha=<linha>/part-0.parquet).
# Cada consulta toca uma partição (linha × mês) e, dentro dela, só os
# row groups do dia pedido; o custo não cresce com o histórico acumulado.
############################################################

OUT <- cfg$saida_dir
TZ <- cfg$tz_local

.ds_cache <- new.env()
ds <- function(tab) {
  if (is.null(.ds_cache[[tab]])) .ds_cache[[tab]] <- arrow::open_dataset(file.path(OUT, tab))
  .ds_cache[[tab]]
}

# Partições linha × mês disponíveis (pela auditoria)
indice_particoes <- function() {
  p <- list.dirs(file.path(OUT, "auditoria_saidas"), recursive = TRUE, full.names = FALSE)
  p <- p[grepl("^mes=[^/]+/linha=[^/]+$", p)]
  data.table(mes = sub("^mes=([^/]+)/.*", "\\1", p), linha = sub(".*/linha=", "", p))
}

datas_linha <- function(l) {
  meses <- indice_particoes()[linha == l, mes]
  d <- ds("programacao_normalizada") |>
    dplyr::filter(linha == !!l, mes %in% !!meses) |>
    dplyr::distinct(service_date) |> dplyr::collect()
  sort(d$service_date)
}

.sem_chave <- function(x) { setattr(x, "sorted", NULL); setattr(x, "index", NULL); x }

le_dia <- function(tab, l, d) {
  m <- format(d, "%Y-%m")
  .sem_chave(as.data.table(dplyr::collect(dplyr::filter(ds(tab), linha == !!l, mes == !!m, service_date == !!d))))
}

le_ref <- function(tab, l, d) {
  m <- format(d, "%Y-%m")
  .sem_chave(as.data.table(dplyr::collect(dplyr::filter(ds(tab), linha == !!l, mes == !!m))))
}

# Tudo o que o app precisa para uma linha × dia
carregar_dia <- function(l, d) {
  au <- le_dia("auditoria_saidas", l, d)
  vg <- le_dia("viagens_reconstruidas", l, d)
  pos <- le_dia("posicoes_normalizadas", l, d)
  tra <- le_ref("referencia_tracados", l, d)
  par <- le_ref("referencia_paradas", l, d)
  sen <- fread(file.path(OUT, "referencia_sentidos.csv"))[line_id == l]
  dg <- le_dia("diagnostico_saidas", l, d)
  lac <- le_dia("lacunas_veiculos", l, d)
  vdf <- le_dia("veiculos_dia_frota", l, d)

  if (nrow(au)) {
    au <- STATUS[, .(audit_status, rotulo, cor)][au, on = "audit_status"]
    au[, t_prog := as.POSIXct(scheduled_departure_utc, tz = TZ)]
    au[, t_obs := as.POSIXct(observed_departure_utc, tz = TZ)]
    au[, horario := format(t_prog, "%H:%M")]
    setorder(au, t_prog, direction_id)
    au[, rid := .I]
    au[, chave := paste(service_date, rid)]
    if (nrow(dg)) au <- dg[, .(scheduled_trip_id, diagnostico_frota, detalhe_frota)][au, on = "scheduled_trip_id"]
    else au[, `:=`(diagnostico_frota = NA_character_, detalhe_frota = NA_character_)]
    # duração: observada (viagem associada) e estimada (mediana das viagens completas do
    # mesmo sentido que partiram até 1 h antes/depois do horário; senão, mediana do dia)
    comp <- vg[trajectory_status %in% c("viagem_confirmada", "viagem_provavel") & !is.na(departure_time_utc)]
    au[, dur_obs := if (nrow(vg)) vg$duration_minutes[match(observed_trip_id, vg$trip_id)] else NA_real_]
    au[, dur_est := if (!nrow(comp)) NA_real_ else mapply(function(di, tp) {
      x <- comp[direction_id == di & abs(as.numeric(departure_time_utc) - tp) <= 3600, duration_minutes]
      if (length(x) >= 2) median(x) else median(comp[direction_id == di, duration_minutes])
    }, direction_id, as.numeric(t_prog))]
    setorder(au, t_prog, direction_id)
  }
  if (nrow(vg)) {
    vg[, `:=`(t_ini = as.POSIXct(start_time_utc, tz = TZ), t_fim = as.POSIXct(end_time_utc, tz = TZ),
              t_dep = as.POSIXct(departure_time_utc, tz = TZ), t_arr = as.POSIXct(arrival_time_utc, tz = TZ))]
    assoc <- if (nrow(au)) au[!is.na(observed_trip_id), .(trip_id = observed_trip_id,
                                                         saida_associada = sprintf("S%d %s", direction_id, horario),
                                                         status_saida = rotulo)] else NULL
    if (!is.null(assoc) && nrow(assoc)) vg <- assoc[vg, on = "trip_id"] else vg[, `:=`(saida_associada = NA_character_, status_saida = NA_character_)]
    setorder(vg, t_ini)
    vg[, vid := .I]
  }
  if (nrow(pos)) {
    pos[, t := as.POSIXct(timestamp_utc, tz = TZ)]
    setorder(pos, vehicle_id, t)
    pos[, s_km := s / 1000]
  }
  if (nrow(lac)) {
    lac[, `:=`(t0 = as.POSIXct(t0_utc, tz = TZ), t1 = as.POSIXct(t1_utc, tz = TZ))]
  }
  list(linha = l, data = d, au = au, vg = vg, pos = pos, tra = tra, par = par, sen = sen,
       lac = lac, vdf = vdf,
       circular = isTRUE(sen$circular[1]), L_km = if (nrow(sen)) sen$L_m[1] / 1000 else NA_real_)
}

# cache simples por linha × dia (o app consulta vários dias de uma vez)
.dia_cache <- new.env()
carregar_dia_cache <- function(l, d) {
  k <- paste(l, d)
  if (is.null(.dia_cache[[k]])) {
    if (length(ls(.dia_cache)) > 20) rm(list = ls(.dia_cache), envir = .dia_cache)
    .dia_cache[[k]] <- carregar_dia(l, d)
  }
  .dia_cache[[k]]
}

# Explicação, em linguagem direta, da regra que gerou cada status
EXPLICA <- c(
  realizada_no_horario = "Uma viagem completa (origem → destino) partiu do terminal até 3 min antes ou depois do horário programado.",
  realizada_com_atraso = "A viagem associada é completa, mas partiu mais de 3 min depois do horário (janela de associação: até +40 min).",
  realizada_antecipadamente = "A viagem associada é completa, mas partiu mais de 3 min antes do horário (janela: até −15 min).",
  viagem_quase_completa = "O ônibus partiu na janela e percorreu ao menos 95% do traçado (GPS em ≥70% do caminho), mas a chegada não foi registrada: o GPS salta para o destino. A viagem muito provavelmente ocorreu; não entra no IPP oficial.",
  partida_sem_viagem_completa = "Um ônibus partiu do terminal na janela e percorreu entre 15% e 95% do traçado, sem chegar ao destino. Não conta como cumprida. (Abaixo de 15% a partida é considerada espúria e descartada.)",
  possivelmente_nao_realizada = "Na associação ordenada do dia (cada partida observada serve a no máximo um horário, janela −15/+40 min) não sobrou partida para este horário. Havia GPS da linha em ≥50% dos minutos da janela −20/+60 min e nenhum ônibus sem GPS (com lacuna, ou que parou de transmitir) poderia ter feito a viagem.",
  inconclusiva = "Sem partida associada, mas a cobertura de GPS na janela foi < 50%, ou um ônibus em serviço estava sem transmitir (lacuna, ou parou de transmitir e não voltou) e poderia ter feito a viagem: não dá para afirmar que a saída não ocorreu.",
  programacao_invalida = "A tabela de horários deste tipo de dia foi considerada inválida; o horário não entra no IPP.",
  fora_do_periodo_coletado = "O horário cai fora do período coberto pelos arquivos de GPS."
)
