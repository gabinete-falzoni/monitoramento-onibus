############################################################
# 11_relatorio_linha.R — funções do relatório de cumprimento por linha
# (usadas por relatorios/relatorio_linha.qmd e pelo app de auditoria)
############################################################

# Status da auditoria: ordem, rótulo curto e cor (paleta de status:
# bom / atenção / grave / crítico; cinzas = sem avaliação)
STATUS <- data.table::data.table(
  audit_status = c("realizada_no_horario", "realizada_com_atraso", "realizada_antecipadamente",
                   "viagem_quase_completa", "partida_sem_viagem_completa", "possivelmente_nao_realizada",
                   "inconclusiva", "programacao_invalida", "fora_do_periodo_coletado"),
  rotulo = c("No horário", "Atrasada", "Antecipada", "Quase completa (≥95%)", "Saiu, não completou",
             "Não realizada", "Inconclusiva", "Programação inválida", "Fora do período"),
  cor = c("#0ca30c", "#fab219", "#ec835a", "#3f7fb5", "#8f2a2a", "#d03b3b", "#9a998f", "#d6d5cf", "#ecebe6"),
  # agrupamentos usados nas tabelas
  grupo = c("cumprida", "fora_tolerancia", "fora_tolerancia", "quase_completa", "nao_cumprida", "nao_cumprida",
            "inconclusiva", "sem_avaliacao", "sem_avaliacao")
)

# Lê a auditoria de uma linha (todas as partições de mês ou só `meses`)
ler_auditoria_linha <- function(linha, meses = NULL, out = cfg$saida_dir) {
  d <- arrow::open_dataset(file.path(out, "auditoria_saidas"))
  d <- dplyr::filter(d, linha == !!linha)
  if (!is.null(meses)) d <- dplyr::filter(d, mes %in% !!meses)
  au <- data.table::as.data.table(dplyr::collect(d))
  data.table::setattr(au, "sorted", NULL)
  if (!nrow(au)) return(au)
  au[, horario := substr(scheduled_departure_local, 12, 16)]
  # horários após a meia-noite pertencem ao dia de serviço anterior: 24:10, 25:00...
  au[, min_dia := as.integer(substr(horario, 1, 2)) * 60L + as.integer(substr(horario, 4, 5))]
  au[as.Date(substr(scheduled_departure_local, 1, 10)) > service_date, min_dia := min_dia + 1440L]
  au[, horario_grade := sprintf("%02d:%02d", min_dia %/% 60L, min_dia %% 60L)]
  au <- STATUS[, .(audit_status, rotulo, grupo)][au, on = "audit_status"]
  au[]
}

# Grade de cumprimento em SVG: colunas = dias, linhas = horários da tabela daquele
# tipo de dia, cor = status. Cada célula tem tooltip (<title>) com os detalhes.
grade_svg <- function(a, cel_w = 20, cel_h = 7, marg_esq = 46, marg_top = 34) {
  if (!nrow(a)) return(htmltools::HTML("<p><em>Sem saídas.</em></p>"))
  datas <- sort(unique(a$service_date))
  hor <- sort(unique(a$min_dia))
  a <- STATUS[, .(audit_status, cor)][a, on = "audit_status"]
  a[, `:=`(cx = match(service_date, datas), cy = match(min_dia, hor))]
  W <- marg_esq + length(datas) * cel_w + 10
  H <- marg_top + length(hor) * cel_h + 6
  esc <- function(x) gsub("<", "&lt;", gsub("&", "&amp;", x, fixed = TRUE), fixed = TRUE)
  tip <- a[, esc(sprintf("%s %s %s\n%s%s%s", format(service_date, "%d/%m"), horario_grade,
                         c(`0` = "sentido 0", `1` = "sentido 1")[as.character(direction_id)], rotulo,
                         ifelse(is.na(observed_departure_local), "",
                                sprintf("\npartida obs. %s (%+.1f min), veículo %s", substr(observed_departure_local, 12, 19),
                                        departure_difference_minutes, observed_vehicle_id)),
                         ifelse(is.na(evidence_summary), "", paste0("\n", evidence_summary))))]
  if ("detalhe_frota" %in% names(a))
    tip <- paste0(tip, ifelse(is.na(a$detalhe_frota), "", esc(paste0("\nfrota: ", a$detalhe_frota))))
  cells <- sprintf('<rect data-s="%s" x="%.1f" y="%.1f" width="%.1f" height="%.1f" fill="%s"><title>%s</title></rect>',
                   a$audit_status, marg_esq + (a$cx - 1) * cel_w + 1, marg_top + (a$cy - 1) * cel_h + 0.5,
                   cel_w - 2, cel_h - 1, a$cor, tip)
  # rótulos: uma marca por hora cheia
  hh <- hor %/% 60L
  first_h <- which(!duplicated(hh))
  # descarta rótulos que ficariam sobrepostos (horas com poucas saídas)
  manter <- logical(length(first_h)); ult <- -Inf
  for (k in seq_along(first_h)) if ((first_h[k] - 1) * cel_h - ult >= 11) { manter[k] <- TRUE; ult <- (first_h[k] - 1) * cel_h }
  first_h <- first_h[manter]
  ylab <- sprintf('<text x="%d" y="%.1f" class="ax" text-anchor="end">%02dh</text>',
                  marg_esq - 6, marg_top + (first_h - 1) * cel_h + cel_h, hh[first_h])
  wd <- c("D", "S", "T", "Q", "Q", "S", "S")[as.POSIXlt(datas)$wday + 1]
  xlab <- sprintf('<text x="%.1f" y="%d" class="ax" text-anchor="middle">%s</text><text x="%.1f" y="%d" class="ax2" text-anchor="middle">%s</text>',
                  marg_esq + (seq_along(datas) - 0.5) * cel_w, marg_top - 18, format(datas, "%d"),
                  marg_esq + (seq_along(datas) - 0.5) * cel_w, marg_top - 6, wd)
  htmltools::HTML(sprintf(
    '<svg class="grade" viewBox="0 0 %d %d" width="%d" height="%d" role="img" aria-label="Grade de cumprimento">%s%s%s</svg>',
    W, H, W, H, paste(xlab, collapse = ""), paste(ylab, collapse = ""), paste(cells, collapse = "")))
}

# Lê qualquer tabela particionada do pipeline para uma linha (e meses)
ler_tabela_linha <- function(tab, linha, meses = NULL, out = cfg$saida_dir) {
  dd <- file.path(out, tab)
  if (!dir.exists(dd)) return(data.table::data.table())
  d <- dplyr::filter(arrow::open_dataset(dd), linha == !!linha)
  if (!is.null(meses)) d <- dplyr::filter(d, mes %in% !!meses)
  x <- data.table::as.data.table(dplyr::collect(d))
  data.table::setattr(x, "sorted", NULL)   # chave gravada no parquet não vale para a ordem lida
  data.table::setattr(x, "index", NULL)
  x
}

# Por horário da grade: quantas vezes no mês foi cumprido / pulado
tabela_horarios <- function(au) {
  v <- au[!grupo %in% c("sem_avaliacao", "inconclusiva")]
  if (!nrow(v)) return(data.table::data.table())
  t <- v[, .(dias_avaliados = .N,
             no_horario = sum(audit_status == "realizada_no_horario"),
             atrasada = sum(audit_status == "realizada_com_atraso"),
             antecipada = sum(audit_status == "realizada_antecipadamente"),
             quase_completa = sum(audit_status == "viagem_quase_completa"),
             saiu_nao_completou = sum(audit_status == "partida_sem_viagem_completa"),
             nao_realizada = sum(audit_status == "possivelmente_nao_realizada"),
             dias_nao_realizada = paste(format(sort(service_date[audit_status == "possivelmente_nao_realizada"]), "%d"),
                                        collapse = ", ")),
         by = .(direction_id, tipo_dia, horario = horario_grade, min_dia)]
  t[, pct_pulada := round(100 * (nao_realizada + saiu_nao_completou) / dias_avaliados, 1)]
  t[, pct_no_horario := round(100 * no_horario / dias_avaliados, 1)]
  t[, n_pulada := nao_realizada + saiu_nao_completou]
  data.table::setorder(t, -n_pulada, -pct_pulada, direction_id, min_dia)
  t[, n_pulada := NULL]
  t[, min_dia := NULL][]
}

# Por dia: totais e lista dos horários não realizados
tabela_dias <- function(au) {
  au[, .(programadas = .N,
         validas = sum(grupo != "sem_avaliacao"),
         no_horario = sum(audit_status == "realizada_no_horario"),
         fora_tolerancia = sum(grupo == "fora_tolerancia"),
         quase_completa = sum(audit_status == "viagem_quase_completa"),
         saiu_nao_completou = sum(audit_status == "partida_sem_viagem_completa"),
         nao_realizada = sum(audit_status == "possivelmente_nao_realizada"),
         inconclusiva = sum(audit_status == "inconclusiva"),
         sem_avaliacao = sum(audit_status %in% c("programacao_invalida", "fora_do_periodo_coletado")),
         horarios_nao_realizados = {
           o <- order(direction_id, min_dia)
           m <- (audit_status == "possivelmente_nao_realizada")[o]
           paste(sprintf("S%d %s", direction_id, horario_grade)[o][m], collapse = "  ")
         }),
     by = .(service_date, tipo_dia)][
       , IPP_pct := ifelse(validas > 0, round(100 * no_horario / validas, 1), NA_real_)][order(service_date)]
}

# ------------------------------------------------------------------
# Saídas estáticas (sem depender de JavaScript nem de <img data:>), para que o
# relatório abra completo em qualquer visualizador de HTML.

# ggplot -> <svg> inline
svg_inline <- function(g, width = 10, height = 3.6) {
  s <- svglite::svgstring(width = width, height = height, standalone = FALSE)
  print(g)
  grDevices::dev.off()
  x <- as.character(s())
  x <- sub("<svg ", '<svg style="width:100%;height:auto;max-width:1000px" ', x, fixed = TRUE)
  htmltools::HTML(x)
}

# data.frame -> <table> HTML. `rotulos`: nomes exibidos (named vector);
# `barra`: coluna numérica 0–100 desenhada como barra; `destaque`: coluna
# numérica realçada quando > 0; `mono`: coluna em fonte monoespaçada.
tabela_html <- function(d, rotulos = NULL, id = NULL, barra = NULL, destaque = NULL, mono = NULL,
                        cor_barra = "#d03b3b", quebra = NULL) {
  d <- as.data.frame(d)
  esc <- function(x) htmltools::htmlEscape(ifelse(is.na(x), "", as.character(x)))
  nm <- names(d)
  lab <- ifelse(nm %in% names(rotulos), rotulos[nm], nm)
  num <- vapply(d, is.numeric, TRUE)
  th <- paste0(sprintf('<th class="%s">%s</th>', ifelse(num, "n", ""), esc(lab)), collapse = "")
  cols <- lapply(seq_along(nm), function(j) {
    v <- d[[j]]; k <- nm[j]
    txt <- if (is.numeric(v)) esc(format(v, big.mark = ".", decimal.mark = ",", trim = TRUE)) else esc(v)
    txt[is.na(v)] <- ""
    if (!is.null(barra) && k == barra)
      txt <- sprintf('<span class="bar" style="width:%.0fpx;background:%s"></span>%s%%',
                     pmax(0, v) * 0.8, cor_barra, txt)
    cls <- ifelse(rep(num[j], length(v)), "n", "")
    if (!is.null(destaque) && k == destaque) cls <- paste(cls, ifelse(!is.na(v) & v > 0, "hl", ""))
    if (!is.null(mono) && k == mono) cls <- paste(cls, "mono")
    if (!is.null(quebra) && k %in% quebra) cls <- paste(cls, "wrap")
    sprintf('<td class="%s" data-v="%s">%s</td>', trimws(cls), esc(v), txt)
  })
  linhas <- do.call(paste0, cols)
  htmltools::HTML(sprintf(
    '<div class="tbwrap"><table class="tb"%s><thead><tr>%s</tr></thead><tbody>%s</tbody></table></div>',
    if (is.null(id)) "" else sprintf(' id="%s"', id), th,
    paste0("<tr>", linhas, "</tr>", collapse = "")))
}
