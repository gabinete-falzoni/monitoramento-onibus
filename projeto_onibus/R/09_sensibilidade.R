############################################################
# 09_sensibilidade.R — cenários alternativos de parâmetros
#
# Varia um parâmetro por vez (os demais no valor central) e registra
# contagens de viagens por classe e indicadores de auditoria.
############################################################

.resumo_cenario <- function(tr, au, linha, parametro, valor) {
  st <- if (nrow(tr)) tr[, .N, by = trajectory_status] else data.table(trajectory_status = character(), N = integer())
  g <- function(k) { x <- st[trajectory_status == k, N]; if (length(x)) x else 0L }
  ind <- if (nrow(au)) indicadores_ipp(au, by = NULL) else data.table(IPP_pct = NA, realizadas_pct = NA,
                                                                       nao_realizadas_pct = NA, inconclusivas_pct = NA,
                                                                       possivelmente_nao_realizadas = NA)
  data.table(line_id = linha, parametro = parametro, valor = as.character(valor),
             viagens_confirmadas = g("viagem_confirmada"), viagens_provaveis = g("viagem_provavel"),
             trajetorias_parciais = g("trajetoria_parcial"), inconclusivas = g("observacao_inconclusiva"),
             IPP_pct = ind$IPP_pct, realizadas_pct = ind$realizadas_pct,
             possivelmente_nao_realizadas = ind$possivelmente_nao_realizadas,
             nao_realizadas_pct = ind$nao_realizadas_pct, saidas_inconclusivas_pct = ind$inconclusivas_pct)
}

sensibilidade_linha <- function(dt, ciclo, prog_l, linha, periodo_coleta = NULL, sens = cfg$sensibilidade) {
  res <- list()
  rodar <- function(mm_p = cfg$mm, vg_p = cfg$viagem, ipp_p = cfg$ipp, v = NULL) {
    if (is.null(v)) v <- mapmatch_linha(dt, ciclo, mm_p)
    ex <- extrair_viagens(v, ciclo, vg_p)
    tr <- classificar_viagens(ex$viagens, ciclo, linha, vg_p)
    au <- auditar_linha(prog_l, tr, ex$obs, linha, ipp_p, lacunas = lacunas_em_servico(v),
                        periodo_coleta = periodo_coleta)$auditoria
    list(tr = tr, au = au, v = v)
  }
  base <- rodar()
  for (r in sens$raio_candidato_m) {
    mp <- cfg$mm; mp$raio_candidato_m <- r
    x <- if (r == cfg$mm$raio_candidato_m) base else rodar(mm_p = mp)
    res[[length(res) + 1]] <- .resumo_cenario(x$tr, x$au, linha, "raio_candidato_m", r)
  }
  for (g in sens$lacuna_interp_s) {
    vp <- cfg$viagem; vp$lacuna_interp_s <- g
    x <- rodar(vg_p = vp, v = base$v)
    res[[length(res) + 1]] <- .resumo_cenario(x$tr, x$au, linha, "lacuna_interp_s", g)
  }
  for (cc in sens$cobertura_confirmada) {
    vp <- cfg$viagem; vp$cobertura_confirmada <- cc
    x <- rodar(vg_p = vp, v = base$v)
    res[[length(res) + 1]] <- .resumo_cenario(x$tr, x$au, linha, "cobertura_confirmada", cc)
  }
  for (tl in sens$tolerancia_ipp_min) {
    ip <- cfg$ipp; ip$tolerancia_min <- tl
    x <- rodar(ipp_p = ip, v = base$v)
    res[[length(res) + 1]] <- .resumo_cenario(x$tr, x$au, linha, "tolerancia_ipp_min", tl)
  }
  out <- rbindlist(res)
  out[, cenario_central := (parametro == "raio_candidato_m" & valor == as.character(cfg$mm$raio_candidato_m)) |
        (parametro == "lacuna_interp_s" & valor == as.character(cfg$viagem$lacuna_interp_s)) |
        (parametro == "cobertura_confirmada" & valor == as.character(cfg$viagem$cobertura_confirmada)) |
        (parametro == "tolerancia_ipp_min" & valor == as.character(cfg$ipp$tolerancia_min))]
  out[]
}
