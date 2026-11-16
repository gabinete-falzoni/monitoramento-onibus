############################################################
# 03_mapmatching.R — posição de cada GPS ao longo do ciclo da linha
#
# Saída por registro válido:
#   s        posição no ciclo (m, 0..L) ou NA se fora da rota
#   d_rota   distância GPS–traçado (m)
#   chain    cadeia contínua de map matching (quebra em lacunas longas
#            ou saltos implausíveis)
#   u        progresso acumulado "desenrolado" (m) dentro da cadeia: u/L
#            conta voltas completas
#   u_mono   max cumulativo de u (remove recuos de ruído GPS)
#   sentido_inferido  sentido do ciclo em que o ponto está (pela posição)
############################################################

mapmatch_linha <- function(dt, ciclo, p = cfg$mm) {
  v <- dt[valido == TRUE]
  setorder(v, vehicle_id, timestamp_utc)
  cand <- as.data.table(mm_candidates(v$x, v$y, ciclo$vert$x, ciclo$vert$y, ciclo$vert$cum,
                                      p$raio_candidato_m, p$fundir_candidatos_m, p$max_candidatos))
  setorder(cand, pt, d)
  cand[, k := seq_len(.N)]
  # penalidade opcional por incompatibilidade com `sl`
  cand[, sentido_cand := ciclo$sentidos$direction_id[findInterval(s, ciclo$sentidos$a)]]
  cand[, pen := 0]
  if (p$penal_sl > 0 && !ciclo$circular && !is.null(ciclo$mapa_sl)) {
    cand[, sl := v$direction_sl[pt]]
    cand[ciclo$mapa_sl, sl_sentido := i.direction_id, on = "sl"]
    cand[!is.na(sl_sentido) & sl_sentido != sentido_cand, pen := p$penal_sl]
  }
  cstart <- integer(nrow(v)); ccount <- integer(nrow(v))
  idx <- cand[, .(st = min(k) - 1L, n = .N), by = pt]
  cstart[idx$pt] <- idx$st; ccount[idx$pt] <- idx$n

  # maior lacuna de transmissão (registros válidos, dentro ou fora da rota)
  # desde o ponto com candidatos anterior do mesmo veículo
  tem <- ccount > 0
  grp <- cumsum(c(0L, head(tem, -1)))
  gdt <- fifelse(is.na(v$dt_s), Inf, v$dt_s)
  gapb <- data.table(grp, gdt)[, g := max(gdt), by = grp]$g
  veh_int <- as.integer(factor(v$vehicle_id))
  tsec <- as.numeric(v$timestamp_utc)
  vt <- mm_viterbi(veh_int, tsec, v$x, v$y, cstart, ccount, cand$s, cand$d, cand$pen,
                   gapb, ciclo$L, p$sigma_gps_m, p$beta_m, p$vmax_ms, p$folga_m, p$recuo_tol_m,
                   p$lacuna_quebra_s, p$outlier_lookahead)
  v[, `:=`(s = cand$s[vt$chosen], d_rota = cand$d[vt$chosen], chain = vt$chain,
           delta = vt$delta, quebra = vt$brk, n_cand = ccount)]
  v[, na_rota := !is.na(s)]
  # progresso desenrolado por cadeia
  v[(na_rota), u := s[1] + cumsum(delta), by = chain]
  v[(na_rota), u_mono := cummax(u), by = chain]
  v[(na_rota), sentido_inferido := ciclo$sentidos$direction_id[findInterval(s, ciclo$sentidos$a)]]
  v[, chain := fifelse(is.na(chain), NA_integer_, chain)]
  v[]
}

# Relação entre a marcação `sl` e o sentido inferido pela posição
diagnostico_sl <- function(v, ciclo) {
  if (ciclo$circular) {
    # na circular, onde (em % do ciclo) o motorista troca o sl
    tr <- v[(na_rota)][, .(s, sl = direction_sl, sl_ant = shift(direction_sl)), by = chain][
      !is.na(sl_ant) & sl != sl_ant]
    tr[, `:=`(linha = ciclo$linha, troca = paste0(sl_ant, "->", sl), pos_km = s / 1000)]
    return(tr[, .(n = .N, pos_km_p10 = quantile(pos_km, .1), pos_km_mediana = median(pos_km),
                  pos_km_p90 = quantile(pos_km, .9)), by = .(linha, troca)])
  }
  v[(na_rota), .N, by = .(sl = direction_sl, sentido_inferido)][, linha := ciclo$linha][
    , pct := round(100 * N / sum(N), 2), by = sl][order(sl, sentido_inferido)]
}
