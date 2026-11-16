// ---------------------------------------------------------------------------
// mapmatch.cpp — rotinas de desempenho do pipeline Olho Vivo
//
// Justificativa (ver README): estas três rotinas são laços sequenciais que não
// vetorizam bem em R e dominam o tempo de execução quando o pipeline roda para
// todas as linhas e meses. Tudo o mais (leitura, junções, agregações) fica em
// R/data.table.
//
//   1. mm_candidates(): para cada posição GPS, encontra as posições candidatas
//      ao longo do "ciclo" da linha (polilinha fechada, em metros). Uma linha
//      pode passar mais de uma vez perto do mesmo ponto (ida/volta na mesma via,
//      laços de linhas circulares), por isso devolvemos todos os mínimos locais
//      de distância dentro do raio, não só o mais próximo.
//
//   2. mm_viterbi(): escolhe, por veículo, a sequência de candidatos mais
//      plausível (HMM de map matching, Newson & Krumm 2009), com a restrição de
//      que o ônibus avança ao longo do ciclo (pequenos recuos tolerados por
//      ruído GPS) e não excede uma velocidade máxima. Quando nenhuma transição
//      é plausível, ou a lacuna temporal é longa, a cadeia é quebrada.
//
//   3. range_max(): máximo de um vetor em intervalos [ini, fim] (lacunas
//      máximas dentro de cada viagem).
// ---------------------------------------------------------------------------
#include <Rcpp.h>
#include <vector>
#include <cmath>
#include <algorithm>
using namespace Rcpp;

// [[Rcpp::export]]
List mm_candidates(NumericVector px, NumericVector py,
                   NumericVector vx, NumericVector vy, NumericVector vcum,
                   double radius, double merge_m, int max_cand, bool circular = true) {
  const int n = px.size();
  const int m = vx.size() - 1;              // número de segmentos
  std::vector<double> x0(m), y0(m), dx(m), dy(m), l2(m), len(m);
  std::vector<double> bx0(m), bx1(m), by0(m), by1(m);
  for (int j = 0; j < m; ++j) {
    x0[j] = vx[j]; y0[j] = vy[j];
    dx[j] = vx[j + 1] - vx[j]; dy[j] = vy[j + 1] - vy[j];
    l2[j] = dx[j] * dx[j] + dy[j] * dy[j];
    len[j] = std::sqrt(l2[j]);
    bx0[j] = std::min(vx[j], vx[j + 1]) - radius; bx1[j] = std::max(vx[j], vx[j + 1]) + radius;
    by0[j] = std::min(vy[j], vy[j + 1]) - radius; by1[j] = std::max(vy[j], vy[j + 1]) + radius;
  }
  std::vector<int> out_pt; std::vector<double> out_s, out_d;
  out_pt.reserve(n * 2); out_s.reserve(n * 2); out_d.reserve(n * 2);
  std::vector<double> dseg(m), sseg(m);
  const double INF = 1e18;
  for (int i = 0; i < n; ++i) {
    if (i % 10000 == 0) Rcpp::checkUserInterrupt();
    const double x = px[i], y = py[i];
    if (!R_finite(x) || !R_finite(y)) continue;
    bool any = false;
    for (int j = 0; j < m; ++j) {
      if (x < bx0[j] || x > bx1[j] || y < by0[j] || y > by1[j] || l2[j] == 0) { dseg[j] = INF; continue; }
      double t = ((x - x0[j]) * dx[j] + (y - y0[j]) * dy[j]) / l2[j];
      t = std::max(0.0, std::min(1.0, t));
      const double qx = x0[j] + t * dx[j], qy = y0[j] + t * dy[j];
      const double d = std::sqrt((x - qx) * (x - qx) + (y - qy) * (y - qy));
      dseg[j] = d; sseg[j] = vcum[j] + t * len[j];
      if (d <= radius) any = true;
    }
    if (!any) continue;
    // mínimos locais (ciclo fechado: vizinhos circulares)
    std::vector<std::pair<double, double>> cand; // (d, s)
    for (int j = 0; j < m; ++j) {
      const double d = dseg[j];
      if (d > radius) continue;
      double dp, dn;
      if (circular) { dp = dseg[(j - 1 + m) % m]; dn = dseg[(j + 1) % m]; }
      else { dp = (j > 0) ? dseg[j - 1] : INF; dn = (j < m - 1) ? dseg[j + 1] : INF; }
      if (d <= dp && d < dn) cand.push_back({d, sseg[j]});
    }
    // fundir candidatos muito próximos ao longo do ciclo
    std::sort(cand.begin(), cand.end());
    std::vector<std::pair<double, double>> keep;
    const double L = vcum[m];
    for (auto &c : cand) {
      bool close = false;
      for (auto &k : keep) {
        double ds = std::fabs(c.second - k.second);
        if (circular) ds = std::min(ds, L - ds);
        if (ds < merge_m) { close = true; break; }
      }
      if (!close) keep.push_back(c);
      if ((int)keep.size() >= max_cand) break;
    }
    for (auto &k : keep) { out_pt.push_back(i + 1); out_d.push_back(k.first); out_s.push_back(k.second); }
  }
  return List::create(_["pt"] = wrap(out_pt), _["s"] = wrap(out_s), _["d"] = wrap(out_d));
}

// Candidatos de cada ponto ficam contíguos em (cs, cd, cpen), indexados por
// [cstart[i], cstart[i] + ccount[i]). Pontos sem candidato têm ccount = 0.
// Pontos devem estar ordenados por veículo e tempo.
// [[Rcpp::export]]
List mm_viterbi(IntegerVector veh, NumericVector t, NumericVector px, NumericVector py,
                IntegerVector cstart, IntegerVector ccount,
                NumericVector cs, NumericVector cd, NumericVector cpen,
                NumericVector gapb, double L, double sigma, double beta, double vmax, double slack_m,
                double back_tol_m, double max_link_gap_s, int lookahead = 6) {
  const int n = veh.size();
  IntegerVector chosen(n, NA_INTEGER);   // índice (1-based) do candidato escolhido
  IntegerVector chain(n, NA_INTEGER);    // id da cadeia
  NumericVector delta(n, NA_REAL);       // avanço (m) desde o ponto anterior da cadeia
  IntegerVector brk(n, 0);               // motivo de quebra no início da cadeia
  // 1 = novo veículo/início; 2 = lacuna longa; 3 = transição implausível;
  // 4 = ponto descartado como outlier (não pertence a cadeia)
  int chain_id = 0;

  std::vector<int> pts;                        // pontos (com candidatos) da cadeia atual
  std::vector<std::vector<double>> cost;       // custo acumulado por candidato
  std::vector<std::vector<int>> back;          // ponteiro para candidato anterior
  std::vector<std::vector<double>> dlt;        // delta da melhor transição

  auto emission = [&](int k) { const double z = cd[k] / sigma; return 0.5 * z * z + cpen[k]; };

  auto flush = [&]() {
    if (pts.empty()) return;
    ++chain_id;
    const int np = pts.size();
    int best = 0; double bc = 1e300;
    for (int a = 0; a < (int)cost[np - 1].size(); ++a) if (cost[np - 1][a] < bc) { bc = cost[np - 1][a]; best = a; }
    for (int p = np - 1; p >= 0; --p) {
      const int i = pts[p];
      chosen[i] = cstart[i] + best + 1;
      chain[i] = chain_id;
      delta[i] = (p == 0) ? 0.0 : dlt[p][best];
      if (p > 0) best = back[p][best];
    }
    pts.clear(); cost.clear(); back.clear(); dlt.clear();
  };

  // transição de `from` (com custos `pc`) para o ponto `i`; devolve se viável
  auto step = [&](int from, const std::vector<double> &pc, int i,
                  std::vector<double> &oc, std::vector<int> &ob, std::vector<double> &od) {
    const int nc = ccount[i];
    const double dt = std::max(1.0, t[i] - t[from]);
    const double gx = px[i] - px[from], gy = py[i] - py[from];
    const double gc = std::sqrt(gx * gx + gy * gy);
    const double fwd_max = vmax * dt + slack_m;
    oc.assign(nc, 1e300); ob.assign(nc, -1); od.assign(nc, NA_REAL);
    bool feasible = false;
    for (int b = 0; b < nc; ++b) {
      const int kb = cstart[i] + b;
      const double em = emission(kb);
      for (int a = 0; a < (int)pc.size(); ++a) {
        const int ka = cstart[from] + a;
        double d = std::fmod(cs[kb] - cs[ka] + L, L);
        if (d > L - back_tol_m) d -= L;          // pequeno recuo (ruído)
        if (d < -back_tol_m || d > fwd_max) continue;
        const double c = pc[a] + std::fabs(d - gc) / beta + em;
        if (c < oc[b]) { oc[b] = c; ob[b] = a; od[b] = d; feasible = true; }
      }
    }
    return feasible;
  };
  auto next_with_cand = [&](int i) { int j = i + 1; while (j < n && ccount[j] == 0) ++j; return j; };

  int last = -1; // último ponto aceito na cadeia
  std::vector<double> oc, od; std::vector<int> ob;
  for (int i = 0; i < n; ++i) {
    if (i % 20000 == 0) Rcpp::checkUserInterrupt();
    const int nc = ccount[i];
    if (nc == 0) continue;
    int reason = 0;
    if (last < 0 || veh[i] != veh[last]) reason = 1;
    // lacuna de TRANSMISSÃO (gapb): maior intervalo entre registros brutos
    // desde o último ponto com candidatos. Ficar fora da rota mas transmitindo
    // (ex.: dentro do terminal) não quebra a cadeia.
    else if (gapb[i] > max_link_gap_s) reason = 2;
    if (reason == 0) {
      if (step(last, cost.back(), i, oc, ob, od)) {
        pts.push_back(i); cost.push_back(oc); back.push_back(ob); dlt.push_back(od);
        last = i; continue;
      }
      // ponto possivelmente espúrio (ex.: "salto" para o terminal do sentido
      // recém-marcado): se algum dos próximos `lookahead` pontos é viável a
      // partir do último aceito, descarta-se `i` como outlier
      bool skip = false; int j = i;
      for (int look = 0; look < lookahead && !skip; ++look) {
        j = next_with_cand(j);
        if (j >= n || veh[j] != veh[last] || gapb[j] > max_link_gap_s) break;
        std::vector<double> tc, td; std::vector<int> tb;
        if (step(last, cost.back(), j, tc, tb, td)) skip = true;
      }
      if (skip) { brk[i] = 4; continue; }
      reason = 3;
    }
    // quebra: fecha cadeia atual e inicia outra
    flush();
    std::vector<double> c0(nc); std::vector<int> b0(nc, -1); std::vector<double> d0(nc, 0.0);
    for (int b = 0; b < nc; ++b) c0[b] = emission(cstart[i] + b);
    pts.push_back(i); cost.push_back(c0); back.push_back(b0); dlt.push_back(d0);
    brk[i] = reason;
    last = i;
  }
  flush();
  return List::create(_["chosen"] = chosen, _["chain"] = chain, _["delta"] = delta, _["brk"] = brk);
}

// [[Rcpp::export]]
NumericVector range_max(NumericVector x, IntegerVector ini, IntegerVector fim) {
  const int k = ini.size();
  NumericVector out(k, NA_REAL);
  for (int j = 0; j < k; ++j) {
    if (ini[j] == NA_INTEGER || fim[j] == NA_INTEGER || fim[j] < ini[j]) continue;
    double mx = R_NegInf;
    for (int i = ini[j] - 1; i < fim[j]; ++i) if (!ISNAN(x[i]) && x[i] > mx) mx = x[i];
    out[j] = R_finite(mx) ? mx : NA_REAL;
  }
  return out;
}

// Associação ordenada entre partidas programadas (s) e observadas (o), ambas
// ordenadas, por programação dinâmica (alinhamento de sequências). Um par é
// admissível se -antecip_max <= o - s <= atraso_max. Custo do par = |o - s|;
// programada sem par custa c_miss; observada sem par custa c_extra.
// Devolve, para cada programada, o índice (1-based) da observada ou NA.
// [[Rcpp::export]]
IntegerVector align_departures(NumericVector s, NumericVector o,
                               double antecip_max, double atraso_max,
                               double c_miss, double c_extra) {
  const int n = s.size(), m = o.size();
  std::vector<std::vector<double>> D(n + 1, std::vector<double>(m + 1, 0.0));
  std::vector<std::vector<char>> P(n + 1, std::vector<char>(m + 1, 0));
  for (int i = 1; i <= n; ++i) { D[i][0] = D[i - 1][0] + c_miss; P[i][0] = 1; }
  for (int j = 1; j <= m; ++j) { D[0][j] = D[0][j - 1] + c_extra; P[0][j] = 2; }
  for (int i = 1; i <= n; ++i) for (int j = 1; j <= m; ++j) {
    double best = D[i - 1][j] + c_miss; char p = 1;
    const double alt = D[i][j - 1] + c_extra;
    if (alt < best) { best = alt; p = 2; }
    const double diff = o[j - 1] - s[i - 1];
    if (diff >= -antecip_max && diff <= atraso_max) {
      const double mc = D[i - 1][j - 1] + std::fabs(diff);
      if (mc < best) { best = mc; p = 3; }
    }
    D[i][j] = best; P[i][j] = p;
  }
  IntegerVector out(n, NA_INTEGER);
  int i = n, j = m;
  while (i > 0 || j > 0) {
    const char p = P[i][j];
    if (p == 3) { out[i - 1] = j; --i; --j; }
    else if (p == 1) --i; else --j;
  }
  return out;
}
