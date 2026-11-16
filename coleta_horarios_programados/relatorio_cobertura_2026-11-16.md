# Cobertura da coleta SPTrans — 2026-11-16

- GTFS de referência: `20261104` (1364 rotas em routes.txt)
- Catálogo do site: 1366 linhas; coletadas: 1366; com partidas: 1356
- Partidas coletadas: 351 559; trechos de itinerário: 189 767

## Situação de cada rota (GTFS ∪ site)

| situacao | N |
|---|---|
| coletada com partidas | 1354 |
| coletada, sem partidas no site |    9 |
| site: ausente do GTFS |    2 |
| erro na API/parse |    1 |

### Rotas do GTFS não coletadas

| route_id | situacao | erro_api |
|---|---|---|
| METRÔ 15 | erro na API/parse | Erro ao buscar detalhe da linha |

### Coletadas, mas o site não publica partidas

(route_type 1/2 = metrô/trem: o site lista as linhas mas não traz horários)

| route_id | route_type |
|---|---|
| CPTM L07 | 2 |
| CPTM L08 | 2 |
| CPTM L09 | 2 |
| CPTM L10 | 2 |
| CPTM L11 | 2 |
| CPTM L12 | 2 |
| METRÔ L1 | 1 |
| METRÔ L2 | 1 |
| METRÔ L3 | 1 |

## Dias de operação: GTFS × site

Linhas com partidas no site e com viagens (trips.txt) no GTFS: 1315 (as demais 39 com partidas não têm trips no GTFS).

1314 de 1315 linhas com partidas têm os mesmos dias (U/S/D) nos dois. Divergências:

| route_id | gtfs | site |
|---|---|---|
| 771P-10 | USD | US_ |

## Partidas em dia útil: GTFS (frequencies.txt) × site

Em 1315 linhas comparáveis, a razão site/GTFS tem mediana 1.00; 1306 linhas (99.3%) ficam entre 0,8 e 1,25.
O GTFS da SPTrans descreve frequências por faixa horária, não horários exatos; a comparação é só ordem de grandeza.

## Correção da falha dos domingos

- Raspagem antiga (`2026-10-25`, navegador): 2205 de 2205 tabelas de domingo idênticas à de dia útil (1290 linhas coletadas).
  Nessas, o sábado era igual ao dia útil em só 221 casos — a tabela D era cópia da U. Além disso, a antiga tem 452 tabelas D para linha/sentido que não operam aos domingos.
- Coleta nova (API): 223 de 1778 tabelas de domingo idênticas à de dia útil; em todas elas o sábado também é idêntico (223), ou seja, linhas com a mesma programação nos três dias, não erro de coleta.
- Partidas de dia útil por linha, antiga × nova: idênticas em 1268 de 1290 linhas em comum.

