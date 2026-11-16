# Projeto Olho Vivo — reconstrução de viagens e auditoria de partidas (IPP)

Pipeline em R que (1) reconstrói as viagens efetivamente realizadas a partir das
posições do endpoint `/Posicao` da API Olho Vivo, (2) estima o horário de passagem
de cada ônibus em cada ponto e (3) compara as partidas observadas com a programação
da SPTrans (tolerância de ±3 min do IPP, IQT.GSO.002.03).

## Como rodar

```bash
cd projeto_onibus
Rscript run_pipeline.R                 # ~1,5 min para as duas linhas de teste
quarto render relatorios/relatorio.qmd # relatório HTML
```

Requisitos: R ≥ 4.3, `data.table`, `arrow`, `sf`, `lubridate`, `ggplot2`, `Rcpp`
(com compilador C++), `dplyr`; `quarto` + `knitr` para o relatório.

Todos os caminhos e parâmetros ficam em `R/00_config.R`. A pasta dos dados brutos
é, por padrão, `x_dados/` na raiz do repositório (fora do git); para mudar, defina a
variável de ambiente `OLHOVIVO_DADOS_DIR`.

## Para rodar o sistema inteiro / outros meses

- `cfg$entrada$posicoes`: vetor de arquivos `.parquet` (ou pasta de dataset) com
  todas as linhas. O pipeline lê **uma linha por vez**, com filtro do arrow.
- `cfg$linhas = NULL`: processa todas as linhas encontradas. `cfg$n_cores`: linhas
  em paralelo.
- `cfg$rodar_sensibilidade = FALSE` e `cfg$gravar_arquivos_unicos = FALSE` em
  produção.
- As saídas detalhadas são datasets particionados
  `dados/processados/<tabela>/mes=AAAA-MM/linha=<linha>/part-0.parquet`. Rodar de
  novo uma linha/mês sobrescreve só a sua partição. As passagens de **um ponto**,
  com todas as linhas e meses acumulados, se obtêm com:

  ```r
  arrow::open_dataset("dados/processados/passagens_pontos") |>
    dplyr::filter(stop_id == "610016906") |> dplyr::collect()
  ```

## Nova raspagem de horários

Rode `../coleta_horarios_programados/coletar_linhas_sptrans.R` (ver o README de lá). Cada coleta
vira uma partição `linhas_collect/api/partidas/data_coleta=AAAA-MM-DD/` e é lida
automaticamente (`cfg$entrada$coletas_api_dir`; restrinja com `coletas_api_datas`).
As raspagens antigas via navegador (`cfg$entrada$raspagens_pastas`) continuam
aceitas; nelas a tabela de domingo é cópia da de dia útil e é descartada
(`D_identica_U`).

Para cada data de serviço × linha × sentido, o pipeline usa a coleta mais recente
com `data_base` ≤ data cuja tabela daquele tipo de dia seja válida; se não houver,
a mais próxima posterior, sinalizada como `raspagem_posterior_a_data`. Assim, em
setembro/2026 dias úteis e sábados vêm da raspagem de 25/10 e domingos/feriados
da coleta de 04/11.

## Relatório de cumprimento por linha

```bash
Rscript relatorios/gerar_relatorios_linhas.R            # todas as linhas × meses auditados
Rscript relatorios/gerar_relatorios_linhas.R 2026-09    # um mês
Rscript relatorios/gerar_relatorios_linhas.R 2026-09 701A-10
```

Gera `relatorios/linhas/<mes>/<linha>.html` (arquivo único, abre offline). Conteúdo:
- **indicadores** (primeira linha de quadros): IPP (a) oficial, IPP (b) com as saídas
  pontuais que não completaram (sem as partidas espúrias) e as inconclusivas, IPP (c) como
  o (b) com ±4 min, ICV e ICV ampliado (ver *ICV* abaixo);
- **grade de cumprimento**: dia × horário da tabela, por sentido e tipo de dia, colorida
  pela classificação (tooltip com a partida observada, o veículo e a evidência);
- não cumprimento por hora do dia;
- **horários mais pulados**: quantas vezes no mês cada horário ficou sem viagem e em quais dias;
- **dias com horários não cumpridos**: totais e lista dos horários não realizados;
- **ICV por período** (madrugada, antes do pico, picos, entre picos, noite);
- **veículos por dia**: quantos e quais operaram, com as viagens completas de cada um e
  a marca † para os que pararam de transmitir enquanto havia saídas que poderiam ter feito;
- **veículos e lacunas de GPS**: para cada saída inconclusiva, não realizada ou sem viagem
  completa, se algum ônibus sem sinal poderia tê-la feito (ver abaixo).

A legenda da grade funciona como filtro, e as tabelas têm filtro e ordenação quando o
visualizador permite JavaScript. Sem JavaScript, o conteúdo aparece completo do mesmo jeito.

### Diagnóstico de frota (`R/12_frota.R`)

Para cada lacuna de transmissão (> 10 min) de um veículo em serviço, registra onde ele
sumiu e onde reapareceu (km no ciclo) e quantas viagens completas caberiam no intervalo,
andando como uma viagem rápida do dia (`cfg$frota`). Com isso, cada saída sem partida
observada recebe um destes diagnósticos: *sem dados da linha*, *nenhum ônibus sem GPS
poderia tê-la feito* (indício forte de não realização), *possível viagem sem GPS* (no
horário ou só com atraso), ou, para saídas que partiram e não completaram, *conclusão
compatível com lacuna de GPS*. Uma lacuna em que cabem k viagens explica no máximo k saídas.
Um veículo com 2 ou mais lacunas desse tipo no dia é marcado como *GPS intermitente
recorrente*, a conferir na bilhetagem. O diagnóstico também ajusta o status final (ver
*Regra de auditoria ipp-0.2.0*).
Saídas: `diagnostico_saidas/`, `lacunas_veiculos/` e `veiculos_dia_frota/` (particionados).

### Regra de auditoria ipp-0.2.0

- **Partida espúria**: viagem incompleta que cobre menos de `cfg$ipp$parcial_espuria_max`
  (15%) do traçado é descartada como evidência de partida (GPS do ônibus parado no terminal
  associado a um viário próximo). A saída é reavaliada sem ela.
- **Quase completa** (`viagem_quase_completa`): ≥ 95% do traçado e cobertura ≥ 70%, sem
  chegada registrada. Não entra no IPP oficial; entra no IPP (b)/(c) e no ICV ampliado.
- **Veículo que parou de transmitir**: o último sinal do dia no itinerário abre uma lacuna
  de até `cfg$ipp$janela_veiculo_sumiu_h` (4 h). Se o modelo de frota mostra que ele
  conseguiria chegar à origem a tempo, a saída fica inconclusiva.
- **Conciliação com a frota** (`reconciliar_status`): "não realizada" para a qual algum
  ônibus sem GPS poderia ter feito a viagem passa a inconclusiva; inconclusiva que dependia
  só de um veículo que sumiu, mas não chegaria a tempo, volta a não realizada.

### ICV (`R/13_icv.R`)

Ficha SPTrans IQT.GSO.001.05: viagens completas por faixa horária e sentido, limitadas ao
programado, agrupadas nos períodos APM/PM/EP/PT/PN/PPN; dia = média dos períodos; mês =
média dos dias; linha = média dos sentidos (circular: sem sentido). Saídas:
`icv_faixas/` (particionado) e `icv_mensal.csv`.

Funções em `R/11_relatorio_linha.R`; modelo em `relatorios/relatorio_linha.qmd`.

## App de auditoria (Shiny)

```bash
Rscript app_auditoria/rodar.R          # http://127.0.0.1:8766  (--porta N para mudar)
```

Escolha a linha e **um ou mais dias**, e marque horários na grade. A grade e as viagens
listam todos os dias escolhidos; o mapa, o diagrama e os veículos mostram o *dia em foco*,
que muda sozinho quando a seleção é de um único dia. O app mostra:
- **mapa**: itinerário, paradas e ônibus no instante escolhido, com rastro e cor pelo
  sentido inferido. Os ônibus das saídas selecionadas ficam destacados e os registros
  descartados aparecem como anéis vermelhos;
- **controle de tempo** com ▶ (play) e passo ajustável;
- **diagrama espaço-tempo**: km ao longo do itinerário × hora para todos os ônibus, com as
  saídas programadas (▼, cor = classificação) ligadas à partida associada. Clique para
  mover o mapa até aquele instante;
- **painel de classificação**: status, regra aplicada, partida observada, viagem, duração
  observada e estimada, cobertura, flags e diagnóstico de frota;
- aba **veículos do dia em foco**: linha do tempo de cada ônibus (viagens, lacunas de GPS e
  saídas sem partida) e tabela de lacunas por veículo;
- abas com a **grade horária do dia**, as **viagens reconstruídas** (também selecionáveis) e
  os **registros GPS brutos** dos ônibus destacados (sl, sentido inferido, progresso,
  distância ao traçado, quebras, flags de QC).

O app lê sob demanda os mesmos Parquet particionados do pipeline (`mes=/linha=`). Cada
consulta abre uma partição e, dentro dela, só os row groups do dia, porque as posições são
gravadas ordenadas por dia em blocos de 50 mil linhas. Isso leva cerca de 0,3 s por
linha-dia e não cresce com o acúmulo de meses. Requisitos: `shiny`, `bslib`, `leaflet`,
`reactable`.

## Estrutura

| Script | Etapa |
|---|---|
| `R/00_config.R` | caminhos e parâmetros |
| `R/01_normalizacao_qc.R` | inventário, leitura por linha, fuso, dia de serviço (corte 03h), sinalizações de QC |
| `R/02_referencia.R` | GTFS → "ciclo" fechado da linha; projeção ordenada das paradas; escolha da versão do GTFS |
| `R/03_mapmatching.R` + `src/mapmatch.cpp` | candidatos ao longo do ciclo e Viterbi (C++) |
| `R/04_viagens.R` | segmentação em viagens, eventos de partida/chegada, classificação |
| `R/05_passagens.R` | horário de passagem por ponto |
| `R/06_programacao.R` | leitura das raspagens e normalização das saídas programadas |
| `R/07_auditoria.R` | associação global programada × observada, status de cada saída, IPP |
| `R/08_indicadores.R` | indicadores por hora |
| `R/09_sensibilidade.R` | cenários alternativos |
| `R/10_exportacao.R` | gravação, verificações automáticas, gráficos, traçados em lat/lon |
| `R/11_relatorio_linha.R` | grade de cumprimento e tabelas do relatório por linha |
| `app_auditoria/` | app Shiny de auditoria visual (`dados.R` = camada de consulta) |
| `run_pipeline.R` | execução completa |

**Por que data.table + Rcpp e não só tidyverse:** o map-matching (Viterbi) e o
alinhamento das partidas são laços sequenciais por veículo, que não vetorizam bem
em R. Em C++ levam ~4 s por linha-mês, contra vários minutos em R puro. O restante
usa data.table pelo volume (sistema inteiro ≈ dezenas de milhões de registros por
mês).

## Principais saídas (`dados/processados/`)

- `posicoes_normalizadas/`: todos os registros brutos padronizados, com flags `qc_*` e resultado do map-matching
- `viagens_reconstruidas/` (+ `.parquet`): uma linha por viagem candidata
- `passagens_pontos/`: uma linha por viagem × parada
- `trajetorias_reconstruidas/`: cadeias contínuas de map-matching
- `referencia_tracados/`, `referencia_paradas/`: traçado (lat/lon, km no ciclo) e paradas usados em cada linha × mês
- `programacao_normalizada/`, `auditoria_saidas/` (+ `.parquet`): uma linha por saída programada
- CSVs: `inventario_dados`, `qualidade_dados`, `indicadores_horarios`, `veiculos_por_hora`, `veiculos_dia`,
  `ranking_linhas`, `ranking_linhas_horario`, `ipp_diario*`, `ipp_mensal_por_sentido`, `saidas_suspeitas`,
  `saidas_inconclusivas`, `viagens_sem_associacao`, `indicadores_gerais`, `validacao_sensibilidade`,
  `parametros_execucao`, `versoes_pacotes`, `verificacoes_consistencia`, `diagnostico_sentido_sl`,
  `gtfs_escolha_por_linha`, `referencia_paradas`, `referencia_sentidos`
- `graficos/*.png`
