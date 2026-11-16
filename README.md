# Monitoramento de ônibus — São Paulo (SPTrans / Olho Vivo)

Ferramentas em R para verificar se as partidas programadas das linhas de ônibus de
São Paulo foram de fato realizadas. O projeto junta três fontes:

- **posições GPS** dos ônibus, coletadas da API Olho Vivo da SPTrans;
- **programação** das linhas (horários de partida por tipo de dia), coletada da
  página de itinerários da SPTrans;
- **GTFS** da SPTrans (traçados e paradas).

A partir delas, reconstrói as viagens realizadas, estima a passagem por cada ponto e
compara as partidas observadas com as programadas (IPP, ±3 min; ICV), gerando
relatórios por linha e um app Shiny para auditoria visual.

## Estado atual

Projeto em desenvolvimento. O pipeline foi validado com **duas linhas de teste
(`701A-10` e `7267-10`) em setembro de 2026**. Os resultados processados dessas duas
linhas estão no repositório (`projeto_onibus/dados/processados/`), de modo que o app e
os relatórios podem ser abertos sem rodar o pipeline. Rodar para o sistema inteiro
ainda depende de ajustes de configuração (ver `projeto_onibus/README.md`).

## Estrutura

```
coleta_gps_olho_vivo/          coleta contínua das posições GPS (API Olho Vivo)
  R/coletor_de_posicoes_v3.R   coletor em uso
  R/old/                       versões e testes antigos (histórico; não usados)
coleta_horarios_programados/   coleta da programação das linhas (API de itinerários da SPTrans)
  coletar_linhas_sptrans.R     coleta
  relatorio_cobertura.R        compara a coleta com o GTFS
projeto_onibus/                pipeline de reconstrução de viagens e auditoria
  run_pipeline.R               execução completa
  R/                           etapas do pipeline (00_config.R = caminhos e parâmetros)
  src/mapmatch.cpp             map-matching em C++ (Rcpp)
  dados/processados/           saídas (Parquet particionado mes=/linha= + CSVs)
  relatorios/                  relatório geral (Quarto) e relatórios por linha
  app_auditoria/               app Shiny de auditoria
x_dados/                       dados brutos — NÃO versionado (ver abaixo)
```

Cada pasta tem o próprio README com detalhes.

## Requisitos

- R ≥ 4.3 e um compilador C++ (para o Rcpp).
- Pacotes R:

  ```r
  install.packages(c(
    "data.table", "arrow", "sf", "lubridate", "ggplot2", "dplyr", "Rcpp",  # pipeline
    "shiny", "bslib", "leaflet", "reactable", "htmltools",                 # app
    "knitr", "svglite",                                                    # relatórios
    "httr2", "jsonlite",                                                   # coleta de horários
    "tidyverse", "httr", "glue"                                            # coletor GPS
  ))
  ```

- [Quarto](https://quarto.org) para renderizar os relatórios.

## Abrir o app de auditoria (Shiny)

O app lê os dados já processados em `projeto_onibus/dados/processados/`, então funciona
logo após clonar o repositório:

```bash
cd projeto_onibus
Rscript app_auditoria/rodar.R              # abre em http://127.0.0.1:8766
Rscript app_auditoria/rodar.R --porta 9000 # outra porta
```

Depois abra o endereço no navegador. No RStudio, também dá para abrir
`projeto_onibus/app_auditoria/rodar.R` e executar com *Source* (o diretório de trabalho
deve ser `projeto_onibus`).

No app, escolha a linha e um ou mais dias e marque horários na grade. Ele mostra o mapa
com itinerário, paradas e ônibus no instante escolhido, o diagrama espaço-tempo, a
classificação de cada saída programada, a linha do tempo dos veículos e os registros GPS
brutos. Detalhes em `projeto_onibus/README.md`.

## Gerar os relatórios

Também usam apenas `dados/processados/`:

```bash
cd projeto_onibus
Rscript relatorios/gerar_relatorios_linhas.R            # um HTML por linha × mês
Rscript relatorios/gerar_relatorios_linhas.R 2026-09 701A-10
quarto render relatorios/relatorio.qmd                  # relatório geral
```

Saídas em `relatorios/linhas/<mes>/<linha>.html` (arquivos únicos, abrem offline).

## Rodar o pipeline (reprocessar os dados)

Precisa dos dados brutos, que **não estão no repositório**. Por padrão os scripts os
procuram em `x_dados/`, na raiz do repositório; para usar outra pasta, defina a variável
de ambiente `OLHOVIVO_DADOS_DIR`. Estrutura esperada:

```
x_dados/
  linha_7267_10_setembro.parquet      posições GPS (/Posicao) das linhas de teste
  linha_701A_10_setembro.parquet
  gtfs/20261104/, gtfs/20260226/      GTFS da SPTrans (txt descompactados)
  linhas_collect/api/                 coletas de coletar_linhas_sptrans.R
  linhas_collect/2026-10-25/          raspagem antiga da programação (navegador)
  token_olho_vivo.txt                 token da API Olho Vivo (só para o coletor GPS)
```

Os arquivos de entrada e as linhas a processar são definidos em
`projeto_onibus/R/00_config.R`. Para rodar:

```bash
cd projeto_onibus
Rscript run_pipeline.R
```

## Coletar dados

**Programação das linhas** (sem credenciais; ~13 min para todas as linhas):

```bash
cd coleta_horarios_programados
Rscript coletar_linhas_sptrans.R     # grava em x_dados/linhas_collect/api/
Rscript relatorio_cobertura.R        # compara com o GTFS mais recente em x_dados/gtfs/
```

**Posições GPS** (precisa de um token da [API Olho Vivo](https://www.sptrans.com.br/desenvolvedores/)):

```bash
export OLHOVIVO_TOKEN="seu_token"    # ou salve em x_dados/token_olho_vivo.txt
Rscript coleta_gps_olho_vivo/R/coletor_de_posicoes_v3.R
```

O coletor consulta a API a cada 30 s e grava um `.parquet` por consulta em
`x_dados/gps_collect/` (ou em `OLHOVIVO_SAIDA_GPS`). Roda indefinidamente; o ideal é
deixá-lo num servidor ou sessão `tmux`/`screen`.

## Licença

Ver [LICENSE](LICENSE).
