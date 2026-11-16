# Coleta da programação das linhas — SPTrans

Substitui `coleta_gps_olho_vivo/R/old/y_coletar_horarios_linhas_teste_v6_helper.R`.

Os dados ficam em `x_dados/` na raiz do repositório (fora do git), ou na pasta
indicada pela variável de ambiente `OLHOVIVO_DADOS_DIR`.

## Executar

```bash
cd coleta_horarios_programados
Rscript coletar_linhas_sptrans.R        # coleta todas as linhas (~12 min)
Rscript relatorio_cobertura.R           # relatório comparando com o GTFS mais recente
```

Pacotes: `httr2`, `jsonlite`, `data.table`, `arrow`.

Opções de `coletar_linhas_sptrans.R`:

| opção | padrão | uso |
|---|---|---|
| `--saida DIR` | `../x_dados/linhas_collect/api` | onde gravar |
| `--intervalo SEG` | `0.5` | pausa mínima entre requisições |
| `--linhas A,B` | todas | só algumas linhas (ex.: `--linhas 701A-10,7267-10`) |
| `--refazer` | — | baixa de novo mesmo o que já foi baixado no dia |
| `--data AAAA-MM-DD` | hoje | rótulo da coleta (útil para reprocessar JSON de outro dia) |

Se a execução for interrompida, rode de novo no mesmo dia: ela retoma de onde
parou (linhas já baixadas são puladas). Com 20 falhas seguidas o script para
sozinho, para não insistir contra o servidor.

Para rodar periodicamente (ex.: toda segunda às 3h), via `crontab -e`:

```
0 3 * * 1 cd /caminho/para/monitoramento-onibus/coleta_horarios_programados && Rscript coletar_linhas_sptrans.R >> logs/coleta.log 2>&1 && Rscript relatorio_cobertura.R >> logs/coleta.log 2>&1
```

## Como funciona

A página `sptrans.com.br/itinerarios/linha/?numero=...` não traz os horários no
HTML. Ao abrir, ela pede tudo de uma vez, em JSON, a uma API pública
(`itinerariosapi.sptrans.com.br`) e depois só redesenha a tela quando se troca
dia ou sentido. O script consulta essa mesma API:

- 1 requisição para o catálogo (`RetornarLinhasTeste`);
- 1 requisição por linha (`BuscarDetalheLinhaTeste`), que já traz partidas,
  itinerários, eventos e tempos de **todos** os dias e sentidos.

São ~1.365 requisições pequenas (~50 KB cada), sequenciais, com 0,5 s entre
elas, contra ~1.300 carregamentos completos de página (dezenas de arquivos cada)
mais 4–6 trocas de seleção por linha no script antigo.

### A falha dos domingos

O script antigo lia a tela renderizada depois de trocar o dia via JavaScript e
esperar um tempo fixo (`Sys.sleep`). A tabela "D" era, na prática, a de dia útil
(idêntica em ~2.272 de 2.274 casos na raspagem de 25/10/2026). A coleta foi feita
num domingo, quando a própria página reseleciona o dia sozinha após carregar
(nos CSVs "D" o rótulo gravado foi "Domingo - 25 de outubro", não "Domingo");
o mais provável é que a troca de seleção do script e a da página tenham se
atropelado. Não dá para reproduzir num dia útil, mas a causa deixa de importar: lendo o JSON, o tipo de dia vem como campo
(`tipoDia` 0/1/2 = U/S/D) e não depende de cliques, esperas nem do dia da semana
em que se roda.

## Armazenamento

```
x_dados/linhas_collect/api/
  bruto/AAAA-MM-DD/<linha>.json.gz       resposta original da API (auditoria/reprocessamento)
  bruto/AAAA-MM-DD/_catalogo.json.gz
  bruto/AAAA-MM-DD/_log_HHMMSS.csv        status de cada requisição
  linhas/data_coleta=AAAA-MM-DD/part-0.parquet
  partidas/data_coleta=AAAA-MM-DD/part-0.parquet
  itinerarios/data_coleta=.../part-0.parquet
  eventos/...            tempos/...            horarios_operacao/...
```

Antes: ~20 mil CSVs (3 pastas × linha × sentido × dia) e milhares de parquets
minúsculos particionados por linha. Agora: uma tabela por tipo de informação,
um arquivo por coleta, todas as linhas juntas. Cada nova coleta acrescenta uma
partição `data_coleta=`; o histórico fica preservado e é lido de uma vez:

```r
library(arrow); library(dplyr)
partidas <- open_dataset("x_dados/linhas_collect/api/partidas") |>
  filter(codigo == "701A-10", data_coleta == "2026-11-04") |> collect()
```

### Tabelas

| tabela | chave | colunas principais |
|---|---|---|
| `linhas` | codigo | nome, área, consórcio, empresa, letreiros, `circular`, `opera_U/S/D`, `n_partidas_U/S/D`, `exibida_em`, `erro_api` |
| `partidas` | codigo, sentido, tipo_dia, ordem | `hora` (HH:MM), `faixa_horaria`, `veiculo_acessivel` |
| `itinerarios` | codigo, sentido, tipo_dia, ordem | `via`, `numeral`, `n_eventos` |
| `eventos` | codigo, sentido, tipo_dia, ordem | evento, descrição, tipo, periodicidade, dia da semana, horários, desvio |
| `tempos` | codigo, sentido, tipo_dia, periodo | `minutos` (manhã, entrepico, tarde) |
| `horarios_operacao` | codigo, tipo_dia | primeira–última partida em cada terminal |

Convenções: `sentido` 0 = ida (parte do terminal principal, `letreiro_ida` é o
destino), 1 = volta; `tipo_dia` U/S/D. `ordem` segue a ordem da página (partidas
após a meia-noite, "00:xx", vêm no fim). Em linhas circulares a página mostra um
só sentido; `circular = TRUE` usa a mesma regra da página.

Linhas `XXXX-1` (e `8700-24`) são atendimentos auxiliares que a página exibe
fundidos à linha principal (ex.: `8000-1` dentro de `8000-10`). Aqui cada uma
tem as próprias partidas; `exibida_em` indica a principal. Para reproduzir o que
o site mostra, una as partidas das duas.

Metrô e CPTM aparecem no catálogo (`METRÔ L1-0` etc.; no GTFS `METRÔ L1`) mas o
site não publica horários para eles.
