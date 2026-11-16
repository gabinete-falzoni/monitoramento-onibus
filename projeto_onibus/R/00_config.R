############################################################
# 00_config.R — caminhos e parâmetros centralizados
#
# Nenhum outro script contém caminhos fixos: tudo vem daqui.
# Para rodar para outras linhas/meses, altere `cfg$entrada` e
# `cfg$linhas` (NULL = todas as linhas presentes nos dados).
############################################################

`%||%` <- function(a, b) if (is.null(a)) b else a

# Raiz do projeto (pasta projeto_onibus). run_pipeline.R define PROJ_DIR;
# se não definido, assume-se que o diretório de trabalho é a raiz.
if (!exists("PROJ_DIR")) PROJ_DIR <- normalizePath(".")
# Pasta com os dados brutos compartilhados: <raiz do repositório>/x_dados
# (fora do git). Para usar outra pasta, defina OLHOVIVO_DADOS_DIR.
DADOS_DIR <- Sys.getenv("OLHOVIVO_DADOS_DIR",
                        normalizePath(file.path(PROJ_DIR, "..", "x_dados"), mustWork = FALSE))

cfg <- list(
  algorithm_version = "mm-ciclo-0.1.0",
  audit_rule_version = "ipp-0.2.0",

  # ---------------------------------------------------------------- entradas
  entrada = list(
    # Arquivos (ou pastas de dataset) .parquet de posições do /Posicao.
    # Pode ser um vetor de arquivos ou uma pasta particionada.
    posicoes = c(file.path(DADOS_DIR, "linha_7267_10_setembro.parquet"),
                 file.path(DADOS_DIR, "linha_701A_10_setembro.parquet")),
    # GTFS: o pipeline escolhe, por linha, a versão cujo traçado melhor
    # explica os pontos GPS (ver 02_referencia.R). Ordem = preferência em empate.
    gtfs_dirs = c(file.path(DADOS_DIR, "gtfs", "20261104"),
                  file.path(DADOS_DIR, "gtfs", "20260226")),
    # Programação da SPTrans (ver 06_programacao.R). Para cada data de serviço,
    # usa-se a coleta válida mais próxima entre todas as configuradas.
    # (a) Coletas da API (coleta_horarios_programados/coletar_linhas_sptrans.R): todas as
    #     partições data_coleta= encontradas, ou só as datas em coletas_api_datas.
    coletas_api_dir = file.path(DADOS_DIR, "linhas_collect", "api"),
    coletas_api_datas = NULL,
    # (b) Raspagens antigas via navegador (uma subpasta por data_base); a tabela
    #     D delas é cópia da U e é descartada automaticamente.
    raspagens_dir = file.path(DADOS_DIR, "linhas_collect"),
    raspagens_pastas = c("2026-10-25")
  ),

  # ---------------------------------------------------------------- saídas
  saida_dir = file.path(PROJ_DIR, "dados", "processados"),
  relatorio_dir = file.path(PROJ_DIR, "relatorios"),

  # NULL = todas as linhas encontradas; ou vetor de letreiros (coluna `c`)
  linhas = NULL,
  n_cores = 2L,
  rodar_sensibilidade = TRUE,        # custoso no sistema inteiro: desligar em produção
  gravar_arquivos_unicos = TRUE,     # além dos datasets particionados, grava .parquet únicos
                                     # de viagens e auditoria (útil para poucas linhas)

  # ---------------------------------------------------------------- tempo
  tz_local = "America/Sao_Paulo",
  # Dia operacional: começa às 03:00 locais (partidas 00:10, 00:30 pertencem
  # ao dia de serviço anterior). Mesmo corte usado nos horários programados.
  hora_corte_dia = 3,

  # ---------------------------------------------------------------- QC
  bbox_sp = c(lon_min = -47.10, lon_max = -46.30, lat_min = -24.05, lat_max = -23.30),
  crs_metrico = 31983,               # SIRGAS 2000 / UTM 23S

  # ---------------------------------------------------------------- map matching
  mm = list(
    raio_candidato_m = 120,          # distância máxima GPS–traçado para ser "na rota"
    fundir_candidatos_m = 40,        # candidatos mais próximos que isso ao longo do ciclo viram um
    max_candidatos = 6,
    sigma_gps_m = 25,                # desvio do erro GPS (emissão)
    beta_m = 80,                     # escala da diferença distância-na-rota x distância reta
    vmax_ms = 70 / 3.6,              # velocidade máxima plausível de ônibus
    folga_m = 150,                   # folga somada a vmax*dt
    recuo_tol_m = 60,                # recuo tolerado (ruído GPS parado)
    lacuna_quebra_s = 30 * 60,       # lacuna de transmissão que quebra a cadeia (não se sabe quantas voltas)
    outlier_lookahead = 6,           # nº de pontos à frente para decidir se um salto é espúrio (~4,5 min)
    penal_sl = 0                     # penalidade (unid. de custo) se o candidato não bate com `sl`
  ),

  # ---------------------------------------------------------------- viagens
  viagem = list(
    lacuna_interp_s = 300,           # pares de pontos com dt <= isso contam como cobertos
                                     # (300 s = critério de "sem transmissão" do IDTA)
    lacuna_evento_s = 180,           # dt máximo do par usado para datar partida/chegada
    offset_partida_m = 50,           # partida = cruzar a posição terminal + offset
    offset_chegada_m = 150,          # chegada = cruzar fim do sentido - offset
    # Quando o par de pontos que cruza o limiar tem lacuna (ex.: ônibus fora do
    # traçado dentro do terminal), mas o ponto do lado de fora do terminal está a
    # até `janela_terminal_m` do limiar, estima-se o instante pela velocidade de
    # referência de manobra (`vel_ref_terminal_ms`).
    janela_terminal_m = 500,
    vel_ref_terminal_ms = 15 / 3.6,
    sobreposicao_min_m = 1000,       # trecho mínimo observado para gerar viagem candidata
    cobertura_confirmada = 0.90,
    cobertura_provavel = 0.70,
    cobertura_parcial = 0.25,
    lacuna_max_confirmada_s = 600,
    vel_media_min_kmh = 4,
    vel_media_max_kmh = 45
  ),

  # ---------------------------------------------------------------- passagens nos pontos
  passagem = list(
    lacuna_max_s = 300               # acima disso a passagem é marcada como estimada sobre lacuna
  ),

  # ---------------------------------------------------------------- programação / IPP
  # diagnóstico de frota para saídas inconclusivas / não realizadas (R/12_frota.R)
  frota = list(
    lacuna_min_s = 600,              # lacuna de transmissão considerada (igual a ipp$lacuna_veiculo_s)
    lacuna_max_h = 4,                # lacunas mais longas = veículo fora de serviço
    q_duracao = 0.25,                # velocidade de referência = viagem rápida (quantil 25% da duração)
    folga_min = 10,                  # folga ao exigir que o veículo chegue onde reapareceu
    tol_pos_m = 300                  # tolerância de posição (terminal / "mesmo ponto")
  ),

  ipp = list(
    tolerancia_min = 3,              # IPP (SPTrans, IQT.GSO.002.03): ±3 min
    janela_antecipacao_min = 15,     # antecipação máxima para associar partida
    janela_atraso_min = 40,          # atraso máximo para associar partida
    custo_nao_assoc_prog = 15,       # custo (min) de deixar programada sem par
    custo_nao_assoc_obs = 15,        # custo (min) de deixar observada sem par
    # Detectabilidade: para afirmar "possivelmente não realizada", a linha deve ter
    # tido GPS em rota na janela [prog - x, prog + y]
    janela_detect_antes_min = 20,
    janela_detect_depois_min = 60,
    lacuna_veiculo_s = 600,          # lacuna (em rota) de um veículo em serviço que torna a saída inconclusiva
    # Veículo que PAROU de transmitir no itinerário e não voltou no dia: pode ter
    # seguido operando sem GPS. As saídas até x h depois do último sinal ficam
    # inconclusivas (não "não realizadas").
    janela_veiculo_sumiu_h = 4,
    # Partidas sem viagem completa, pela extensão do traçado percorrida (frac_observada):
    #  < parcial_espuria_max: partida espúria (o ônibus não saiu de fato: GPS associado a
    #    um trecho de viário perto do terminal). É descartada como evidência de partida.
    #  >= quase_completa_min (e cobertura >= quase_completa_cob_min): viagem quase completa
    #    (o GPS "salta" para o destino no fim). Não entra no IPP oficial.
    parcial_espuria_max = 0.15,
    quase_completa_min = 0.95,
    quase_completa_cob_min = 0.70,
    tolerancia_ampliada_min = 4,     # IPP (c): margem de segurança
    feriados = as.Date(c("2026-09-07", "2026-10-12", "2026-11-02", "2026-11-15",
                         "2026-11-20", "2026-12-25")),
    # Tipos de dia cuja tabela não deve ser auditada, em qualquer fonte
    # (ex.: c("D")). Vazio desde a coleta via API de 04/11/2026, cuja D é correta.
    tipos_dia_invalidos = character(0),
    # Se TRUE, nas raspagens via navegador, uma tabela D idêntica à U é marcada
    # inválida (D_identica_U). Na API, D == U é legítimo e não é marcado.
    detectar_D_igual_U = TRUE
  ),

  # ---------------------------------------------------------------- sensibilidade
  sensibilidade = list(
    raio_candidato_m = c(80, 120, 200),
    lacuna_interp_s = c(180, 300, 600),
    cobertura_confirmada = c(0.80, 0.90, 0.95),
    tolerancia_ipp_min = c(2, 3, 5)
  )
)

