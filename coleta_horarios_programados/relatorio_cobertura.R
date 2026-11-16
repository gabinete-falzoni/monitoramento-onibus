#!/usr/bin/env Rscript
################################################################################
# relatorio_cobertura.R — compara a coleta da SPTrans com o GTFS
#
# Uso: Rscript relatorio_cobertura.R [--data AAAA-MM-DD] [--gtfs PASTA]
#                                    [--saida DIR_DADOS] [--antiga PASTA_RASPAGEM]
# Padrões (DADOS = $OLHOVIVO_DADOS_DIR ou <raiz do repositório>/x_dados):
# coleta mais recente; GTFS mais recente em DADOS/gtfs;
# raspagem antiga (navegador) em DADOS/linhas_collect/2026-10-25, se existir.
# Grava relatorio_cobertura_<data>.md ao lado deste script.
################################################################################
suppressPackageStartupMessages({ library(data.table); library(arrow) })

args_cli <- commandArgs(trailingOnly = TRUE)
arg <- function(n, p = NULL) { i <- match(paste0("--", n), args_cli); if (is.na(i)) p else args_cli[i + 1] }
dir_script <- local({
  f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
  if (length(f)) dirname(normalizePath(f[1])) else getwd()
})
raiz  <- Sys.getenv("OLHOVIVO_DADOS_DIR", file.path(dirname(dir_script), "x_dados"))
saida <- arg("saida", file.path(raiz, "linhas_collect", "api"))
datas <- sub("data_coleta=", "", list.files(file.path(saida, "linhas")))
data_c <- arg("data", max(datas))
dir_gtfs <- arg("gtfs", max(list.dirs(file.path(raiz, "gtfs"), recursive = FALSE)))
dir_antiga <- arg("antiga", file.path(raiz, "linhas_collect", "2026-10-25"))

ler <- function(tb) {
  f <- file.path(saida, tb, paste0("data_coleta=", data_c), "part-0.parquet")
  if (file.exists(f)) as.data.table(read_parquet(f)) else data.table()
}
lin <- ler("linhas"); par <- ler("partidas")
cat_site <- as.data.table(jsonlite::fromJSON(paste(readLines(gzfile(
  file.path(saida, "bruto", data_c, "_catalogo.json.gz"), encoding = "UTF-8")), collapse = "")))
cat_site[, codigo := trimws(codigo)]

# --- GTFS ----------------------------------------------------------------------
rt <- fread(file.path(dir_gtfs, "routes.txt"), colClasses = "character")
tr <- fread(file.path(dir_gtfs, "trips.txt"), colClasses = "character")
fq <- fread(file.path(dir_gtfs, "frequencies.txt"), colClasses = "character")
# Metrô/CPTM: no GTFS "METRÔ L1", no site "METRÔ L1-0"
norm <- function(x) sub("-0$", "", x)
lin[, rota := norm(codigo)]; cat_site[, rota := norm(codigo)]

g <- tr[, .(service_id = paste(sort(unique(service_id)), collapse = "|"),
            n_dir_gtfs = uniqueN(direction_id)), by = route_id]
g <- merge(rt[, .(route_id, route_type)], g, by = "route_id", all.x = TRUE)

l <- lin[, .(rota, codigo, erro_api, opera_U, opera_S, opera_D,
             n_U = n_partidas_U, circular, exibida_em)]
ns <- par[, .(n_sent_site = uniqueN(sentido)), by = codigo]
l <- merge(l, ns, by = "codigo", all.x = TRUE)
l[is.na(n_sent_site), n_sent_site := 0L]

m <- merge(g, l, by.x = "route_id", by.y = "rota", all = TRUE)
m[, situacao := fcase(
  is.na(codigo) & !route_id %in% cat_site$rota, "GTFS: ausente do catálogo do site",
  is.na(codigo), "GTFS: no catálogo, mas não coletada",
  !is.na(erro_api), "erro na API/parse",
  is.na(route_type), "site: ausente do GTFS",
  !(opera_U | opera_S | opera_D), "coletada, sem partidas no site",
  default = "coletada com partidas")]

# Dias de operação: GTFS (service_id) x site
m[, gtfs_U := grepl("U", service_id)]; m[, gtfs_S := grepl("S", service_id)]
m[, gtfs_D := grepl("D", service_id)]
ok_cmp <- m[situacao == "coletada com partidas" & !is.na(service_id)]
dif_dias <- ok_cmp[gtfs_U != opera_U | gtfs_S != opera_S | gtfs_D != opera_D,
                   .(route_id, gtfs = service_id,
                     site = paste0(fifelse(opera_U, "U", "_"), fifelse(opera_S, "S", "_"), fifelse(opera_D, "D", "_")))]

# Partidas em dia útil: GTFS (frequencies, que descreve o dia útil) x site
fq[, `:=`(ini = as.numeric(substr(start_time, 1, 2)) * 3600 + as.numeric(substr(start_time, 4, 5)) * 60,
          fim = as.numeric(substr(end_time, 1, 2)) * 3600 + as.numeric(substr(end_time, 4, 5)) * 60 + 60)]
fq[, n := (fim - ini) / as.numeric(headway_secs)]
fq <- merge(fq, tr[, .(trip_id, route_id)], by = "trip_id")
gu <- fq[, .(n_U_gtfs = round(sum(n))), by = route_id]
cmpU <- merge(gu, ok_cmp[opera_U == TRUE, .(route_id, n_U)], by = "route_id")
cmpU[, razao := n_U / n_U_gtfs]

# Correção da falha: D igual a U (antiga) x nova
assin <- function(d) d[, .(a = paste(sort(hora), collapse = ",")), by = .(codigo, sentido, tipo_dia)]
an <- dcast(assin(par), codigo + sentido ~ tipo_dia, value.var = "a")
nova_DU <- an[!is.na(D) & !is.na(U), .(n = .N, iguais = sum(D == U), iguais_USD = sum(D == U & !is.na(S) & S == U))]
antiga_DU <- NULL
if (dir.exists(file.path(dir_antiga, "partidas"))) {
  fs <- list.files(file.path(dir_antiga, "partidas"), pattern = "_[01]_[USD]\\.csv$", full.names = TRUE)
  mm <- regmatches(basename(fs), regexec("^(.+?)_(.+)_([01])_([USD])\\.csv$", basename(fs)))
  pa <- rbindlist(lapply(seq_along(fs), function(i) {
    if (file.size(fs[i]) == 0) return(NULL)
    d <- fread(fs[i], sep = ";", select = "hora", colClasses = "character")
    if (nrow(d)) data.table(codigo = mm[[i]][3], sentido = as.integer(mm[[i]][4]), tipo_dia = mm[[i]][5], hora = d$hora)
  }))
  aa <- dcast(assin(pa), codigo + sentido ~ tipo_dia, value.var = "a")
  antiga_DU <- aa[!is.na(D) & !is.na(U), .(n = .N, iguais = sum(D == U), iguais_USD = sum(D == U & !is.na(S) & S == U))]
  # tabelas D na antiga para linhas/sentidos que, segundo a API, não operam no domingo
  D_fantasma <- nrow(aa[!is.na(D)][!an[!is.na(D)], on = .(codigo, sentido)])
  antigas_linhas <- uniqueN(pa$codigo)
  # mudança de quantidade de partidas em U entre as duas coletas
  nU_ant <- pa[tipo_dia == "U", .N, by = codigo]; nU_nov <- par[tipo_dia == "U", .N, by = codigo]
  cmp_ant <- merge(nU_ant, nU_nov, by = "codigo", suffixes = c("_ant", "_nov"))
}

# --- Relatório -----------------------------------------------------------------
tab <- function(d) {
  if (!nrow(d)) return("_(nenhuma)_\n")
  d <- as.data.frame(d)
  paste0(c(paste0("| ", paste(names(d), collapse = " | "), " |"),
           paste0("|", paste(rep("---", ncol(d)), collapse = "|"), "|"),
           apply(d, 1, function(r) paste0("| ", paste(r, collapse = " | "), " |"))), collapse = "\n")
}
cnt <- m[, .N, by = situacao][order(-N)]
log_f <- list.files(file.path(saida, "bruto", data_c), "^_log_.*csv$", full.names = TRUE)
nao_coletadas <- m[situacao %in% c("GTFS: ausente do catálogo do site", "GTFS: no catálogo, mas não coletada",
                                   "erro na API/parse"), .(route_id, situacao, erro_api)]
sem_partidas <- m[situacao == "coletada, sem partidas no site", .(route_id, route_type)]

out <- c(
  sprintf("# Cobertura da coleta SPTrans — %s", data_c), "",
  sprintf("- GTFS de referência: `%s` (%d rotas em routes.txt)", basename(dir_gtfs), nrow(rt)),
  sprintf("- Catálogo do site: %d linhas; coletadas: %d; com partidas: %d",
          nrow(cat_site), nrow(lin), lin[opera_U | opera_S | opera_D, .N]),
  sprintf("- Partidas coletadas: %s; trechos de itinerário: %s",
          format(nrow(par), big.mark = " "), format(nrow(ler("itinerarios")), big.mark = " ")), "",
  "## Situação de cada rota (GTFS ∪ site)", "", tab(cnt), "",
  "### Rotas do GTFS não coletadas", "", tab(nao_coletadas), "",
  "### Coletadas, mas o site não publica partidas", "",
  "(route_type 1/2 = metrô/trem: o site lista as linhas mas não traz horários)", "", tab(sem_partidas), "",
  "## Dias de operação: GTFS × site", "",
  sprintf("Linhas com partidas no site e com viagens (trips.txt) no GTFS: %d (as demais %d com partidas não têm trips no GTFS).",
          nrow(ok_cmp), m[situacao == "coletada com partidas" & is.na(service_id), .N]), "",
  sprintf("%d de %d linhas com partidas têm os mesmos dias (U/S/D) nos dois. Divergências:",
          nrow(ok_cmp) - nrow(dif_dias), nrow(ok_cmp)), "", tab(dif_dias), "",
  "## Partidas em dia útil: GTFS (frequencies.txt) × site", "",
  sprintf("Em %d linhas comparáveis, a razão site/GTFS tem mediana %.2f; %d linhas (%.1f%%) ficam entre 0,8 e 1,25.",
          nrow(cmpU), median(cmpU$razao), cmpU[razao >= 0.8 & razao <= 1.25, .N],
          100 * cmpU[razao >= 0.8 & razao <= 1.25, .N] / nrow(cmpU)),
  "O GTFS da SPTrans descreve frequências por faixa horária, não horários exatos; a comparação é só ordem de grandeza.", "",
  "## Correção da falha dos domingos", "",
  if (!is.null(antiga_DU)) sprintf("- Raspagem antiga (`%s`, navegador): %d de %d tabelas de domingo idênticas à de dia útil (%d linhas coletadas).",
                                   basename(dir_antiga), antiga_DU$iguais, antiga_DU$n, antigas_linhas),
  if (!is.null(antiga_DU)) sprintf("  Nessas, o sábado era igual ao dia útil em só %d casos — a tabela D era cópia da U. Além disso, a antiga tem %d tabelas D para linha/sentido que não operam aos domingos.",
                                   antiga_DU$iguais_USD, D_fantasma),
  sprintf("- Coleta nova (API): %d de %d tabelas de domingo idênticas à de dia útil; em todas elas o sábado também é idêntico (%d), ou seja, linhas com a mesma programação nos três dias, não erro de coleta.",
          nova_DU$iguais, nova_DU$n, nova_DU$iguais_USD),
  if (!is.null(antiga_DU)) sprintf("- Partidas de dia útil por linha, antiga × nova: idênticas em %d de %d linhas em comum.",
                                   cmp_ant[N_ant == N_nov, .N], nrow(cmp_ant)),
  "")
arq <- file.path(dir_script, sprintf("relatorio_cobertura_%s.md", data_c))
writeLines(out, arq)
fwrite(m, file.path(dir_script, sprintf("cobertura_detalhe_%s.csv", data_c)))
cat("Relatório:", arq, "\n")
