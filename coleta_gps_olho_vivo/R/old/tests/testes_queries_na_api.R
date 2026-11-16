# https://www.sptrans.com.br/desenvolvedores/api-do-olho-vivo-guia-de-referencia/documentacao-api/

library('tidyverse')
library('tidylog')
library('httr')
library('jsonlite')
library('arrow')

pasta_base <- '/mnt/fern/Dados/projetos/2026_Monitoramento_Olho_Vivo'
pasta_json <- sprintf('%s/json_posicao', pasta_base)
dir.create(pasta_json, recursive = TRUE, showWarnings = FALSE)


token <- sprintf('%s/token_olho_vivo.txt', pasta_base)
token <- trimws(readLines(token, warn = FALSE))

# token <- '<SEU_TOKEN_AQUI>'
# token <- '<SEU_TOKEN_AQUI>'

# 1) authenticate and get cookie
base_url <- "https://api.olhovivo.sptrans.com.br/v2.1"
login <- POST(paste0(base_url, "/Login/Autenticar?token=", token))
stop_for_status(login)

# check result
cat("Status:", login$status_code, "\n")
cat("Body:", content(login, "text"), "\n")  # should be "true"

# extract cookie
cookies_df <- httr::cookies(login)
print(cookies_df)

# find the API session cookie
api_cookie <- cookies_df$value[cookies_df$name == "apiCredentials"]

# # 2) use that cookie in the next request
# if (length(api_cookie) == 1) {
#   resp2 <- httr::GET(
#     paste0(base_url, "/Linha/Buscar?termosBusca=8000"),
#     httr::set_cookies(apiCredentials = api_cookie)
#   )
#   print(content(resp2, "text"))
# } else {
#   stop("Login did not return an apiCredentials cookie")
# }
#
#
# print(
#   content(
#   httr::GET(paste0(base_url, "/Linha/Buscar?termosBusca=478P-10"),
#   httr::set_cookies(apiCredentials = api_cookie))
# , 'text'))

# [int]cl
# Código identificador da linha. Este é um código identificador único de cada linha do sistema (por sentido de operação)
# [bool]lc
# Indica se uma linha opera no modo circular (sem um terminal secundário)
# [string]lt
# Informa a primeira parte do letreiro numérico da linha
# [int]tl
# Informa a segunda parte do letreiro numérico da linha, que indica se a linha opera nos modos: BASE (10), ATENDIMENTO (21, 23, 32, 41)
# [int]sl
# Informa o sentido ao qual a linha atende, onde 1 significa Terminal Principal para Terminal Secundário e 2 para Terminal Secundário para Terminal Principal
# [str]tp
# Informa o letreiro descritivo da linha no sentido Terminal Principal para Terminal Secundário
# [str]ts
# Informa o letreiro descritivo da linha no sentido Terminal Secundário para Terminal Principal




response <- httr::GET(paste0(base_url, "/Posicao"))
stop_for_status(response)
json <- fromJSON(
  content(response, "text", encoding = "UTF-8"),
  flatten = TRUE
)
vehicles <- json$l %>% unnest(vs)
vehicles
# vehicles %>% select(sv) %>% distinct()
# vehicles %>% select(is) %>% distinct()
# vehicles %>% filter(str_starts(c, '478P'))

vehicles %>% select(-c(lt0, lt1, qv, a, sv, is)) %>% filter(str_starts(c, '478P'))
# c          cl    sl     p ta                      py    px
# <chr>   <int> <int> <int> <chr>                <dbl> <dbl>
# 1 478P-10 33117     2 52001 2026-03-02T17:06:55Z -23.6 -46.6
# 2 478P-10 33117     2 52400 2026-03-02T17:06:35Z -23.6 -46.6
# 3 478P-10 33117     2 52053 2026-03-02T17:06:51Z -23.6 -46.7
# 4 478P-10 33117     2 52403 2026-03-02T17:06:37Z -23.5 -46.7
# 5 478P-10   349     1 52049 2026-03-02T17:07:00Z -23.6 -46.6
# 6 478P-10   349     1 52051 2026-03-02T17:06:56Z -23.5 -46.7
# 7 478P-10   349     1 52153 2026-03-02T17:06:33Z -23.6 -46.6
# 8 478P-10   349     1 52011 2026-03-02T17:06:34Z -23.6 -46.7
# 9 478P-10   349     1 52005 2026-03-02T17:06:35Z -23.6 -46.6

out_json <- sprintf('%s/%s', pasta_json, str_c(str_replace_all(str_sub(Sys.time(), 1, 19), '[-: ]', '_'), '.parquet'))
# write_delim(vehicles, out_json, delim = ';')
write_parquet(vehicles, out_json)

vehicles <- vehicles %>% select(-c(lt0, lt1, qv, a, sv, is))


linhas <- httr::GET(paste0(base_url, "/Linha/Buscar?termosBusca=478P"))
(linhas <- tibble(content(linhas, "parsed", simplifyVector = TRUE)))
# cl lc    lt       sl    tl tp         ts
# <int> <lgl> <chr> <int> <int> <chr>      <chr>
# 1   349 FALSE 478P      1    10 VL. ROMANA SACOMÃ
# 2 33117 FALSE 478P      2    10 VL. ROMANA SACOMÃ


response_2 <- httr::GET(paste0(base_url, "/Parada/BuscarParadasPorLinha?codigoLinha=33275"), httr::set_cookies(apiCredentials = api_cookie))
fromJSON(content(response_2, "text", encoding = "UTF-8"))

vehicles %>% filter(cl == '33275')

content(response_2, "parsed")
tibble(content(response_2, "parsed", simplifyVector = TRUE))

paradas <- httr::GET(paste0(base_url, "/Parada/Buscar?termosBusca=478P"), httr::set_cookies(apiCredentials = api_cookie))
paradas <- tibble(content(paradas, "parsed", simplifyVector = TRUE)) %>% mutate(np = ifelse(np == '', as.character(NA), np))
paradas %>% st_as_sf(coords = c('px', 'py'), crs = 4326) %>% ver_leaflet()





print(
  content(
    httr::GET(paste0(base_url, "/Parada/Buscar?termosBusca=349"),
              httr::set_cookies(apiCredentials = api_cookie))
    , 'text'))

print(
  content(
    httr::GET(paste0(base_url, "/Parada/BuscarParadasPorLinha?codigoLinha=478P"),
              httr::set_cookies(apiCredentials = api_cookie))
    , 'text'))



this <- httr::GET(paste0(base_url, "/Posicao"),
          httr::set_cookies(apiCredentials = api_cookie))
content(this, "text")

base_url <- 'https://api.olhovivo.sptrans.com.br/v2.1'
login_url <- paste0(base_url, '/Login/Autenticar?token=', token)
login_resp <- httr::POST(login_url)
cat("Status:", login_resp$status_code, "\n")
cat("Body:", content(login_resp, "text"), "\n")  # should be "true"

content(login_resp, "text")
if (status_code(login_resp) != 200) {
  stop("Authentication request failed (status ", status_code(login_resp), ")")
}
POST(
  "http://api.olhovivo.sptrans.com.br/v2.1/Login/Autenticar",
  query = list(token = token)
)


this <- httr::GET("https://api.olhovivo.sptrans.com.br/v2.1/Linha/Buscar?termosBusca=8000")
content(this, "text")


api_token <- trimws(readLines(token, warn = FALSE))

cat("Token length:", nchar(api_token), "\n")
cat("First 5 chars:", substr(api_token, 1, 5), "\n")

login_resp <- POST(
  "https://api.olhovivo.sptrans.com.br/v2.1/Login/Autenticar",
  query = list(token = api_token)
)

cat("Status:", status_code(login_resp), "\n")
cat("Response:", content(login_resp, "text"), "\n")



login_resp <- POST(
  "http://api.olhovivo.sptrans.com.br/v2.1/Login/Autenticar",
  body = list(token = api_token),
  encode = "form"
)

content(login_resp, "text")


base_url <- "https://api.olhovivo.sptrans.com.br/v2.1"
# api_token <- trimws(readLines(token, warn = FALSE))
api_token <- '<SEU_TOKEN_AQUI>'

h <- handle(base_url)

login_resp <- POST(
  url = paste0(base_url, "/Login/Autenticar"),
  query = list(token = api_token),
  handle = h
)

cat("Status:", status_code(login_resp), "\n")
cat("Response:", content(login_resp, "text"), "\n")
