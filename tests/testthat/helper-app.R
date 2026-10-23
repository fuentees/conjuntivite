# Carrega o app com dados sintéticos, sem acessar o banco.
raiz_app <- normalizePath(file.path(testthat::test_path(), "..", ".."))

# Porta fechada: a carga inicial falha na hora em vez de esperar o banco da rede interna.
Sys.setenv(OFTALMO_DB_HOST = "127.0.0.1", OFTALMO_DB_PORT = "1", OFTALMO_REFRESH_MINUTES = "5")

na_raiz <- function(code) withr::with_dir(raiz_app, code)

suppressWarnings(suppressPackageStartupMessages(na_raiz({
  library(shiny)
  library(bslib)
  library(dplyr)
  library(plotly)
  library(leaflet)
  library(lubridate)
  library(tidyr)
  source("R/data.R")
  source("R/ui.R")
  source("R/server.R")
})))

# Notificações brutas no formato da tabela do banco.
dados_brutos_sinteticos <- function() {
  municipios <- tibble::tribble(
    ~MunicipioNotificacao,      ~GVE_NOME,       ~DRS_NOME,
    "SAO PAULO",                "CAPITAL",       "GRANDE SAO PAULO",
    "SANTOS",                   "SANTOS",        "BAIXADA SANTISTA",
    "SAO LUIS DO PARAITINGA",   "TAUBATE",       "TAUBATE"
  )
  tidyr::expand_grid(municipios, ANO = 2014:2026, SemEpidemio = 1:52) %>%
    filter(!(ANO == 2026 & SemEpidemio > 10)) %>%
    # Santos para de notificar 3 semanas antes do fim da base.
    filter(!(MunicipioNotificacao == "SANTOS" & ANO == 2026 & SemEpidemio > 7)) %>%
    mutate(
      TotalCaso = (SemEpidemio %% 5) + (ANO - 2010) + ifelse(MunicipioNotificacao == "SAO PAULO", 100, 0),
      Populacao = 80644,
      NuSurto = ifelse(ANO == 2026 & SemEpidemio == 2, 1, 0),
      Surto = NuSurto,
      SexMasc = ifelse(ANO == 2026, 1, 0),
      SexFem = ifelse(ANO == 2026, 1, 0),
      Unid_notificacao = "UBS TESTE"
    )
}

dados_sinteticos <- function() {
  na_raiz(
    prepare_oftalmo_dataset(dados_brutos_sinteticos()) %>%
      padronizar_regionais() %>%
      anexar_populacao_ibge()
  )
}

# Coloca os dados sintéticos no cache que o server usa.
usar_dados_sinteticos <- function() {
  dados <- dados_sinteticos()
  dataset_cache$dados <- dados
  dataset_cache$carregado_em <- Sys.time()
  dataset_cache$erro <- NULL
  assign("dados_epidemiologicos", dados, envir = globalenv())
  assign("carga_inicial", list(dados = dados, carregado_em = Sys.time(), erro = NULL), envir = globalenv())
  invisible(dados)
}
