library(dplyr)
library(lubridate)

set.seed(42)

drs_list <- c("DRS Ribeirão Preto", "DRS Campinas", "DRS São Paulo", "DRS Bauru", "DRS Araçatuba")

drs_gve_mapping <- list(
  "DRS Ribeirão Preto" = c("GVE Ribeirão Preto", "GVE Araraquara", "GVE São Carlos"),
  "DRS Campinas" = c("GVE Campinas", "GVE Piracicaba", "GVE Sorocaba"),
  "DRS São Paulo" = c("GVE Santo Amaro", "GVE Vila Mariana", "GVE Pinheiros"),
  "DRS Bauru" = c("GVE Bauru", "GVE Avaré", "GVE Jaú"),
  "DRS Araçatuba" = c("GVE Araçatuba", "GVE Birigui", "GVE Andradina")
)

gve_municipio_mapping <- list(
  "GVE Ribeirão Preto" = c("Ribeirão Preto", "Cravinhos", "Serrana"),
  "GVE Araraquara" = c("Araraquara", "Boa Esperança", "Matão"),
  "GVE São Carlos" = c("São Carlos", "Ibaté", "Brotas"),
  "GVE Campinas" = c("Campinas", "Valinhos", "Vinhedo"),
  "GVE Piracicaba" = c("Piracicaba", "Santa Maria da Serra", "Limeira"),
  "GVE Sorocaba" = c("Sorocaba", "Salto", "Boituva"),
  "GVE Santo Amaro" = c("São Paulo", "Santo André", "São Bernardo"),
  "GVE Vila Mariana" = c("São Paulo", "Taboão da Serra", "Embu-Guaçu"),
  "GVE Pinheiros" = c("São Paulo", "Osasco", "Diadema"),
  "GVE Bauru" = c("Bauru", "Agudos", "Pederneiras"),
  "GVE Avaré" = c("Avaré", "Arandu", "Itatinga"),
  "GVE Jaú" = c("Jaú", "Mineiros do Tietê", "Barra Bonita"),
  "GVE Araçatuba" = c("Araçatuba", "Castilho", "Guzolândia"),
  "GVE Birigui" = c("Birigui", "Penápolis", "Luiziânia"),
  "GVE Andradina" = c("Andradina", "Lavínia", "Murutinga do Sul")
)

generate_mock_data <- function() {
  data <- expand_grid(
    ano = c(2022, 2023, 2024),
    semana = 1:52,
    drs = drs_list
  ) %>%
    mutate(
      gve = case_when(
        drs == "DRS Ribeirão Preto" ~ sample(drs_gve_mapping[["DRS Ribeirão Preto"]], n(), replace = TRUE),
        drs == "DRS Campinas" ~ sample(drs_gve_mapping[["DRS Campinas"]], n(), replace = TRUE),
        drs == "DRS São Paulo" ~ sample(drs_gve_mapping[["DRS São Paulo"]], n(), replace = TRUE),
        drs == "DRS Bauru" ~ sample(drs_gve_mapping[["DRS Bauru"]], n(), replace = TRUE),
        drs == "DRS Araçatuba" ~ sample(drs_gve_mapping[["DRS Araçatuba"]], n(), replace = TRUE),
      ),
      municipio = case_when(
        gve == "GVE Ribeirão Preto" ~ sample(gve_municipio_mapping[["GVE Ribeirão Preto"]], n(), replace = TRUE),
        gve == "GVE Araraquara" ~ sample(gve_municipio_mapping[["GVE Araraquara"]], n(), replace = TRUE),
        gve == "GVE São Carlos" ~ sample(gve_municipio_mapping[["GVE São Carlos"]], n(), replace = TRUE),
        gve == "GVE Campinas" ~ sample(gve_municipio_mapping[["GVE Campinas"]], n(), replace = TRUE),
        gve == "GVE Piracicaba" ~ sample(gve_municipio_mapping[["GVE Piracicaba"]], n(), replace = TRUE),
        gve == "GVE Sorocaba" ~ sample(gve_municipio_mapping[["GVE Sorocaba"]], n(), replace = TRUE),
        gve == "GVE Santo Amaro" ~ sample(gve_municipio_mapping[["GVE Santo Amaro"]], n(), replace = TRUE),
        gve == "GVE Vila Mariana" ~ sample(gve_municipio_mapping[["GVE Vila Mariana"]], n(), replace = TRUE),
        gve == "GVE Pinheiros" ~ sample(gve_municipio_mapping[["GVE Pinheiros"]], n(), replace = TRUE),
        gve == "GVE Bauru" ~ sample(gve_municipio_mapping[["GVE Bauru"]], n(), replace = TRUE),
        gve == "GVE Avaré" ~ sample(gve_municipio_mapping[["GVE Avaré"]], n(), replace = TRUE),
        gve == "GVE Jaú" ~ sample(gve_municipio_mapping[["GVE Jaú"]], n(), replace = TRUE),
        gve == "GVE Araçatuba" ~ sample(gve_municipio_mapping[["GVE Araçatuba"]], n(), replace = TRUE),
        gve == "GVE Birigui" ~ sample(gve_municipio_mapping[["GVE Birigui"]], n(), replace = TRUE),
        gve == "GVE Andradina" ~ sample(gve_municipio_mapping[["GVE Andradina"]], n(), replace = TRUE),
        TRUE ~ "Desconhecido"
      ),
      casos = case_when(
        ano == 2024 ~ rpois(n(), lambda = 15 + semana * 0.5 + rnorm(n(), 0, 3)),
        ano == 2023 ~ rpois(n(), lambda = 10 + rnorm(n(), 0, 2)),
        TRUE ~ rpois(n(), lambda = 8 + rnorm(n(), 0, 1))
      ),
      casos = pmax(casos, 0),
      populacao = case_when(
        municipio == "São Paulo" ~ 11000000,
        municipio == "Campinas" ~ 1200000,
        municipio == "Ribeirão Preto" ~ 700000,
        municipio == "Sorocaba" ~ 650000,
        TRUE ~ sample(100000:500000, 1)
      ),
      incidencia = round((casos / populacao) * 100000, 2)
    ) %>%
    arrange(ano, semana, drs, gve, municipio)

  return(data)
}

dados_epidemiologicos <- generate_mock_data()

get_gve_by_drs <- function(drs) {
  drs_gve_mapping[[drs]]
}

get_municipios_by_gve <- function(gve) {
  gve_municipio_mapping[[gve]]
}
