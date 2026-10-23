test_that("calendário epidemiológico segue a regra dos 4 dias", {
  expect_equal(semanas_no_ano_epidemiologico(2020), 53L)
  expect_equal(semanas_no_ano_epidemiologico(2024), 52L)
  expect_equal(semanas_no_ano_epidemiologico(2025), 53L)
  expect_equal(inicio_ano_epidemiologico(2026), as.Date("2026-01-04"))

  se <- semana_epidemiologica_da_data(as.Date(c("2026-01-03", "2026-01-04", "2026-10-22")))
  expect_equal(se$ano, c(2025L, 2026L, 2026L))
  expect_equal(se$semana, c(53L, 1L, 42L))
})

test_that("normalize_key remove acentos e pontuação", {
  expect_equal(normalize_key("São José do Rio Preto"), "sao jose do rio preto")
  expect_equal(normalize_key("Santa Bárbara d'Oeste"), "santa barbara d oeste")
  expect_equal(normalize_key("ÓLEO"), "oleo")
  expect_equal(normalize_key(NA), "")
})

test_that("tabela de população IBGE está completa", {
  pop <- na_raiz(load_populacao_ibge())
  expect_setequal(unique(pop$ano), 2007:2025)
  expect_true(all(table(pop$ano) == 645))
  expect_true(all(pop$populacao > 0))
  expect_equal(pop$populacao[pop$cod_ibge == "3550308" & pop$ano == 2025], 11904961)
  expect_equal(sum(pop$populacao[pop$ano == 2025]), 46081801)
})

test_that("cada município tem GVE e DRS oficiais", {
  ref <- na_raiz(load_municipios_referencia())
  regionais <- na_raiz(utils::read.csv("data/regionais_sp.csv", encoding = "UTF-8"))
  expect_equal(nrow(ref), 645)
  expect_equal(length(unique(ref$gve)), 28)
  expect_equal(length(unique(ref$drs)), 17)
  expect_true(all(ref$gve %in% regionais$gve))
  expect_setequal(ref$cod_ibge, unique(na_raiz(load_populacao_ibge())$cod_ibge))
})

test_that("código IBGE de 6 dígitos é completado", {
  ref <- c("3550308", "3548500")
  expect_equal(normalize_cod_ibge(c("355030", "3548500", "999", NA), ref), c("3550308", "3548500", NA, NA))
})

test_that("preparo ignora a população do banco e usa a do IBGE", {
  dados <- dados_sinteticos()

  sp_2024 <- dados %>% filter(municipio == "São Paulo", ano == 2024)
  expect_true(all(sp_2024$populacao == 11895578))
  expect_true(all(sp_2024$cod_ibge == "3550308"))

  # 2026 ainda não tem população do IBGE: usa 2025.
  sp_2026 <- dados %>% filter(municipio == "São Paulo", ano == 2026)
  expect_true(all(sp_2026$populacao_ano_ref == 2025))

  # Grafia diferente do IBGE é reconhecida pelo apelido.
  expect_true("São Luiz do Paraitinga" %in% dados$municipio)
  expect_length(attr(dados, "municipios_sem_ibge"), 0)

  # Nomes oficiais de GVE e DRS.
  expect_setequal(unique(dados$gve), c("Capital", "Santos", "Taubaté"))
  expect_setequal(unique(dados$drs), c("Grande São Paulo", "Baixada Santista", "Taubaté"))
})

test_that("surto com NuSurto e flag não é contado duas vezes", {
  bruto <- dados_brutos_sinteticos() %>% filter(ANO == 2026, SemEpidemio == 2)
  preparado <- prepare_oftalmo_dataset(bruto)
  expect_equal(sum(preparado$surtos), nrow(bruto))
})

test_that("malhas do mapa carregam com as chaves esperadas", {
  skip_if_not_installed("sf")
  mun <- na_raiz(get_local_shape_layer("municipio"))
  gve <- na_raiz(get_local_shape_layer("gve"))
  drs <- na_raiz(get_local_shape_layer("drs"))
  expect_equal(nrow(mun), 645)
  expect_equal(nrow(gve), 28)
  expect_equal(nrow(drs), 17)
  expect_true("3550308" %in% mun$cod_ibge)
  expect_true("drs grande sao paulo" %in% drs$map_key)
})
