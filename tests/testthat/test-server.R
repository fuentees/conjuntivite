test_that("cards de casos e incidência usam a população IBGE", {
  dados <- usar_dados_sinteticos()

  na_raiz(testServer(server, {
    session$setInputs(
      filtro_ano = "2024", filtro_drs = "Todas", filtro_gve = "Todas", filtro_municipio = "Todos",
      filtro_semanas = c(1, 53), nivel_mapa = "municipio"
    )
    casos_2024 <- sum(dados$casos[dados$ano == 2024])
    pop_2024 <- sum(load_populacao_ibge()$populacao[
      load_populacao_ibge()$ano == 2024 & load_populacao_ibge()$cod_ibge %in% unique(dados$cod_ibge)
    ])
    expect_equal(incidencia_periodo(), casos_2024 / pop_2024 * 1e5)
    expect_equal(output$total_casos, format(casos_2024, big.mark = ".", decimal.mark = ","))
    expect_match(output$incidencia_subtitulo, "2024 \\(SE 1 a 52\\)")
  }))
})

test_that("com todos os anos a incidência é média anual", {
  dados <- usar_dados_sinteticos()

  na_raiz(testServer(server, {
    session$setInputs(filtro_ano = "Todos", filtro_drs = "Todas", filtro_gve = "Todas", filtro_municipio = "Todos", filtro_semanas = c(1, 53))
    pop <- load_populacao_ibge()
    anos_pop <- pmin(2014:2026, 2025)
    pessoas_ano <- sum(vapply(anos_pop, function(a) sum(pop$populacao[pop$ano == a & pop$cod_ibge %in% unique(dados$cod_ibge)]), numeric(1)))
    expect_equal(incidencia_periodo(), sum(dados$casos) / pessoas_ano * 1e5)
    expect_match(output$incidencia_subtitulo, "Média anual")
  }))
})

test_that("filtro de semanas restringe o período e a comparação", {
  dados <- usar_dados_sinteticos()

  na_raiz(testServer(server, {
    session$setInputs(filtro_ano = "2024", filtro_drs = "Todas", filtro_gve = "Todas", filtro_municipio = "Todos", filtro_semanas = c(10, 20))
    esperado <- sum(dados$casos[dados$ano == 2024 & dados$semana >= 10 & dados$semana <= 20])
    expect_equal(sum(dados_contexto_periodo()$casos), esperado)
    expect_match(output$total_casos_subtitulo, "2024 \\(SE 10 a 20\\) · .*vs mesmo período de 2023")
  }))
})

test_that("canal endêmico usa quartis dos anos anteriores sem os atípicos", {
  dados <- usar_dados_sinteticos()

  na_raiz(testServer(server, {
    session$setInputs(filtro_ano = "2024", filtro_drs = "Todas", filtro_gve = "Todas", filtro_municipio = "Todos", filtro_semanas = c(1, 53))
    canal <- canal_endemico()
    anos <- attr(canal, "anos_historicos")
    expect_false(any(anos %in% c(2011, 2020, 2021)))
    expect_true(all(anos < 2024))

    semana1 <- dados %>% filter(ano %in% anos, semana == 1) %>% group_by(ano) %>% summarise(casos = sum(casos))
    expect_equal(canal$mediana[1], stats::median(semana1$casos))
    expect_equal(canal$q3[1], unname(stats::quantile(semana1$casos, 0.75)))
    expect_equal(nrow(canal), 52)
  }))
})

test_that("silêncio epidemiológico aponta quem parou e quem nunca notificou", {
  usar_dados_sinteticos()

  na_raiz(testServer(server, {
    session$setInputs(filtro_ano = "2026", filtro_drs = "Baixada Santista", filtro_gve = "Todas", filtro_municipio = "Todos", filtro_semanas = c(1, 53))
    silencio <- silencio_municipios()
    santos <- silencio %>% filter(municipio == "Santos")
    expect_equal(santos$semanas_sem, 3L)
    expect_equal(as.character(santos$situacao), "2 a 3 semanas")
    # Os outros municípios da Baixada não estão na base sintética.
    expect_true(all(silencio$situacao[silencio$municipio != "Santos"] == "Nunca notificou"))
  }))
})

test_that("ranking de territórios ordena por incidência", {
  usar_dados_sinteticos()

  na_raiz(testServer(server, {
    session$setInputs(
      filtro_ano = "2024", filtro_drs = "Todas", filtro_gve = "Todas", filtro_municipio = "Todos",
      filtro_semanas = c(1, 53), agrupar_territorio = "drs", metrica_territorio = "incidencia"
    )
    expect_equal(nivel_territorio(), "drs")
    expect_match(output$titulo_grafico_casos_territorio, "^Incidência por DRS")
    areas <- indicadores_por_area("drs")
    expect_setequal(areas$area, c("Grande São Paulo", "Baixada Santista", "Taubaté"))
    expect_true(all(is.finite(areas$incidencia)))
  }))
})
