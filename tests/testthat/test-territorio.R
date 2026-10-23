# Cada nível (DRS, GVE e município) deve ver só o próprio território em todo o painel.
filtros <- function(drs = "Todas", gve = "Todas", municipio = "Todos", ano = "2025") {
  list(
    filtro_ano = ano, filtro_drs = drs, filtro_gve = gve, filtro_municipio = municipio,
    filtro_semanas = c(1, 53), nivel_mapa = "municipio", agrupar_territorio = "gve", metrica_territorio = "casos"
  )
}

test_that("com uma DRS, todas as partes mostram só a DRS", {
  usar_dados_sinteticos()

  na_raiz(testServer(server, {
    do.call(session$setInputs, filtros(drs = "Baixada Santista"))

    expect_true(all(dados_contexto_periodo()$drs == "Baixada Santista"))
    expect_true(all(canal_endemico()$casos[is.finite(canal_endemico()$casos)] >= 0))
    expect_setequal(indicadores_por_area("gve")$area, "Santos")
    expect_true(all(silencio_municipios()$drs == "Baixada Santista"))
    expect_true(all(tabela_operacional_municipios()$gve == "Santos"))
    expect_match(output$territorio_atual, "DRS Baixada Santista · 9 municípios")

    # Ranking "por DRS" não faz sentido com uma DRS escolhida: vira "por GVE".
    session$setInputs(agrupar_territorio = "drs")
    expect_equal(nivel_territorio(), "gve")
    expect_match(output$titulo_grafico_casos_territorio, "por GVE — DRS Baixada Santista")

    # Mapa: municípios fora da DRS ficam fora, inclusive a capital.
    fora <- areas_fora_do_filtro()
    expect_true("3550308" %in% fora)
    expect_false("3548500" %in% fora)
    # Guarujá nunca notificou na base sintética, mas é da DRS e continua dentro.
    expect_false("3518701" %in% fora)
  }))
})

test_that("com uma GVE, o mapa não mostra o polígono da DRS com números da GVE", {
  usar_dados_sinteticos()

  na_raiz(testServer(server, {
    do.call(session$setInputs, filtros(drs = "Taubaté", gve = "Taubaté"))
    session$setInputs(nivel_mapa = "drs")
    expect_equal(nivel_mapa_atual(), "gve")
    expect_equal(nivel_territorio(), "municipio")
    ref <- load_municipios_referencia()
    expect_true(nrow(resumo_municipios()) > 0)
    expect_true(all(resumo_municipios()$cod_ibge %in% ref$cod_ibge[ref$gve == "Taubaté"]))
    expect_true(all(silencio_municipios()$gve == "Taubaté"))
    expect_true(nrow(silencio_municipios()) > 1)
  }))
})

test_that("com um município, tudo é só do município", {
  usar_dados_sinteticos()

  na_raiz(testServer(server, {
    do.call(session$setInputs, filtros(drs = "Baixada Santista", gve = "Santos", municipio = "Santos"))
    session$setInputs(nivel_mapa = "gve")
    expect_equal(nivel_mapa_atual(), "municipio")
    expect_true(all(dados_contexto_periodo()$municipio == "Santos"))
    expect_equal(nrow(silencio_municipios()), 1)
    expect_equal(nivel_territorio(), "unidade")
    expect_match(output$territorio_atual, "Santos · GVE Santos · DRS Baixada Santista")
    expect_equal(length(areas_fora_do_filtro()), 644)
  }))
})

test_that("município que nunca notificou pode ser escolhido e o painel não quebra", {
  usar_dados_sinteticos()

  na_raiz(testServer(server, {
    expect_true("Guarujá" %in% all_municipio_choices())
    do.call(session$setInputs, filtros(drs = "Baixada Santista", gve = "Santos", municipio = "Guarujá"))
    expect_equal(output$total_casos, "0")
    expect_equal(as.character(silencio_municipios()$situacao), "Nunca notificou")
    expect_equal(areas_fora_do_filtro()[!(areas_fora_do_filtro() %in% "3518701")], areas_fora_do_filtro())
  }))
})
