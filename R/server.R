server <- function(input, output, session) {
  anos_ordenados <- sort(unique(dados_epidemiologicos$ano), decreasing = TRUE)
  ano_padrao <- if (length(anos_ordenados) > 0) anos_ordenados[1] else as.integer(format(Sys.Date(), "%Y"))

  empty_plotly <- function(message) {
    plotly::plot_ly() %>%
      plotly::layout(
        annotations = list(
          text = message,
          x = 0.5,
          y = 0.5,
          xref = "paper",
          yref = "paper",
          showarrow = FALSE,
          font = list(size = 14, color = "#6b7280")
        ),
        xaxis = list(visible = FALSE),
        yaxis = list(visible = FALSE),
        plot_bgcolor = "rgba(245,245,245,0.5)",
        paper_bgcolor = "white"
      )
  }

  output$fonte_dados <- renderText({
    if (identical(dados_fonte, "database")) {
      "Fonte: Banco MariaDB"
    } else {
      "Fonte: Mock data (fallback)"
    }
  })

  if (identical(dados_fonte, "mock")) {
    session$onFlushed(
      function() {
        showNotification(
          "Conexão com banco indisponível. Dashboard usando dados simulados.",
          type = "warning",
          duration = 8
        )
      },
      once = TRUE
    )
  }

  observeEvent(input$filtro_ano, {
    req(input$filtro_ano)
    df_ano <- dados_epidemiologicos %>%
      filter(ano == as.integer(input$filtro_ano))

    drs_choices <- sort(unique(df_ano$drs))
    drs_choices <- drs_choices[!is.na(drs_choices)]

    updateSelectInput(
      session,
      "filtro_drs",
      choices = c("Todas", drs_choices),
      selected = "Todas"
    )
    updateSelectInput(session, "filtro_gve", choices = "Todas", selected = "Todas")
    updateSelectInput(session, "filtro_municipio", choices = "Todos", selected = "Todos")
  }, ignoreNULL = FALSE)

  observeEvent(input$filtro_drs, {
    req(input$filtro_ano)
    df <- dados_epidemiologicos %>% filter(ano == as.integer(input$filtro_ano))

    if (!is.null(input$filtro_drs) && input$filtro_drs != "Todas") {
      df <- df %>% filter(drs == input$filtro_drs)
    }

    gve_choices <- sort(unique(df$gve))
    gve_choices <- gve_choices[!is.na(gve_choices)]
    if (length(gve_choices) == 0) {
      gve_choices <- "Sem dados"
    }

    updateSelectInput(
      session,
      "filtro_gve",
      choices = c("Todas", gve_choices),
      selected = "Todas"
    )
    updateSelectInput(session, "filtro_municipio", choices = "Todos", selected = "Todos")
  }, ignoreNULL = FALSE)

  observeEvent(input$filtro_gve, {
    req(input$filtro_ano)
    df <- dados_epidemiologicos %>% filter(ano == as.integer(input$filtro_ano))

    if (!is.null(input$filtro_drs) && input$filtro_drs != "Todas") {
      df <- df %>% filter(drs == input$filtro_drs)
    }

    if (!is.null(input$filtro_gve) && input$filtro_gve != "Todas") {
      df <- df %>% filter(gve == input$filtro_gve)
    }

    mun_choices <- sort(unique(df$municipio))
    mun_choices <- mun_choices[!is.na(mun_choices)]
    if (length(mun_choices) == 0) {
      mun_choices <- "Sem dados"
    }

    updateSelectInput(
      session,
      "filtro_municipio",
      choices = c("Todos", mun_choices),
      selected = "Todos"
    )
  }, ignoreNULL = FALSE)

  observeEvent(input$btn_reset, {
    updateSelectInput(session, "filtro_ano", selected = ano_padrao)
    updateSelectInput(session, "filtro_drs", selected = "Todas")
    updateSelectInput(session, "filtro_gve", selected = "Todas")
    updateSelectInput(session, "filtro_municipio", selected = "Todos")
  })

  dados_filtrados_dimensoes <- reactive({
    req(input$filtro_ano)
    df <- dados_epidemiologicos

    if (!is.null(input$filtro_drs) && input$filtro_drs != "Todas") {
      df <- df %>% filter(drs == input$filtro_drs)
    }

    if (!is.null(input$filtro_gve) && input$filtro_gve != "Todas" && input$filtro_gve != "Sem dados") {
      df <- df %>% filter(gve == input$filtro_gve)
    }

    if (!is.null(input$filtro_municipio) && input$filtro_municipio != "Todos" && input$filtro_municipio != "Sem dados") {
      df <- df %>% filter(municipio == input$filtro_municipio)
    }

    df
  })

  dados_filtrados <- reactive({
    req(input$filtro_ano)
    dados_filtrados_dimensoes() %>%
      filter(ano == as.integer(input$filtro_ano))
  })

  serie_semanal_atual <- reactive({
    df <- dados_filtrados() %>%
      group_by(semana) %>%
      summarise(casos = sum(casos, na.rm = TRUE), .groups = "drop")

    tibble::tibble(semana = 1:53) %>%
      left_join(df, by = "semana") %>%
      mutate(casos = tidyr::replace_na(casos, 0))
  })

  canal_endemico <- reactive({
    req(input$filtro_ano)
    df <- dados_filtrados_dimensoes() %>%
      group_by(ano, semana) %>%
      summarise(casos = sum(casos, na.rm = TRUE), .groups = "drop")

    if (nrow(df) == 0) {
      return(tibble::tibble(
        semana = 1:53,
        media = 0,
        limite_superior = 0,
        limite_inferior = 0,
        casos = 0
      ))
    }

    ano_sel <- as.integer(input$filtro_ano)
    historico_base <- df %>% filter(ano < ano_sel)
    if (nrow(historico_base) == 0) {
      historico_base <- df %>% filter(ano != ano_sel)
    }
    if (nrow(historico_base) == 0) {
      historico_base <- df
    }

    stats_semanal <- historico_base %>%
      group_by(semana) %>%
      summarise(
        media = mean(casos, na.rm = TRUE),
        desvio = sd(casos, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        desvio = tidyr::replace_na(desvio, 0),
        limite_superior = pmax(media + 1.96 * desvio, media * 1.15),
        limite_inferior = pmax(media - 1.96 * desvio, 0)
      ) %>%
      select(semana, media, limite_superior, limite_inferior)

    atual <- df %>%
      filter(ano == ano_sel) %>%
      group_by(semana) %>%
      summarise(casos = sum(casos, na.rm = TRUE), .groups = "drop")

    tibble::tibble(semana = 1:53) %>%
      left_join(stats_semanal, by = "semana") %>%
      mutate(
        media = tidyr::replace_na(media, 0),
        limite_superior = tidyr::replace_na(limite_superior, 0),
        limite_inferior = tidyr::replace_na(limite_inferior, 0)
      ) %>%
      left_join(atual, by = "semana") %>%
      mutate(casos = tidyr::replace_na(casos, 0))
  })

  situacao_data <- reactive({
    canal_df <- canal_endemico()

    ultimas <- canal_df %>%
      arrange(desc(semana)) %>%
      slice_head(n = 4)

    media_recente <- mean(ultimas$casos, na.rm = TRUE)
    media_esperada <- mean(ultimas$media, na.rm = TRUE)
    limite_critico <- mean(ultimas$limite_superior, na.rm = TRUE)

    if (is.na(media_recente) || is.na(media_esperada) || is.na(limite_critico)) {
      return(list(status = "SEM DADOS", descricao = "Sem dados para classificação"))
    }

    if (media_recente > limite_critico) {
      return(list(status = "EPIDEMIA", descricao = "Acima do limite superior do canal"))
    }

    if (media_recente > media_esperada * 1.1) {
      return(list(status = "ALERTA", descricao = "Acima da média histórica esperada"))
    }

    list(status = "NORMAL", descricao = "Dentro do comportamento esperado")
  })

  output$total_casos <- renderText({
    df <- dados_filtrados()
    format(sum(df$casos, na.rm = TRUE), big.mark = ".", decimal.mark = ",")
  })

  output$incidencia_media <- renderText({
    df <- dados_filtrados()

    if (nrow(df) == 0) {
      return("0,00")
    }

    pop_total <- df %>%
      distinct(municipio, populacao) %>%
      summarise(total_pop = sum(populacao, na.rm = TRUE)) %>%
      pull(total_pop)

    total_casos <- sum(df$casos, na.rm = TRUE)
    incidencia <- ifelse(pop_total > 0, (total_casos / pop_total) * 100000, 0)

    paste0(format(round(incidencia, 2), decimal.mark = ","), "/100 mil")
  })

  output$num_surtos <- renderText({
    canal_df <- canal_endemico()
    surtos <- sum(canal_df$casos > canal_df$limite_superior, na.rm = TRUE)
    as.character(surtos)
  })

  output$situacao_epidemiologica <- renderText({
    situacao_data()$status
  })

  output$situacao_descricao <- renderText({
    situacao_data()$descricao
  })

  output$grafico_serie_temporal <- renderPlotly({
    serie <- serie_semanal_atual()

    if (nrow(serie) == 0) {
      return(empty_plotly("Sem dados para os filtros selecionados"))
    }

    media <- mean(serie$casos, na.rm = TRUE)

    plot_ly(serie, x = ~semana, y = ~casos, type = "scatter", mode = "lines+markers", name = "Casos") %>%
      add_trace(
        x = ~semana,
        y = media,
        type = "scatter",
        mode = "lines",
        name = "Média do ano",
        line = list(color = "#607d8b", dash = "dash", width = 1.5)
      ) %>%
      layout(
        hovermode = "x unified",
        legend = list(orientation = "h", x = 0, y = 1.08),
        margin = list(l = 55, r = 20, t = 10, b = 45),
        plot_bgcolor = "rgba(245,245,245,0.5)",
        paper_bgcolor = "white",
        xaxis = list(title = "Semana epidemiológica", showgrid = TRUE, gridcolor = "#e5e7eb"),
        yaxis = list(title = "Casos", showgrid = TRUE, gridcolor = "#e5e7eb")
      )
  })

  output$grafico_canal_endemico <- renderPlotly({
    canal_df <- canal_endemico()

    if (nrow(canal_df) == 0) {
      return(empty_plotly("Sem dados para construir o canal endêmico"))
    }

    plot_ly() %>%
      add_ribbons(
        data = canal_df,
        x = ~semana,
        ymin = ~limite_inferior,
        ymax = ~limite_superior,
        name = "Faixa esperada",
        fillcolor = "rgba(25, 118, 210, 0.18)",
        line = list(color = "rgba(0,0,0,0)")
      ) %>%
      add_trace(
        data = canal_df,
        x = ~semana,
        y = ~media,
        type = "scatter",
        mode = "lines",
        name = "Média histórica",
        line = list(color = "#607d8b", dash = "dash", width = 1.6)
      ) %>%
      add_trace(
        data = canal_df,
        x = ~semana,
        y = ~casos,
        type = "scatter",
        mode = "lines+markers",
        name = as.character(input$filtro_ano),
        line = list(color = "#005ca9", width = 2.5),
        marker = list(size = 5)
      ) %>%
      layout(
        hovermode = "x unified",
        legend = list(orientation = "h", x = 0, y = 1.08),
        margin = list(l = 55, r = 20, t = 10, b = 45),
        plot_bgcolor = "rgba(245,245,245,0.5)",
        paper_bgcolor = "white",
        xaxis = list(title = "Semana epidemiológica", showgrid = TRUE, gridcolor = "#e5e7eb"),
        yaxis = list(title = "Casos", showgrid = TRUE, gridcolor = "#e5e7eb")
      )
  })

  output$grafico_diagrama_controle <- renderPlotly({
    serie <- serie_semanal_atual()

    if (nrow(serie) == 0) {
      return(empty_plotly("Sem dados para o diagrama de controle"))
    }

    media <- mean(serie$casos, na.rm = TRUE)
    desvio <- sd(serie$casos, na.rm = TRUE)
    desvio <- ifelse(is.na(desvio), 0, desvio)

    limite_sup <- media + 2 * desvio
    limite_inf <- pmax(media - 2 * desvio, 0)

    serie <- serie %>%
      mutate(
        media = media,
        limite_sup = limite_sup,
        limite_inf = limite_inf,
        fora_controle = casos > limite_sup
      )

    plot_ly(serie, x = ~semana, y = ~casos, type = "scatter", mode = "markers", name = "Casos") %>%
      add_trace(
        x = ~semana,
        y = ~media,
        type = "scatter",
        mode = "lines",
        name = "Média",
        line = list(color = "#2e7d32", width = 2)
      ) %>%
      add_trace(
        x = ~semana,
        y = ~limite_sup,
        type = "scatter",
        mode = "lines",
        name = "Limite superior",
        line = list(color = "#d32f2f", dash = "dash", width = 1.6)
      ) %>%
      add_trace(
        x = ~semana,
        y = ~limite_inf,
        type = "scatter",
        mode = "lines",
        name = "Limite inferior",
        line = list(color = "#d32f2f", dash = "dash", width = 1.6),
        showlegend = FALSE
      ) %>%
      add_trace(
        data = serie %>% filter(fora_controle),
        x = ~semana,
        y = ~casos,
        type = "scatter",
        mode = "markers",
        name = "Ponto crítico",
        marker = list(size = 9, color = "#d32f2f", symbol = "x")
      ) %>%
      layout(
        hovermode = "x unified",
        legend = list(orientation = "h", x = 0, y = 1.08),
        margin = list(l = 55, r = 20, t = 10, b = 45),
        plot_bgcolor = "rgba(245,245,245,0.5)",
        paper_bgcolor = "white",
        xaxis = list(title = "Semana epidemiológica", showgrid = TRUE, gridcolor = "#e5e7eb"),
        yaxis = list(title = "Casos", showgrid = TRUE, gridcolor = "#e5e7eb")
      )
  })

  output$grafico_casos_gve <- renderPlotly({
    df <- dados_filtrados() %>%
      group_by(gve) %>%
      summarise(casos = sum(casos, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(casos))

    if (nrow(df) == 0) {
      return(empty_plotly("Sem dados por GVE para os filtros selecionados"))
    }

    plot_ly(
      df,
      x = ~reorder(gve, casos),
      y = ~casos,
      type = "bar",
      marker = list(color = "#005ca9", opacity = 0.85),
      name = "Casos"
    ) %>%
      layout(
        showlegend = FALSE,
        margin = list(l = 55, r = 20, t = 10, b = 100),
        plot_bgcolor = "rgba(245,245,245,0.5)",
        paper_bgcolor = "white",
        xaxis = list(title = "GVE", tickangle = 35, showgrid = FALSE),
        yaxis = list(title = "Casos", showgrid = TRUE, gridcolor = "#e5e7eb")
      )
  })

  output$mapa <- renderLeaflet({
    df_mapa <- dados_filtrados() %>%
      group_by(municipio) %>%
      summarise(
        casos = sum(casos, na.rm = TRUE),
        populacao = max(populacao, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        populacao = ifelse(is.infinite(populacao), NA_real_, populacao),
        incidencia = ifelse(populacao > 0, (casos / populacao) * 100000, NA_real_),
        municipio_norm = normalize_key(municipio)
      )

    if (nrow(df_mapa) == 0) {
      return(
        leaflet() %>%
          addProviderTiles("CartoDB.Positron") %>%
          setView(lng = -48.0, lat = -22.5, zoom = 6)
      )
    }

    shapes <- get_sp_municipality_shapes()
    inc_values <- df_mapa$incidencia[is.finite(df_mapa$incidencia)]
    if (length(inc_values) == 0) {
      inc_values <- c(0, 1)
    }

    pal <- colorNumeric(
      palette = c("#deebf7", "#9ecae1", "#3182bd", "#08519c"),
      domain = inc_values,
      na.color = "#f2f4f6"
    )

    if (!is.null(shapes) && nrow(shapes) > 0) {
      shp_join <- shapes %>%
        left_join(df_mapa, by = "municipio_norm")

      shp_join$tooltip <- ifelse(
        is.na(shp_join$casos),
        paste0(shp_join$name_muni, "<br>Sem notificações"),
        paste0(
          "<strong>", shp_join$name_muni, "</strong><br>",
          "Casos: ", format(round(shp_join$casos), big.mark = ".", decimal.mark = ","), "<br>",
          "Incidência: ", format(round(shp_join$incidencia, 2), decimal.mark = ","), "/100 mil"
        )
      )

      return(
        leaflet(shp_join) %>%
          addProviderTiles("CartoDB.Positron") %>%
          addPolygons(
            fillColor = ~pal(incidencia),
            color = "#d1d5db",
            weight = 1,
            fillOpacity = 0.8,
            smoothFactor = 0.2,
            label = ~htmltools::HTML(tooltip),
            highlightOptions = highlightOptions(weight = 2, color = "#005ca9", bringToFront = TRUE)
          ) %>%
          addLegend(
            pal = pal,
            values = inc_values,
            title = "Incidência (/100 mil)",
            position = "bottomright",
            opacity = 0.9
          )
      )
    }

    pontos <- df_mapa %>%
      left_join(get_municipio_coords(df_mapa$municipio), by = "municipio")

    leaflet(pontos) %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = -48.0, lat = -22.5, zoom = 6) %>%
      addCircleMarkers(
        lng = ~lng,
        lat = ~lat,
        radius = ~pmax(6, sqrt(casos) / 1.5),
        stroke = TRUE,
        weight = 1.2,
        color = "#0f172a",
        fillColor = ~pal(incidencia),
        fillOpacity = 0.85,
        popup = ~paste0(
          "<strong>", municipio, "</strong><br>",
          "Casos: ", format(round(casos), big.mark = ".", decimal.mark = ","), "<br>",
          "Incidência: ", format(round(incidencia, 2), decimal.mark = ","), "/100 mil"
        )
      ) %>%
      addLegend(
        pal = pal,
        values = inc_values,
        title = "Incidência (/100 mil)",
        position = "bottomright",
        opacity = 0.9
      )
  })
}
