server <- function(input, output, session) {

  observeEvent(input$btn_reset, {
    updateSelectInput(session, "filtro_ano", selected = 2024)
    updateSelectInput(session, "filtro_drs", selected = "Todas")
    updateSelectInput(session, "filtro_gve", selected = "Todas")
    updateSelectInput(session, "filtro_municipio", selected = "Todos")
  })

  observeEvent(input$filtro_drs, {
    if (input$filtro_drs == "Todas") {
      gve_choices <- c("Todas", sort(unique(dados_epidemiologicos$gve)))
    } else {
      gve_choices <- c("Todas", sort(get_gve_by_drs(input$filtro_drs)))
    }
    updateSelectInput(session, "filtro_gve", choices = gve_choices, selected = "Todas")
    updateSelectInput(session, "filtro_municipio", choices = "Todos")
  })

  observeEvent(input$filtro_gve, {
    if (input$filtro_gve == "Todas") {
      mun_choices <- "Todos"
      if (input$filtro_drs != "Todas") {
        gves <- get_gve_by_drs(input$filtro_drs)
        municipios <- unique(unlist(lapply(gves, get_municipios_by_gve)))
        mun_choices <- c("Todos", sort(municipios))
      } else {
        municipios <- unique(dados_epidemiologicos$municipio)
        mun_choices <- c("Todos", sort(municipios))
      }
    } else {
      mun_choices <- c("Todos", sort(get_municipios_by_gve(input$filtro_gve)))
    }
    updateSelectInput(session, "filtro_municipio", choices = mun_choices, selected = "Todos")
  })

  dados_filtrados <- reactive({
    df <- dados_epidemiologicos %>%
      filter(ano == input$filtro_ano)

    if (input$filtro_drs != "Todas") {
      df <- df %>% filter(drs == input$filtro_drs)
    }

    if (input$filtro_gve != "Todas") {
      df <- df %>% filter(gve == input$filtro_gve)
    }

    if (input$filtro_municipio != "Todos") {
      df <- df %>% filter(municipio == input$filtro_municipio)
    }

    return(df)
  })

  output$total_casos <- renderText({
    df <- dados_filtrados()
    format(sum(df$casos, na.rm = TRUE), big.mark = ".", decimal.mark = ",")
  })

  output$incidencia_media <- renderText({
    df <- dados_filtrados()
    media_incidencia <- mean(df$incidencia, na.rm = TRUE)
    paste0(format(round(media_incidencia, 2), decimal.mark = ","), "/100mil")
  })

  output$num_surtos <- renderText({
    df <- dados_filtrados()
    media_casos <- mean(df$casos, na.rm = TRUE)
    surtos <- sum(df$casos > media_casos * 1.5, na.rm = TRUE)
    surtos
  })

  output$situacao_epidemiologica <- renderText({
    df <- dados_filtrados()
    media_casos <- mean(df$casos, na.rm = TRUE)
    casos_atuais <- sum(df$casos[df$semana > 40], na.rm = TRUE) / (length(unique(df$semana[df$semana > 40])) + 0.0001)

    if (casos_atuais > media_casos * 1.5) {
      "EPIDEMIA"
    } else if (casos_atuais > media_casos * 1.1) {
      "ALERTA"
    } else {
      "NORMAL"
    }
  })

  output$situacao_descricao <- renderText({
    df <- dados_filtrados()
    media_casos <- mean(df$casos, na.rm = TRUE)
    casos_atuais <- sum(df$casos[df$semana > 40], na.rm = TRUE) / (length(unique(df$semana[df$semana > 40])) + 0.0001)

    if (casos_atuais > media_casos * 1.5) {
      "Acima do limite de alerta"
    } else if (casos_atuais > media_casos * 1.1) {
      "Próximo ao limite crítico"
    } else {
      "Dentro dos parâmetros esperados"
    }
  })

  output$grafico_serie_temporal <- renderPlotly({
    df <- dados_filtrados() %>%
      group_by(semana) %>%
      summarise(casos = sum(casos, na.rm = TRUE), .groups = "drop")

    media_historica <- mean(df$casos, na.rm = TRUE)

    p <- plot_ly(df, x = ~semana, y = ~casos, type = 'scatter', mode = 'lines+markers',
                 name = 'Casos',
                 line = list(color = '#0066cc', width = 2.5),
                 marker = list(size = 6, color = '#0066cc')) %>%
      add_trace(x = ~semana, y = media_historica, type = 'scatter', mode = 'lines',
                name = 'Média Histórica',
                line = list(color = '#999', width = 1.5, dash = 'dash')) %>%
      layout(
        hovermode = 'x unified',
        showlegend = TRUE,
        legend = list(x = 0.02, y = 0.98),
        plot_bgcolor = 'rgba(245,245,245,0.5)',
        paper_bgcolor = 'white',
        margin = list(l = 60, r = 20, t = 20, b = 60),
        xaxis = list(
          title = 'Semana Epidemiológica',
          showgrid = TRUE,
          gridwidth = 1,
          gridcolor = '#e0e0e0',
          zeroline = FALSE
        ),
        yaxis = list(
          title = 'Número de Casos',
          showgrid = TRUE,
          gridwidth = 1,
          gridcolor = '#e0e0e0',
          zeroline = FALSE
        )
      )

    p
  })

  output$grafico_canal_endemico <- renderPlotly({
    df <- dados_epidemiologicos %>%
      filter(ano %in% c(2022, 2023, 2024)) %>%
      group_by(semana, ano) %>%
      summarise(casos = sum(casos, na.rm = TRUE), .groups = "drop")

    media_2023 <- df %>%
      filter(ano == 2023) %>%
      group_by(semana) %>%
      summarise(media = mean(casos, na.rm = TRUE), .groups = "drop")

    limite_superior <- media_2023$media * 1.3
    limite_inferior <- media_2023$media * 0.7

    ano_atual <- df %>% filter(ano == 2024)

    p <- plot_ly() %>%
      add_ribbons(x = media_2023$semana, ymin = limite_inferior, ymax = limite_superior,
                  name = 'Zona de Segurança',
                  fillcolor = 'rgba(200, 230, 255, 0.3)',
                  line = list(color = 'rgba(0,0,0,0)')) %>%
      add_trace(x = media_2023$semana, y = media_2023$media, type = 'scatter', mode = 'lines',
                name = 'Média Histórica',
                line = list(color = '#999', width = 1.5, dash = 'dash')) %>%
      add_trace(x = ano_atual$semana, y = ano_atual$casos, type = 'scatter', mode = 'lines+markers',
                name = '2024',
                line = list(color = '#0066cc', width = 2.5),
                marker = list(size = 5, color = '#0066cc')) %>%
      layout(
        hovermode = 'x unified',
        showlegend = TRUE,
        legend = list(x = 0.02, y = 0.98),
        plot_bgcolor = 'rgba(245,245,245,0.5)',
        paper_bgcolor = 'white',
        margin = list(l = 60, r = 20, t = 20, b = 60),
        xaxis = list(
          title = 'Semana Epidemiológica',
          showgrid = TRUE,
          gridwidth = 1,
          gridcolor = '#e0e0e0'
        ),
        yaxis = list(
          title = 'Número de Casos',
          showgrid = TRUE,
          gridwidth = 1,
          gridcolor = '#e0e0e0'
        )
      )

    p
  })

  output$grafico_diagrama_controle <- renderPlotly({
    df <- dados_filtrados() %>%
      group_by(semana) %>%
      summarise(casos = sum(casos, na.rm = TRUE), .groups = "drop")

    media <- mean(df$casos, na.rm = TRUE)
    sd_casos <- sd(df$casos, na.rm = TRUE)

    limite_superior <- media + (2 * sd_casos)
    limite_inferior <- max(0, media - (2 * sd_casos))

    p <- plot_ly(df, x = ~semana, y = ~casos, type = 'scatter', mode = 'markers',
                 marker = list(size = 8, color = '#0066cc', opacity = 0.7),
                 name = 'Casos') %>%
      add_trace(x = ~semana, y = media, type = 'scatter', mode = 'lines',
                name = 'Média',
                line = list(color = '#4caf50', width = 2)) %>%
      add_trace(x = ~semana, y = limite_superior, type = 'scatter', mode = 'lines',
                name = 'Limite Superior',
                line = list(color = '#f44336', width = 1.5, dash = 'dash')) %>%
      add_trace(x = ~semana, y = limite_inferior, type = 'scatter', mode = 'lines',
                name = 'Limite Inferior',
                line = list(color = '#f44336', width = 1.5, dash = 'dash'),
                showlegend = FALSE) %>%
      layout(
        hovermode = 'x unified',
        showlegend = TRUE,
        legend = list(x = 0.02, y = 0.98),
        plot_bgcolor = 'rgba(245,245,245,0.5)',
        paper_bgcolor = 'white',
        margin = list(l = 60, r = 20, t = 20, b = 60),
        xaxis = list(
          title = 'Semana Epidemiológica',
          showgrid = TRUE,
          gridwidth = 1,
          gridcolor = '#e0e0e0'
        ),
        yaxis = list(
          title = 'Número de Casos',
          showgrid = TRUE,
          gridwidth = 1,
          gridcolor = '#e0e0e0'
        )
      )

    p
  })

  output$grafico_casos_gve <- renderPlotly({
    df <- dados_filtrados() %>%
      group_by(gve) %>%
      summarise(casos = sum(casos, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(casos))

    p <- plot_ly(df, x = ~reorder(gve, casos), y = ~casos, type = 'bar',
                 marker = list(color = '#0066cc', opacity = 0.8),
                 name = 'Casos') %>%
      layout(
        showlegend = FALSE,
        plot_bgcolor = 'rgba(245,245,245,0.5)',
        paper_bgcolor = 'white',
        margin = list(l = 60, r = 20, t = 20, b = 120),
        xaxis = list(
          title = 'GVE',
          tickangle = 45,
          showgrid = FALSE
        ),
        yaxis = list(
          title = 'Número de Casos',
          showgrid = TRUE,
          gridwidth = 1,
          gridcolor = '#e0e0e0'
        )
      )

    p
  })

  output$mapa <- renderLeaflet({
    df_mapa <- dados_filtrados() %>%
      group_by(municipio) %>%
      summarise(
        casos = sum(casos, na.rm = TRUE),
        incidencia = mean(incidencia, na.rm = TRUE),
        .groups = "drop"
      )

    coords_municipios <- data.frame(
      municipio = c("Ribeirão Preto", "Araraquara", "São Carlos", "Campinas", "Piracicaba",
                   "Sorocaba", "São Paulo", "Santo André", "São Bernardo", "Bauru", "Jaú",
                   "Araçatuba", "Birigui", "Andradina", "Cravinhos"),
      lat = c(-21.1797, -21.7975, -22.0097, -22.8955, -22.7329, -23.4945, -23.5505, -23.6672,
             -23.6939, -22.3110, -22.2818, -21.2035, -21.1300, -20.6344, -21.0864),
      lng = c(-47.8103, -48.1767, -47.8908, -47.0646, -47.6397, -47.4789, -46.6333, -46.5052,
             -46.5601, -49.0646, -48.5606, -50.4366, -50.3390, -51.4269, -47.6692)
    )

    df_mapa <- df_mapa %>% left_join(coords_municipios, by = "municipio")

    casos_max <- max(df_mapa$casos, na.rm = TRUE)
    cores <- colorNumeric(
      palette = c('#c8e6ff', '#66b3ff', '#0066cc', '#004d99'),
      domain = 0:casos_max,
      na.color = '#f0f0f0'
    )

    mapa <- leaflet(df_mapa) %>%
      addTiles(urlTemplate = 'https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png',
               attribution = '© OpenStreetMap') %>%
      setView(lng = -48, lat = -22.5, zoom = 7) %>%
      addCircles(
        lng = ~lng, lat = ~lat, radius = ~sqrt(casos) * 500,
        popup = ~paste0("<strong>", municipio, "</strong><br>",
                       "Casos: ", casos, "<br>",
                       "Incidência: ", round(incidencia, 2), "/100mil"),
        color = ~cores(casos),
        weight = 2,
        opacity = 0.8,
        fillOpacity = 0.6
      )

    mapa
  })
}
