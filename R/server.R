ensure_attached_package <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(
      sprintf(
        "Pacote '%s' não está instalado. Execute install.packages('%s').",
        pkg, pkg
      ),
      call. = FALSE
    )
  }

  if (!(paste0("package:", pkg) %in% search())) {
    suppressPackageStartupMessages(library(pkg, character.only = TRUE))
  }
}

ensure_attached_package("plotly")
ensure_attached_package("leaflet")

server <- function(input, output, session) {
  anos_ordenados <- sort(unique(dados_epidemiologicos$ano), decreasing = TRUE)
  ano_padrao <- "Todos"

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

  selected_year <- reactive({
    if (is.null(input$filtro_ano) || identical(input$filtro_ano, "Todos")) {
      if (length(anos_ordenados) > 0) {
        return(as.integer(anos_ordenados[1]))
      }
      return(as.integer(format(Sys.Date(), "%Y")))
    }

    suppressWarnings(as.integer(input$filtro_ano))
  })

  output$fonte_dados <- renderText({
    if (identical(dados_fonte, "database")) {
      "Fonte: Banco MariaDB"
    } else {
      "Fonte: Mock data (fallback)"
    }
  })

  clean_choice_values <- function(values) {
    values <- as.character(values)
    values <- trimws(values)
    values <- values[!is.na(values) & values != "" & tolower(values) != "na"]
    sort(unique(values))
  }

  all_drs_choices <- clean_choice_values(dados_epidemiologicos$drs)
  all_gve_choices <- clean_choice_values(dados_epidemiologicos$gve)
  all_municipio_choices <- clean_choice_values(dados_epidemiologicos$municipio)

  if (length(all_drs_choices) == 0) {
    all_drs_choices <- "Não informado"
  }
  if (length(all_gve_choices) == 0) {
    all_gve_choices <- "Não informado"
  }
  if (length(all_municipio_choices) == 0) {
    all_municipio_choices <- "Não informado"
  }

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
    df_ano <- if (identical(input$filtro_ano, "Todos")) {
      dados_epidemiologicos
    } else {
      dados_epidemiologicos %>% filter(ano == selected_year())
    }

    drs_choices <- clean_choice_values(df_ano$drs)
    if (length(drs_choices) == 0) {
      drs_choices <- all_drs_choices
    }

    updateSelectInput(
      session,
      "filtro_drs",
      choices = c("Todas", drs_choices),
      selected = "Todas"
    )
    updateSelectInput(session, "filtro_gve", choices = c("Todas", all_gve_choices), selected = "Todas")
    updateSelectInput(session, "filtro_municipio", choices = c("Todos", all_municipio_choices), selected = "Todos")
  }, ignoreNULL = FALSE)

  observeEvent(input$filtro_drs, {
    req(input$filtro_ano)
    df_ano <- if (identical(input$filtro_ano, "Todos")) {
      dados_epidemiologicos
    } else {
      dados_epidemiologicos %>% filter(ano == selected_year())
    }
    df <- df_ano

    if (!is.null(input$filtro_drs) && input$filtro_drs != "Todas") {
      df <- df %>% filter(drs == input$filtro_drs)
    }

    gve_choices <- clean_choice_values(df$gve)
    if (length(gve_choices) == 0) {
      if (!is.null(input$filtro_drs) && input$filtro_drs != "Todas") {
        gve_choices <- clean_choice_values(dados_epidemiologicos$gve[dados_epidemiologicos$drs == input$filtro_drs])
      }

      if (length(gve_choices) == 0) {
        gve_choices <- clean_choice_values(df_ano$gve)
      }
      if (length(gve_choices) == 0) {
        gve_choices <- all_gve_choices
      }
    }

    updateSelectInput(
      session,
      "filtro_gve",
      choices = c("Todas", gve_choices),
      selected = "Todas"
    )
    updateSelectInput(session, "filtro_municipio", choices = c("Todos", all_municipio_choices), selected = "Todos")
  }, ignoreNULL = FALSE)

  observeEvent(input$filtro_gve, {
    req(input$filtro_ano)
    df_ano <- if (identical(input$filtro_ano, "Todos")) {
      dados_epidemiologicos
    } else {
      dados_epidemiologicos %>% filter(ano == selected_year())
    }
    df <- df_ano

    if (!is.null(input$filtro_drs) && input$filtro_drs != "Todas") {
      df <- df %>% filter(drs == input$filtro_drs)
    }

    if (!is.null(input$filtro_gve) && input$filtro_gve != "Todas") {
      df <- df %>% filter(gve == input$filtro_gve)
    }

    mun_choices <- clean_choice_values(df$municipio)
    if (length(mun_choices) == 0) {
      if (!is.null(input$filtro_gve) && input$filtro_gve != "Todas") {
        mun_choices <- clean_choice_values(dados_epidemiologicos$municipio[dados_epidemiologicos$gve == input$filtro_gve])
      }
      if (length(mun_choices) == 0) {
        mun_choices <- clean_choice_values(df_ano$municipio)
      }
      if (length(mun_choices) == 0) {
        mun_choices <- all_municipio_choices
      }
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

    if (!is.null(input$filtro_gve) && input$filtro_gve != "Todas") {
      df <- df %>% filter(gve == input$filtro_gve)
    }

    if (!is.null(input$filtro_municipio) && input$filtro_municipio != "Todos") {
      df <- df %>% filter(municipio == input$filtro_municipio)
    }

    df
  })

  dados_filtrados <- reactive({
    req(input$filtro_ano)
    if (identical(input$filtro_ano, "Todos")) {
      return(dados_filtrados_dimensoes())
    }

    dados_filtrados_dimensoes() %>%
      filter(ano == selected_year())
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

    ano_sel <- selected_year()
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

    plot_ly(
      serie,
      x = ~semana,
      y = ~casos,
      type = "bar",
      name = "Casos por semana",
      marker = list(color = "#1976d2", opacity = 0.9),
      hovertemplate = "Semana %{x}<br>Casos: %{y}<extra></extra>"
    ) %>%
      add_trace(
        x = ~semana,
        y = media,
        type = "scatter",
        mode = "lines",
        name = "CL (média)",
        line = list(color = "#455a64", dash = "dash", width = 2)
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
        name = paste0("Ano selecionado: ", selected_year()),
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

  diagrama_controle_10anos <- reactive({
    ano_ref <- selected_year()
    req(!is.na(ano_ref))

    df_base <- dados_filtrados_dimensoes() %>%
      filter(ano != 2011)

    anos_janela <- seq.int(ano_ref - 9, ano_ref)
    df_janela <- df_base %>%
      filter(ano %in% anos_janela)

    if (nrow(df_janela) == 0) {
      df_janela <- df_base %>% filter(ano <= ano_ref)
    }

    if (nrow(df_janela) == 0) {
      return(
        tibble::tibble(
          semana = 1:53,
          casos = 0,
          CL = 0,
          UCL = 0,
          LCL = 0,
          acima_ucl = FALSE
        )
      )
    }

    serie_ano_semana <- df_janela %>%
      group_by(ano, semana) %>%
      summarise(casos = sum(casos, na.rm = TRUE), .groups = "drop")

    serie_ano_ref <- serie_ano_semana %>%
      filter(ano == ano_ref) %>%
      group_by(semana) %>%
      summarise(casos = sum(casos, na.rm = TRUE), .groups = "drop")

    serie_historica <- serie_ano_semana %>% filter(ano != ano_ref)
    if (nrow(serie_historica) == 0) {
      serie_historica <- serie_ano_semana
    }

    limites <- serie_historica %>%
      group_by(semana) %>%
      summarise(
        CL = mean(casos, na.rm = TRUE),
        DP = sd(casos, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        DP = tidyr::replace_na(DP, 0),
        UCL = CL + 3 * DP,
        LCL = pmax(CL - 3 * DP, 0)
      ) %>%
      select(semana, CL, UCL, LCL)

    tibble::tibble(semana = 1:53) %>%
      left_join(serie_ano_ref, by = "semana") %>%
      mutate(casos = tidyr::replace_na(casos, 0)) %>%
      left_join(limites, by = "semana") %>%
      mutate(
        CL = tidyr::replace_na(CL, 0),
        UCL = tidyr::replace_na(UCL, 0),
        LCL = tidyr::replace_na(LCL, 0),
        acima_ucl = casos > UCL
      )
  })

  output$grafico_diagrama_controle <- renderPlotly({
    serie <- diagrama_controle_10anos()

    if (nrow(serie) == 0) {
      return(empty_plotly("Sem dados para o diagrama de controle"))
    }

    plot_ly(
      serie,
      x = ~semana,
      y = ~casos,
      type = "scatter",
      mode = "markers",
      name = "Casos semanais",
      marker = list(size = 8, color = "#1f77b4", opacity = 0.75)
    ) %>%
      add_trace(
        x = ~semana,
        y = ~CL,
        type = "scatter",
        mode = "lines",
        name = "CL (Média)",
        line = list(color = "#2e7d32", width = 2)
      ) %>%
      add_trace(
        x = ~semana,
        y = ~UCL,
        type = "scatter",
        mode = "lines",
        name = "UCL (Média + 3DP)",
        line = list(color = "#d32f2f", dash = "dash", width = 1.6)
      ) %>%
      add_trace(
        x = ~semana,
        y = ~LCL,
        type = "scatter",
        mode = "lines",
        name = "LCL (Média - 3DP)",
        line = list(color = "#ef6c00", dash = "dot", width = 1.6)
      ) %>%
      add_trace(
        data = serie %>% filter(acima_ucl),
        x = ~semana,
        y = ~casos,
        type = "scatter",
        mode = "markers",
        name = "Acima do UCL",
        marker = list(size = 10, color = "#d32f2f", symbol = "circle")
      ) %>%
      layout(
        hovermode = "x unified",
        legend = list(orientation = "h", x = 0, y = 1.08),
        margin = list(l = 55, r = 20, t = 10, b = 45),
        plot_bgcolor = "rgba(245,245,245,0.5)",
        paper_bgcolor = "white",
        xaxis = list(title = "Semana epidemiológica", showgrid = TRUE, gridcolor = "#e5e7eb"),
        yaxis = list(title = "Casos", showgrid = TRUE, gridcolor = "#e5e7eb"),
        annotations = list(
          list(
            x = 0.01,
            y = 1.14,
            xref = "paper",
            yref = "paper",
            xanchor = "left",
            yanchor = "bottom",
            text = paste0("Base histórica: últimos 10 anos (exceto 2011) | Ano exibido: ", selected_year()),
            showarrow = FALSE,
            font = list(size = 11, color = "#52606d")
          )
        )
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

  safe_weighted_mean <- function(x, w) {
    valid <- is.finite(x) & is.finite(w) & w > 0
    if (!any(valid)) {
      return(mean(x, na.rm = TRUE))
    }
    stats::weighted.mean(x[valid], w[valid], na.rm = TRUE)
  }

  nivel_mapa_atual <- reactive({
    nivel <- input$nivel_mapa
    if (is.null(nivel) || !(nivel %in% c("municipio", "gve", "drs"))) {
      return("municipio")
    }
    nivel
  })

  dados_mapa_nivel <- reactive({
    df <- dados_filtrados()
    nivel <- nivel_mapa_atual()

    if (nrow(df) == 0) {
      return(
        tibble::tibble(
          rotulo = character(),
          map_key = character(),
          casos = numeric(),
          populacao = numeric(),
          incidencia = numeric()
        )
      )
    }

    base_pop <- df %>%
      distinct(drs, gve, municipio, populacao)

    if (nivel == "municipio") {
      return(
        df %>%
          group_by(municipio) %>%
          summarise(
            casos = sum(casos, na.rm = TRUE),
            populacao = max(populacao, na.rm = TRUE),
            .groups = "drop"
          ) %>%
          mutate(
            populacao = ifelse(is.infinite(populacao), NA_real_, populacao),
            incidencia = ifelse(populacao > 0, (casos / populacao) * 100000, NA_real_),
            rotulo = municipio,
            map_key = normalize_shape_key(rotulo, level = "municipio")
          ) %>%
          select(rotulo, map_key, casos, populacao, incidencia)
      )
    }

    if (nivel == "gve") {
      casos_nivel <- df %>%
        group_by(gve) %>%
        summarise(casos = sum(casos, na.rm = TRUE), .groups = "drop")

      pop_nivel <- base_pop %>%
        group_by(gve) %>%
        summarise(populacao = sum(populacao, na.rm = TRUE), .groups = "drop")

      return(
        casos_nivel %>%
          left_join(pop_nivel, by = "gve") %>%
          mutate(
            populacao = ifelse(populacao <= 0, NA_real_, populacao),
            incidencia = ifelse(populacao > 0, (casos / populacao) * 100000, NA_real_),
            rotulo = gve,
            map_key = normalize_shape_key(rotulo, level = "gve")
          ) %>%
          select(rotulo, map_key, casos, populacao, incidencia)
      )
    }

    casos_nivel <- df %>%
      group_by(drs) %>%
      summarise(casos = sum(casos, na.rm = TRUE), .groups = "drop")

    pop_nivel <- base_pop %>%
      group_by(drs) %>%
      summarise(populacao = sum(populacao, na.rm = TRUE), .groups = "drop")

    casos_nivel %>%
      left_join(pop_nivel, by = "drs") %>%
      mutate(
        populacao = ifelse(populacao <= 0, NA_real_, populacao),
        incidencia = ifelse(populacao > 0, (casos / populacao) * 100000, NA_real_),
        rotulo = drs,
        map_key = normalize_shape_key(rotulo, level = "drs")
      ) %>%
      select(rotulo, map_key, casos, populacao, incidencia)
  })

  pontos_base_mapa <- reactive({
    nivel <- nivel_mapa_atual()
    df_filtrado <- dados_filtrados()

    base <- df_filtrado %>%
      distinct(drs, gve, municipio, populacao) %>%
      left_join(get_municipio_coords(df_filtrado$municipio), by = "municipio") %>%
      filter(is.finite(lat), is.finite(lng))

    if (nrow(base) == 0) {
      return(tibble::tibble(rotulo = character(), lat = numeric(), lng = numeric()))
    }

    if (nivel == "municipio") {
      return(
        base %>%
          group_by(municipio) %>%
          summarise(
            lat = mean(lat, na.rm = TRUE),
            lng = mean(lng, na.rm = TRUE),
            .groups = "drop"
          ) %>%
          transmute(rotulo = municipio, lat, lng)
      )
    }

    if (nivel == "gve") {
      return(
        base %>%
          group_by(gve) %>%
          summarise(
            lat = safe_weighted_mean(lat, populacao),
            lng = safe_weighted_mean(lng, populacao),
            .groups = "drop"
          ) %>%
          transmute(rotulo = gve, lat, lng)
      )
    }

    base %>%
      group_by(drs) %>%
      summarise(
        lat = safe_weighted_mean(lat, populacao),
        lng = safe_weighted_mean(lng, populacao),
        .groups = "drop"
      ) %>%
      transmute(rotulo = drs, lat, lng)
  })

  output$fonte_geografica <- renderText({
    nivel <- nivel_mapa_atual()
    shape_local <- get_local_shape_layer(nivel)

    if (!is.null(shape_local)) {
      shp_name <- basename(attr(shape_local, "shape_path"))
      return(paste("Fonte geográfica: shape local (", shp_name, ")", sep = ""))
    }

    if (nivel == "municipio") {
      shp_muni_fallback <- get_sp_municipality_shapes()
      if (!is.null(shp_muni_fallback) && nrow(shp_muni_fallback) > 0) {
        return("Fonte geográfica: geobr (fallback)")
      }
    }

    "Fonte geográfica: pontos estimados (sem shape)"
  })

  output$mapa <- renderLeaflet({
    nivel <- nivel_mapa_atual()
    df_mapa <- dados_mapa_nivel()

    if (nrow(df_mapa) == 0) {
      return(
        leaflet() %>%
          addProviderTiles("CartoDB.Positron") %>%
          setView(lng = -48.0, lat = -22.5, zoom = 6)
      )
    }

    inc_values <- df_mapa$incidencia[is.finite(df_mapa$incidencia)]
    if (length(inc_values) == 0) {
      inc_values <- c(0, 1)
    }

    pal <- colorNumeric(
      palette = c("#deebf7", "#9ecae1", "#3182bd", "#08519c"),
      domain = inc_values,
      na.color = "#f2f4f6"
    )

    local_shape <- get_local_shape_layer(nivel)
    shape_source <- "local"

    if (is.null(local_shape) && nivel == "municipio") {
      geobr_shape <- get_sp_municipality_shapes()
      if (!is.null(geobr_shape) && nrow(geobr_shape) > 0) {
        local_shape <- geobr_shape %>%
          mutate(
            map_key = municipio_norm,
            map_label = name_muni
          )
        shape_source <- "geobr"
      }
    }

    if (!is.null(local_shape) && nrow(local_shape) > 0) {
      shp_join <- local_shape %>%
        left_join(df_mapa, by = "map_key")

      shp_join$label_exibicao <- ifelse(
        is.na(shp_join$rotulo) | shp_join$rotulo == "",
        as.character(shp_join$map_label),
        shp_join$rotulo
      )

      shp_join$tooltip <- ifelse(
        is.na(shp_join$casos),
        paste0("<strong>", shp_join$label_exibicao, "</strong><br>Sem notificações para os filtros"),
        paste0(
          "<strong>", shp_join$label_exibicao, "</strong><br>",
          "Casos: ", format(round(shp_join$casos), big.mark = ".", decimal.mark = ","), "<br>",
          "Incidência: ", format(round(shp_join$incidencia, 2), decimal.mark = ","), "/100 mil"
        )
      )

      titulo_legenda <- ifelse(
        shape_source == "local",
        "Incidência (/100 mil)",
        "Incidência (/100 mil) - fallback"
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
            title = titulo_legenda,
            position = "bottomright",
            opacity = 0.9
          )
      )
    }

    pontos_base <- pontos_base_mapa()
    pontos <- df_mapa %>%
      left_join(pontos_base, by = "rotulo") %>%
      filter(is.finite(lat), is.finite(lng))

    if (nrow(pontos) == 0) {
      return(
        leaflet() %>%
          addProviderTiles("CartoDB.Positron") %>%
          setView(lng = -48.0, lat = -22.5, zoom = 6)
      )
    }

    titulo_nivel <- switch(
      nivel,
      municipio = "Município",
      gve = "GVE",
      drs = "DRS"
    )

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
          "<strong>", titulo_nivel, ": ", rotulo, "</strong><br>",
          "Casos: ", format(round(casos), big.mark = ".", decimal.mark = ","), "<br>",
          "Incidência: ", format(round(incidencia, 2), decimal.mark = ","), "/100 mil"
        )
      ) %>%
      addLegend(
        pal = pal,
        values = inc_values,
        title = paste0("Incidência (/100 mil) - ", toupper(nivel)),
        position = "bottomright",
        opacity = 0.9
      )
  })
}
