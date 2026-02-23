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
ensure_attached_package("DT")

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
      df_ref <- dados_epidemiologicos

      if (!is.null(input$filtro_drs) && input$filtro_drs != "Todas") {
        df_ref <- df_ref %>% filter(drs == input$filtro_drs)
      }
      if (!is.null(input$filtro_gve) && input$filtro_gve != "Todas") {
        df_ref <- df_ref %>% filter(gve == input$filtro_gve)
      }
      if (!is.null(input$filtro_municipio) && input$filtro_municipio != "Todos") {
        df_ref <- df_ref %>% filter(municipio == input$filtro_municipio)
      }
      if (
        !is.null(input$filtro_uvis) &&
          input$filtro_uvis != "Todas" &&
          !is.null(input$filtro_municipio) &&
          input$filtro_municipio != "Todos" &&
          normalize_key(input$filtro_municipio) == "sao paulo"
      ) {
        df_ref <- df_ref %>% filter(uvis == input$filtro_uvis)
      }

      anos_validos <- df_ref %>%
        group_by(ano) %>%
        summarise(total_casos = sum(casos, na.rm = TRUE), .groups = "drop") %>%
        arrange(desc(ano))

      anos_com_caso <- anos_validos %>%
        filter(total_casos > 0)

      if (nrow(anos_com_caso) > 0) {
        return(as.integer(anos_com_caso$ano[1]))
      }
      if (nrow(anos_validos) > 0) {
        return(as.integer(anos_validos$ano[1]))
      }

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

  is_sao_paulo_selected <- reactive({
    !is.null(input$filtro_municipio) &&
      input$filtro_municipio != "Todos" &&
      normalize_key(input$filtro_municipio) == "sao paulo"
  })

  uvis_choices_for_context <- reactive({
    if (!is_sao_paulo_selected()) {
      return(character())
    }

    df <- if (identical(input$filtro_ano, "Todos")) {
      dados_epidemiologicos
    } else {
      dados_epidemiologicos %>% filter(ano == selected_year())
    }

    if (!is.null(input$filtro_drs) && input$filtro_drs != "Todas") {
      df <- df %>% filter(drs == input$filtro_drs)
    }

    if (!is.null(input$filtro_gve) && input$filtro_gve != "Todas") {
      df <- df %>% filter(gve == input$filtro_gve)
    }

    if (!is.null(input$filtro_municipio) && input$filtro_municipio != "Todos") {
      df <- df %>% filter(municipio == input$filtro_municipio)
    }

    clean_choice_values(df$uvis)
  })

  output$uvis_filter_ui <- renderUI({
    if (!is_sao_paulo_selected()) {
      return(NULL)
    }

    uvis_choices <- uvis_choices_for_context()
    if (length(uvis_choices) == 0) {
      uvis_choices <- "Não informado"
    }

    selected_uvis <- if (!is.null(input$filtro_uvis) && input$filtro_uvis %in% c("Todas", uvis_choices)) {
      input$filtro_uvis
    } else {
      "Todas"
    }

    selectInput(
      "filtro_uvis",
      label = "UVIS (somente Município de São Paulo)",
      choices = c("Todas", uvis_choices),
      selected = selected_uvis,
      selectize = FALSE
    )
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
    if (!is.null(input$filtro_uvis)) {
      updateSelectInput(session, "filtro_uvis", selected = "Todas")
    }
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

    if (!is.null(input$filtro_uvis) && input$filtro_uvis != "Todas" && is_sao_paulo_selected()) {
      df <- df %>% filter(uvis == input$filtro_uvis)
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

  dados_ano_referencia <- reactive({
    ano_ref <- selected_year()
    dados_filtrados_dimensoes() %>%
      filter(ano == ano_ref)
  })

  serie_semanal_atual <- reactive({
    df <- dados_ano_referencia() %>%
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
    df_historico <- df %>% filter(ano != 2011)

    historico_base <- df_historico %>% filter(ano < ano_sel)
    if (nrow(historico_base) == 0) {
      historico_base <- df_historico %>% filter(ano != ano_sel)
    }

    if (nrow(historico_base) == 0) {
      stats_semanal <- tibble::tibble(
        semana = 1:53,
        media = 0,
        limite_superior = 0,
        limite_inferior = 0
      )
    } else {
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
    }

    atual <- df %>%
      filter(ano == ano_sel) %>%
      group_by(semana) %>%
      summarise(casos = sum(casos, na.rm = TRUE), .groups = "drop")

    semana_limite_atual <- if (nrow(atual) > 0) {
      suppressWarnings(max(atual$semana[atual$casos > 0], na.rm = TRUE))
    } else {
      NA_real_
    }
    if (!is.finite(semana_limite_atual)) {
      semana_limite_atual <- 0
    }

    tibble::tibble(semana = 1:53) %>%
      left_join(stats_semanal, by = "semana") %>%
      mutate(
        media = tidyr::replace_na(media, 0),
        limite_superior = tidyr::replace_na(limite_superior, 0),
        limite_inferior = tidyr::replace_na(limite_inferior, 0)
      ) %>%
      left_join(atual, by = "semana") %>%
      mutate(
        casos = tidyr::replace_na(casos, 0),
        casos = ifelse(semana > semana_limite_atual, NA_real_, casos)
      )
  })

  situacao_data <- reactive({
    canal_df <- canal_endemico()

    ultimas <- canal_df %>%
      filter(is.finite(casos)) %>%
      arrange(desc(semana)) %>%
      slice_head(n = 4)

    if (nrow(ultimas) == 0) {
      return(list(status = "SEM DADOS", descricao = "Sem dados para classificação"))
    }

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
    df <- dados_ano_referencia()
    format(sum(df$casos, na.rm = TRUE), big.mark = ".", decimal.mark = ",")
  })

  output$incidencia_media <- renderText({
    df <- dados_ano_referencia()

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
    df <- if (identical(input$filtro_ano, "Todos")) {
      dados_filtrados_dimensoes()
    } else {
      dados_ano_referencia()
    }

    surtos_notificados <- sum(df$surtos, na.rm = TRUE)
    if (!is.finite(surtos_notificados)) {
      surtos_notificados <- 0
    }

    format(as.integer(round(surtos_notificados)), big.mark = ".", decimal.mark = ",")
  })

  output$num_surtos_subtitulo <- renderText({
    if (identical(input$filtro_ano, "Todos")) {
      "Surtos notificados (todos os anos)"
    } else {
      paste0("Surtos notificados em ", selected_year())
    }
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
        yaxis = list(title = "Casos", showgrid = TRUE, gridcolor = "#e5e7eb"),
        annotations = list(
          list(
            x = 0.01,
            y = 1.14,
            xref = "paper",
            yref = "paper",
            xanchor = "left",
            yanchor = "bottom",
            text = "Base histórica do canal endêmico: exclui 2011",
            showarrow = FALSE,
            font = list(size = 11, color = "#52606d")
          )
        )
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
        mode = "lines",
        name = paste0("Ano atual: ", selected_year()),
        line = list(color = "#7b1fa2", width = 2.8)
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
            y = 1.13,
            xref = "paper",
            yref = "paper",
            xanchor = "left",
            yanchor = "bottom",
            text = "A linha do ano atual é exibida até a última semana com dados",
            showarrow = FALSE,
            font = list(size = 11, color = "#52606d")
          )
        )
      )
  })

  output$titulo_grafico_casos_territorio <- renderText({
    if (!is.null(input$filtro_gve) && input$filtro_gve != "Todas") {
      paste0("Total de casos por município — ", input$filtro_gve)
    } else {
      "Total de casos por GVE"
    }
  })

  output$grafico_casos_gve <- renderPlotly({
    df_base <- dados_ano_referencia()
    gve_especifico <- !is.null(input$filtro_gve) && input$filtro_gve != "Todas"

    if (gve_especifico) {
      df <- df_base %>%
        group_by(municipio) %>%
        summarise(casos = sum(casos, na.rm = TRUE), .groups = "drop") %>%
        mutate(categoria = municipio) %>%
        arrange(desc(casos))

      msg_empty <- "Sem dados por município para os filtros selecionados"
      x_title <- "Município"
      trace_name <- "Casos por município"
    } else {
      df <- df_base %>%
        group_by(gve) %>%
        summarise(casos = sum(casos, na.rm = TRUE), .groups = "drop") %>%
        mutate(categoria = gve) %>%
        arrange(desc(casos))

      msg_empty <- "Sem dados por GVE para os filtros selecionados"
      x_title <- "GVE"
      trace_name <- "Casos por GVE"
    }

    if (nrow(df) == 0) {
      return(empty_plotly(msg_empty))
    }

    plot_ly(
      df,
      x = ~reorder(categoria, casos),
      y = ~casos,
      type = "bar",
      marker = list(color = "#005ca9", opacity = 0.85),
      name = trace_name,
      hovertemplate = paste0(x_title, ": %{x}<br>Casos: %{y}<extra></extra>")
    ) %>%
      layout(
        showlegend = FALSE,
        margin = list(l = 55, r = 20, t = 10, b = 100),
        plot_bgcolor = "rgba(245,245,245,0.5)",
        paper_bgcolor = "white",
        xaxis = list(title = x_title, tickangle = 35, showgrid = FALSE),
        yaxis = list(title = "Casos", showgrid = TRUE, gridcolor = "#e5e7eb")
      )
  })

  modelo_tendencia <- reactive({
    serie <- serie_semanal_atual() %>% arrange(semana)

    if (nrow(serie) == 0) {
      return(
        list(
          observado = tibble::tibble(semana = integer(), casos = numeric(), tendencia = numeric()),
          previsao = tibble::tibble(semana = integer(), previsto = numeric()),
          slope = 0,
          semana_limite = 0
        )
      )
    }

    semanas_validas <- serie$semana[serie$casos > 0]
    semana_limite <- if (length(semanas_validas) > 0) max(semanas_validas) else max(serie$semana)

    observado <- serie %>%
      filter(semana <= semana_limite)

    if (nrow(observado) == 0) {
      observado <- serie
    }

    if (nrow(observado) >= 8) {
      ajuste_modelo <- tryCatch(
        stats::loess(casos ~ semana, data = observado, span = 0.35, degree = 1, family = "symmetric"),
        error = function(e) NULL
      )
      if (!is.null(ajuste_modelo)) {
        observado$tendencia <- pmax(as.numeric(stats::predict(ajuste_modelo, newdata = observado)), 0)
        observado$tendencia[!is.finite(observado$tendencia)] <- observado$casos[!is.finite(observado$tendencia)]
      } else {
        observado$tendencia <- observado$casos
      }
    } else if (nrow(observado) >= 2) {
      ajuste_modelo <- stats::lm(casos ~ semana, data = observado)
      observado$tendencia <- pmax(as.numeric(stats::predict(ajuste_modelo, newdata = observado)), 0)
    } else {
      observado$tendencia <- observado$casos
    }

    n_slope <- min(8, nrow(observado))
    slope <- 0
    if (n_slope >= 2) {
      slope_model <- stats::lm(casos ~ semana, data = utils::tail(observado, n_slope))
      slope <- as.numeric(coef(slope_model)[["semana"]])
      if (!is.finite(slope)) {
        slope <- 0
      }
    }

    max_semana_previsao <- min(53, semana_limite + 8)
    semanas_previsao <- if (semana_limite < 53) {
      seq.int(semana_limite + 1, max_semana_previsao)
    } else {
      integer()
    }
    ultimo_tendencia <- dplyr::last(observado$tendencia)
    if (!is.finite(ultimo_tendencia)) {
      ultimo_tendencia <- dplyr::last(observado$casos)
    }
    previsao <- tibble::tibble(
      semana = semanas_previsao,
      previsto = if (length(semanas_previsao) > 0) {
        pmax(ultimo_tendencia + slope * (semanas_previsao - semana_limite), 0)
      } else {
        numeric()
      }
    )

    list(
      observado = observado,
      previsao = previsao,
      slope = slope,
      semana_limite = semana_limite
    )
  })

  roll_mean <- function(x, k = 4) {
    vapply(
      seq_along(x),
      function(i) {
        idx <- seq.int(max(1, i - k + 1), i)
        mean(x[idx], na.rm = TRUE)
      },
      numeric(1)
    )
  }

  calc_auc <- function(prob, y_true) {
    prob <- as.numeric(prob)
    y_true <- as.integer(y_true)
    valid <- is.finite(prob) & !is.na(y_true)
    prob <- prob[valid]
    y_true <- y_true[valid]

    if (length(y_true) == 0 || length(unique(y_true)) < 2) {
      return(NA_real_)
    }

    n_pos <- sum(y_true == 1)
    n_neg <- sum(y_true == 0)
    if (n_pos == 0 || n_neg == 0) {
      return(NA_real_)
    }

    ranks <- rank(prob, ties.method = "average")
    (sum(ranks[y_true == 1]) - (n_pos * (n_pos + 1) / 2)) / (n_pos * n_neg)
  }

  calc_class_metrics <- function(pred, obs, prob = NULL) {
    pred <- as.integer(pred)
    obs <- as.integer(obs)
    valid <- !is.na(pred) & !is.na(obs)

    pred <- pred[valid]
    obs <- obs[valid]

    n <- length(obs)
    if (n == 0) {
      return(list(
        n = 0, tp = 0, fp = 0, tn = 0, fn = 0,
        acuracia = NA_real_, precisao = NA_real_, sensibilidade = NA_real_,
        especificidade = NA_real_, f1 = NA_real_, auc = NA_real_,
        taxa_obs = NA_real_, taxa_prev = NA_real_
      ))
    }

    tp <- sum(pred == 1 & obs == 1)
    fp <- sum(pred == 1 & obs == 0)
    tn <- sum(pred == 0 & obs == 0)
    fn <- sum(pred == 0 & obs == 1)

    acuracia <- (tp + tn) / n
    precisao <- if ((tp + fp) > 0) tp / (tp + fp) else NA_real_
    sensibilidade <- if ((tp + fn) > 0) tp / (tp + fn) else NA_real_
    especificidade <- if ((tn + fp) > 0) tn / (tn + fp) else NA_real_
    f1 <- if ((2 * tp + fp + fn) > 0) (2 * tp) / (2 * tp + fp + fn) else NA_real_
    auc <- if (!is.null(prob)) calc_auc(prob[valid], obs) else NA_real_

    list(
      n = n, tp = tp, fp = fp, tn = tn, fn = fn,
      acuracia = acuracia, precisao = precisao, sensibilidade = sensibilidade,
      especificidade = especificidade, f1 = f1, auc = auc,
      taxa_obs = mean(obs == 1), taxa_prev = mean(pred == 1)
    )
  }

  backtest_ia <- reactive({
    df_bt <- dados_filtrados_dimensoes() %>%
      filter(ano != 2011) %>%
      group_by(ano, semana) %>%
      summarise(
        casos = sum(casos, na.rm = TRUE),
        surtos = sum(surtos, na.rm = TRUE),
        .groups = "drop"
      )

    if (nrow(df_bt) == 0) {
      return(
        list(
          predictions = tibble::tibble(),
          metrics_global = calc_class_metrics(integer(), integer()),
          metrics_year = tibble::tibble()
        )
      )
    }

    anos <- sort(unique(df_bt$ano))
    anos_candidatos <- anos
    if (length(anos) >= 3) {
      anos_candidatos <- anos[anos < max(anos)]
    }
    anos_candidatos <- anos_candidatos[anos_candidatos > min(anos)]

    if (length(anos_candidatos) == 0) {
      return(
        list(
          predictions = tibble::tibble(),
          metrics_global = calc_class_metrics(integer(), integer()),
          metrics_year = tibble::tibble()
        )
      )
    }

    pred_list <- lapply(anos_candidatos, function(ano_ref) {
      hist <- df_bt %>% filter(ano < ano_ref)
      if (nrow(hist) == 0) {
        return(NULL)
      }

      stats_hist <- hist %>%
        group_by(semana) %>%
        summarise(
          media_hist = mean(casos, na.rm = TRUE),
          dp_hist = stats::sd(casos, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(
          dp_hist = tidyr::replace_na(dp_hist, 0),
          ucl_hist = media_hist + 1.96 * dp_hist
        )

      serie_ref <- tibble::tibble(semana = 1:53) %>%
        left_join(
          df_bt %>% filter(ano == ano_ref) %>% select(semana, casos, surtos),
          by = "semana"
        ) %>%
        mutate(
          casos = tidyr::replace_na(casos, 0),
          surtos = tidyr::replace_na(surtos, 0)
        )

      semana_limite <- suppressWarnings(max(serie_ref$semana[(serie_ref$casos > 0) | (serie_ref$surtos > 0)], na.rm = TRUE))
      if (!is.finite(semana_limite)) {
        semana_limite <- suppressWarnings(max(serie_ref$semana, na.rm = TRUE))
      }
      if (!is.finite(semana_limite)) {
        return(NULL)
      }

      serie_ref <- serie_ref %>%
        filter(semana <= semana_limite) %>%
        left_join(stats_hist, by = "semana") %>%
        mutate(
          media_hist = tidyr::replace_na(media_hist, 0),
          dp_hist = tidyr::replace_na(dp_hist, 0),
          ucl_hist = dplyr::coalesce(ucl_hist, media_hist),
          zscore = ifelse(dp_hist > 0, (casos - media_hist) / dp_hist, 0),
          media_movel4 = roll_mean(casos, k = 4),
          media_movel4_prev = dplyr::lag(media_movel4, n = 4, default = dplyr::first(media_movel4)),
          crescimento = ifelse(media_movel4_prev > 0, (media_movel4 - media_movel4_prev) / media_movel4_prev, 0),
          crescimento = tidyr::replace_na(crescimento, 0),
          z_norm = pmin(pmax(zscore / 3, -2), 2),
          cresc_norm = pmin(pmax(crescimento, -1), 2),
          score_ia = 1.1 * z_norm + 0.8 * cresc_norm,
          prob_surto = stats::plogis(score_ia),
          alerta_previsto = prob_surto >= 0.70,
          observado = ifelse((surtos > 0) | (casos > ucl_hist), 1L, 0L),
          ano = ano_ref
        ) %>%
        select(ano, semana, casos, surtos, prob_surto, alerta_previsto, observado)

      serie_ref
    })

    pred_all <- dplyr::bind_rows(pred_list)

    if (nrow(pred_all) == 0) {
      return(
        list(
          predictions = tibble::tibble(),
          metrics_global = calc_class_metrics(integer(), integer()),
          metrics_year = tibble::tibble()
        )
      )
    }

    metrics_global <- calc_class_metrics(
      pred = pred_all$alerta_previsto,
      obs = pred_all$observado,
      prob = pred_all$prob_surto
    )

    metrics_year <- dplyr::bind_rows(lapply(sort(unique(pred_all$ano)), function(ano_i) {
      df_i <- pred_all %>% filter(ano == ano_i)
      m <- calc_class_metrics(df_i$alerta_previsto, df_i$observado, df_i$prob_surto)
      tibble::tibble(
        ano = ano_i,
        semanas = m$n,
        taxa_obs = m$taxa_obs,
        taxa_prev = m$taxa_prev,
        acuracia = m$acuracia,
        precisao = m$precisao,
        sensibilidade = m$sensibilidade,
        f1 = m$f1,
        auc = m$auc
      )
    }))

    list(
      predictions = pred_all,
      metrics_global = metrics_global,
      metrics_year = metrics_year
    )
  })

  motor_alerta_ia <- reactive({
    ano_ref <- selected_year()
    serie_atual <- serie_semanal_atual() %>% arrange(semana)
    tendencia <- modelo_tendencia()

    historico <- dados_filtrados_dimensoes() %>%
      filter(ano != ano_ref, ano != 2011) %>%
      group_by(ano, semana) %>%
      summarise(casos = sum(casos, na.rm = TRUE), .groups = "drop")

    historico_stats <- if (nrow(historico) == 0) {
      media_ref <- mean(serie_atual$casos, na.rm = TRUE)
      dp_ref <- stats::sd(serie_atual$casos, na.rm = TRUE)
      if (!is.finite(media_ref)) media_ref <- 0
      if (!is.finite(dp_ref)) dp_ref <- 0
      tibble::tibble(
        semana = 1:53,
        media_hist = media_ref,
        dp_hist = dp_ref
      )
    } else {
      historico %>%
        group_by(semana) %>%
        summarise(
          media_hist = mean(casos, na.rm = TRUE),
          dp_hist = stats::sd(casos, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(dp_hist = tidyr::replace_na(dp_hist, 0))
    }

    observado <- tibble::tibble(semana = 1:53) %>%
      left_join(serie_atual, by = "semana") %>%
      mutate(casos = tidyr::replace_na(casos, 0)) %>%
      left_join(historico_stats, by = "semana") %>%
      mutate(
        media_hist = tidyr::replace_na(media_hist, 0),
        dp_hist = tidyr::replace_na(dp_hist, 0),
        zscore = ifelse(dp_hist > 0, (casos - media_hist) / dp_hist, 0),
        media_movel4 = roll_mean(casos, k = 4),
        media_movel4_prev = dplyr::lag(media_movel4, n = 4, default = dplyr::first(media_movel4)),
        crescimento = ifelse(media_movel4_prev > 0, (media_movel4 - media_movel4_prev) / media_movel4_prev, 0),
        crescimento = tidyr::replace_na(crescimento, 0),
        z_norm = pmin(pmax(zscore / 3, -2), 2),
        cresc_norm = pmin(pmax(crescimento, -1), 2),
        score_ia = 1.1 * z_norm + 0.8 * cresc_norm,
        prob_surto = stats::plogis(score_ia),
        risco = dplyr::case_when(
          prob_surto >= 0.85 ~ "CRITICO",
          prob_surto >= 0.70 ~ "ALTO",
          prob_surto >= 0.50 ~ "MODERADO",
          TRUE ~ "BAIXO"
        ),
        alerta_previsto = prob_surto >= 0.70,
        semana_fechada = semana <= tendencia$semana_limite
      )

    media_recente <- mean(observado$casos[observado$semana_fechada], na.rm = TRUE)
    if (!is.finite(media_recente)) {
      media_recente <- 1
    }

    tendencia_norm <- pmin(
      pmax(tendencia$slope / pmax(media_recente, 1), -1),
      2
    )

    futuro <- tendencia$previsao %>%
      left_join(historico_stats, by = "semana") %>%
      mutate(
        media_hist = tidyr::replace_na(media_hist, 0),
        dp_hist = tidyr::replace_na(dp_hist, 0),
        zscore = ifelse(dp_hist > 0, (previsto - media_hist) / dp_hist, 0),
        z_norm = pmin(pmax(zscore / 3, -2), 2),
        score_ia = 1.1 * z_norm + 0.6 * tendencia_norm,
        prob_surto = stats::plogis(score_ia),
        risco = dplyr::case_when(
          prob_surto >= 0.85 ~ "CRITICO",
          prob_surto >= 0.70 ~ "ALTO",
          prob_surto >= 0.50 ~ "MODERADO",
          TRUE ~ "BAIXO"
        ),
        alerta_previsto = prob_surto >= 0.70
      )

    list(
      observado = observado,
      futuro = futuro,
      semana_limite = tendencia$semana_limite
    )
  })

  ia_resumo_atual <- reactive({
    motor <- motor_alerta_ia()
    observado <- motor$observado %>%
      filter(semana_fechada)

    if (nrow(observado) == 0) {
      return(
        list(
          nivel = "SEM DADOS",
          prob = 0,
          semanas_criticas = 0,
          mensagem = "Sem informação suficiente para predição."
        )
      )
    }

    ultima <- observado %>% arrange(desc(semana)) %>% slice(1)
    semanas_criticas <- sum(motor$futuro$alerta_previsto, na.rm = TRUE)

    mensagem <- if (semanas_criticas >= 3) {
      "Reforçar ações imediatas e investigação ativa."
    } else if (semanas_criticas >= 1) {
      "Monitorar semanalmente com plano de resposta local."
    } else {
      "Manter vigilância de rotina e prevenção."
    }

    list(
      nivel = as.character(ultima$risco),
      prob = as.numeric(ultima$prob_surto),
      semanas_criticas = as.integer(semanas_criticas),
      mensagem = mensagem
    )
  })

  alerta_ia_cache <- reactiveVal("")

  observeEvent(
    {
      list(
        selected_year(),
        input$filtro_ano,
        input$filtro_drs,
        input$filtro_gve,
        input$filtro_municipio,
        input$filtro_uvis
      )
    },
    {
      ia <- ia_resumo_atual()
      safe_input <- function(x, default) {
        if (is.null(x) || length(x) == 0 || is.na(x[1])) {
          return(default)
        }
        as.character(x[1])
      }

      chave_alerta <- paste(
        selected_year(),
        safe_input(input$filtro_ano, "Todos"),
        safe_input(input$filtro_drs, "Todas"),
        safe_input(input$filtro_gve, "Todas"),
        safe_input(input$filtro_municipio, "Todos"),
        safe_input(input$filtro_uvis, "Todas"),
        ia$nivel,
        sep = "|"
      )

      if (identical(chave_alerta, alerta_ia_cache())) {
        return(invisible(NULL))
      }

      alerta_ia_cache(chave_alerta)

      if (ia$nivel %in% c("ALTO", "CRITICO")) {
        showNotification(
          paste0(
            "Alerta preditivo IA (", ia$nivel, "): ",
            ia$mensagem,
            " Probabilidade atual: ",
            round(ia$prob * 100, 1), "%."
          ),
          type = "warning",
          duration = 8
        )
      }
    },
    ignoreInit = TRUE
  )

  output$grafico_tendencia <- renderPlotly({
    modelo <- modelo_tendencia()

    if (nrow(modelo$observado) == 0) {
      return(empty_plotly("Sem dados para estimar tendência"))
    }

    p <- plot_ly() %>%
      add_trace(
        data = modelo$observado,
        x = ~semana,
        y = ~casos,
        type = "scatter",
        mode = "lines",
        name = "Observado",
        line = list(color = "#005ca9", width = 2.4)
      ) %>%
      add_trace(
        data = modelo$observado,
        x = ~semana,
        y = ~tendencia,
        type = "scatter",
        mode = "lines",
        name = "Tendência (LOESS)",
        line = list(color = "#2e7d32", width = 2, dash = "dash")
      )

    if (nrow(modelo$previsao) > 0) {
      p <- p %>%
        add_trace(
          data = modelo$previsao,
          x = ~semana,
          y = ~previsto,
          type = "scatter",
          mode = "lines",
          name = "Projeção (8 semanas)",
          line = list(color = "#ef6c00", width = 2.2, dash = "dot")
        )
    }

    p %>%
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

  output$grafico_sexo <- renderPlotly({
    df <- dados_ano_referencia()

    if (nrow(df) == 0) {
      return(empty_plotly("Sem dados de sexo para os filtros selecionados"))
    }

    total_masc <- sum(df$sexo_masc, na.rm = TRUE)
    total_fem <- sum(df$sexo_fem, na.rm = TRUE)
    total_casos <- sum(df$casos, na.rm = TRUE)
    total_nao_info <- pmax(total_casos - total_masc - total_fem, 0)

    sexo_df <- tibble::tibble(
      sexo = c("Masculino", "Feminino", "Não informado"),
      casos = c(total_masc, total_fem, total_nao_info)
    ) %>%
      filter(casos > 0)

    if (nrow(sexo_df) == 0) {
      return(empty_plotly("Sem registros de sexo no período selecionado"))
    }

    plot_ly(
      sexo_df,
      labels = ~sexo,
      values = ~casos,
      type = "pie",
      hole = 0.45,
      marker = list(colors = c("#1976d2", "#ec407a", "#90a4ae"))
    ) %>%
      layout(
        margin = list(l = 20, r = 20, t = 10, b = 10),
        showlegend = TRUE,
        legend = list(orientation = "h", x = 0, y = -0.05),
        paper_bgcolor = "white"
      )
  })

  resumo_faixa_etaria <- reactive({
    df <- dados_ano_referencia()

    faixas <- c("< 1 ano", "1 a 4", "5 a 9", "10 a 14", "15+")
    zero_tbl <- tibble::tibble(faixa = faixas, casos = 0, origem = "sem_dados")

    if (nrow(df) == 0) {
      return(zero_tbl)
    }

    valores_observados <- c(
      sum(df$fx_menor1, na.rm = TRUE),
      sum(df$fx_1a4, na.rm = TRUE),
      sum(df$fx_5a9, na.rm = TRUE),
      sum(df$fx_10a14, na.rm = TRUE),
      sum(df$fx_15mais, na.rm = TRUE)
    )

    total_observado <- sum(valores_observados, na.rm = TRUE)
    if (is.finite(total_observado) && total_observado > 0) {
      return(tibble::tibble(faixa = faixas, casos = valores_observados, origem = "observado"))
    }

    zero_tbl
  })

  output$grafico_faixa_etaria <- renderPlotly({
    faixa_df <- resumo_faixa_etaria()
    if (nrow(faixa_df) == 0 || sum(faixa_df$casos, na.rm = TRUE) <= 0) {
      return(empty_plotly("Sem dados de faixa etária para os filtros selecionados"))
    }

    origem <- as.character(faixa_df$origem[1])
    anotacao <- switch(
      origem,
      observado = "Faixas etárias observadas na base",
      "Faixas etárias sem dados observados no recorte atual"
    )

    plot_ly(
      faixa_df,
      x = ~faixa,
      y = ~casos,
      type = "bar",
      marker = list(color = c("#90caf9", "#64b5f6", "#42a5f5", "#1e88e5", "#0d47a1")),
      name = "Casos"
    ) %>%
      layout(
        showlegend = FALSE,
        margin = list(l = 55, r = 20, t = 10, b = 45),
        plot_bgcolor = "rgba(245,245,245,0.5)",
        paper_bgcolor = "white",
        xaxis = list(title = "Faixa etária", showgrid = FALSE),
        yaxis = list(title = "Casos", showgrid = TRUE, gridcolor = "#e5e7eb"),
        annotations = list(
          list(
            x = 0.01,
            y = 1.12,
            xref = "paper",
            yref = "paper",
            xanchor = "left",
            yanchor = "bottom",
            text = anotacao,
            showarrow = FALSE,
            font = list(size = 11, color = "#52606d")
          )
        )
      )
  })

  output$grafico_acoes_treinamentos <- renderPlotly({
    df <- dados_ano_referencia()

    if (nrow(df) == 0) {
      return(empty_plotly("Sem dados de ações para os filtros selecionados"))
    }

    acoes_df <- tibble::tibble(
      acao = c("Ações educativas", "Treinamentos", "Coleta de material", "Encaminhamentos"),
      quantidade = c(
        sum(df$acao_educativa, na.rm = TRUE),
        sum(df$treinamentos, na.rm = TRUE),
        sum(df$coleta_material, na.rm = TRUE),
        sum(df$encaminhamentos, na.rm = TRUE)
      )
    )

    if (sum(acoes_df$quantidade, na.rm = TRUE) <= 0) {
      return(empty_plotly("Sem registros de ações no período selecionado"))
    }

    plot_ly(
      acoes_df,
      x = ~reorder(acao, quantidade),
      y = ~quantidade,
      type = "bar",
      orientation = "v",
      marker = list(color = c("#43a047", "#00897b", "#7cb342", "#5e35b1")),
      name = "Quantidade"
    ) %>%
      layout(
        showlegend = FALSE,
        margin = list(l = 55, r = 20, t = 10, b = 90),
        plot_bgcolor = "rgba(245,245,245,0.5)",
        paper_bgcolor = "white",
        xaxis = list(title = "Tipo de ação", tickangle = 20, showgrid = FALSE),
        yaxis = list(title = "Quantidade", showgrid = TRUE, gridcolor = "#e5e7eb")
      )
  })

  safe_weighted_mean <- function(x, w) {
    valid <- is.finite(x) & is.finite(w) & w > 0
    if (!any(valid)) {
      return(mean(x, na.rm = TRUE))
    }
    stats::weighted.mean(x[valid], w[valid], na.rm = TRUE)
  }

  incidencia_faixas <- c(
    "Sem dados",
    "Muito baixa (<50)",
    "Baixa (50 a <100)",
    "Moderada (100 a <300)",
    "Alta (300 a <500)",
    "Muito alta (>=500)"
  )

  classificar_faixa_incidencia <- function(incidencia) {
    dplyr::case_when(
      !is.finite(incidencia) ~ "Sem dados",
      incidencia < 50 ~ "Muito baixa (<50)",
      incidencia < 100 ~ "Baixa (50 a <100)",
      incidencia < 300 ~ "Moderada (100 a <300)",
      incidencia < 500 ~ "Alta (300 a <500)",
      TRUE ~ "Muito alta (>=500)"
    )
  }

  first_non_empty <- function(x) {
    x <- as.character(x)
    x <- trimws(x)
    x <- x[!is.na(x) & x != "" & tolower(x) != "na"]
    if (length(x) == 0) {
      return(NA_character_)
    }
    x[1]
  }

  extract_numeric_geo_key <- function(values, level = c("gve", "drs")) {
    level <- match.arg(level)
    values_norm <- normalize_key(values)
    has_num <- grepl("\\d+", values_norm)

    nums <- ifelse(
      has_num,
      suppressWarnings(as.integer(sub(".*?(\\d+).*", "\\1", values_norm))),
      NA_integer_
    )

    out <- rep(NA_character_, length(values_norm))
    valid <- !is.na(nums)
    out[valid] <- paste(level, nums[valid])
    out
  }

  nivel_mapa_atual <- reactive({
    nivel <- input$nivel_mapa
    if (is.null(nivel) || !(nivel %in% c("municipio", "gve", "drs"))) {
      return("municipio")
    }
    nivel
  })

  resumo_municipios <- reactive({
    df <- dados_ano_referencia()

    if (nrow(df) == 0) {
      return(
        tibble::tibble(
          municipio = character(),
          casos = numeric(),
          surtos = numeric(),
          populacao = numeric(),
          incidencia = numeric(),
          status_surto = character(),
          faixa_incidencia = character()
        )
      )
    }

    df %>%
      group_by(municipio) %>%
      summarise(
        casos = sum(casos, na.rm = TRUE),
        surtos = sum(surtos, na.rm = TRUE),
        populacao = max(populacao, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        populacao = ifelse(is.infinite(populacao) | populacao <= 0, NA_real_, populacao),
        incidencia = ifelse(populacao > 0, (casos / populacao) * 100000, NA_real_),
        status_surto = ifelse(surtos > 0, "Em surto", "Sem surto"),
        faixa_incidencia = classificar_faixa_incidencia(incidencia)
      ) %>%
      arrange(desc(incidencia), desc(casos), municipio)
  })

  tabela_operacional_municipios <- reactive({
    df <- dados_ano_referencia()

    if (nrow(df) == 0) {
      return(
        tibble::tibble(
          gve = character(),
          municipio = character(),
          ano = integer(),
          populacao = numeric(),
          total_ano = numeric(),
          incidencia_anual = numeric(),
          total_4s = numeric(),
          incidencia_4s = numeric(),
          alerta = character(),
          alerta_flag = integer()
        )
      )
    }

    semana_limite <- suppressWarnings(max(df$semana[df$casos > 0], na.rm = TRUE))
    if (!is.finite(semana_limite)) {
      semana_limite <- suppressWarnings(max(df$semana, na.rm = TRUE))
    }
    if (!is.finite(semana_limite)) {
      semana_limite <- 53
    }
    semana_inicio <- max(1, as.integer(semana_limite) - 3)

    resumo_ano <- df %>%
      group_by(gve, municipio, ano) %>%
      summarise(
        populacao = max(populacao, na.rm = TRUE),
        total_ano = sum(casos, na.rm = TRUE),
        surtos = sum(surtos, na.rm = TRUE),
        .groups = "drop"
      )

    resumo_4s <- df %>%
      filter(semana >= semana_inicio, semana <= semana_limite) %>%
      group_by(gve, municipio, ano) %>%
      summarise(
        total_4s = sum(casos, na.rm = TRUE),
        .groups = "drop"
      )

    resumo <- resumo_ano %>%
      left_join(resumo_4s, by = c("gve", "municipio", "ano")) %>%
      mutate(
        total_4s = tidyr::replace_na(total_4s, 0),
        populacao = ifelse(is.infinite(populacao) | populacao <= 0, NA_real_, populacao),
        incidencia_anual = ifelse(populacao > 0, (total_ano / populacao) * 100000, NA_real_),
        incidencia_4s = ifelse(populacao > 0, (total_4s / populacao) * 100000, NA_real_)
      )

    limiar_alerta <- suppressWarnings(stats::quantile(
      resumo$incidencia_4s[is.finite(resumo$incidencia_4s) & resumo$incidencia_4s > 0],
      probs = 0.75,
      na.rm = TRUE
    ))

    if (!is.finite(limiar_alerta) || limiar_alerta <= 0) {
      limiar_alerta <- Inf
    }

    resumo %>%
      mutate(
        alerta_flag = ifelse(surtos > 0 | (is.finite(incidencia_4s) & incidencia_4s >= limiar_alerta), 1L, 0L),
        alerta = ifelse(alerta_flag == 1L, "ALERTA", "ESTÁVEL")
      ) %>%
      arrange(desc(alerta_flag), desc(incidencia_4s), desc(total_4s), municipio)
  })

  output$tabela_municipios <- DT::renderDT({
    tabela_base <- tabela_operacional_municipios()

    tabela <- tabela_base %>%
      transmute(
        GVE = toupper(gve),
        MUNICIPIO = toupper(municipio),
        ANO = as.integer(ano),
        POPULACAO = populacao,
        TOTAL_ANO = total_ano,
        INCID_ANUAL = round(incidencia_anual, 2),
        TOTAL_4S = total_4s,
        INCID_4S = round(incidencia_4s, 2),
        ALERTA = alerta,
        alerta_flag = alerta_flag
      )

    DT::datatable(
      tabela,
      rownames = FALSE,
      class = "compact stripe hover",
      options = list(
        pageLength = 12,
        lengthMenu = c(12, 25, 50, 100),
        autoWidth = TRUE,
        order = list(list(9, "desc"), list(7, "desc"), list(6, "desc")),
        dom = "tip",
        columnDefs = list(
          list(visible = FALSE, targets = 9)
        )
      )
    ) %>%
      DT::formatCurrency(
        columns = c("POPULACAO", "TOTAL_ANO", "TOTAL_4S"),
        currency = "",
        interval = 3,
        mark = ".",
        dec.mark = ",",
        digits = 0
      ) %>%
      DT::formatCurrency(
        columns = c("INCID_ANUAL", "INCID_4S"),
        currency = "",
        interval = 3,
        mark = ".",
        dec.mark = ",",
        digits = 2
      ) %>%
      DT::formatStyle(
        "INCID_4S",
        valueColumns = "alerta_flag",
        backgroundColor = DT::styleEqual(c(1, 0), c("#f8d7da", "#d4edda")),
        color = DT::styleEqual(c(1, 0), c("#7f1d1d", "#14532d")),
        fontWeight = "700"
      ) %>%
      DT::formatStyle(
        "ALERTA",
        valueColumns = "alerta_flag",
        backgroundColor = DT::styleEqual(c(1, 0), c("#f8d7da", "#d4edda")),
        color = DT::styleEqual(c(1, 0), c("#7f1d1d", "#14532d")),
        fontWeight = "700"
      )
  }, server = FALSE)

  dados_mapa_nivel <- reactive({
    df <- dados_ano_referencia()
    nivel <- nivel_mapa_atual()

    if (nrow(df) == 0) {
      return(
        tibble::tibble(
          rotulo = character(),
          map_key = character(),
          map_key_alt = character(),
          casos = numeric(),
          surtos = numeric(),
          populacao = numeric(),
          incidencia = numeric(),
          status_surto = character(),
          faixa_incidencia = character()
        )
      )
    }

    base_pop <- df %>%
      distinct(drs, gve, municipio, populacao)

    if (nivel == "municipio") {
      return(
        resumo_municipios() %>%
          mutate(
            rotulo = municipio,
            map_key = normalize_shape_key(rotulo, level = "municipio"),
            map_key_alt = normalize_key(rotulo)
          ) %>%
          select(rotulo, map_key, map_key_alt, casos, surtos, populacao, incidencia, status_surto, faixa_incidencia)
      )
    }

    if (nivel == "gve") {
      casos_nivel <- df %>%
        group_by(gve) %>%
        summarise(
          casos = sum(casos, na.rm = TRUE),
          surtos = sum(surtos, na.rm = TRUE),
          gve_codigo = first_non_empty(gve_codigo),
          .groups = "drop"
        )

      pop_nivel <- base_pop %>%
        group_by(gve) %>%
        summarise(populacao = sum(populacao, na.rm = TRUE), .groups = "drop")

      return(
        casos_nivel %>%
          left_join(pop_nivel, by = "gve") %>%
          mutate(
            populacao = ifelse(populacao <= 0, NA_real_, populacao),
            incidencia = ifelse(populacao > 0, (casos / populacao) * 100000, NA_real_),
            status_surto = ifelse(surtos > 0, "Em surto", "Sem surto"),
            faixa_incidencia = classificar_faixa_incidencia(incidencia),
            rotulo = gve,
            map_key_nome = normalize_shape_key(rotulo, level = "gve"),
            map_key_codigo = normalize_shape_key(gve_codigo, level = "gve"),
            map_key_num = extract_numeric_geo_key(rotulo, level = "gve"),
            map_key = dplyr::coalesce(map_key_codigo, map_key_num, map_key_nome),
            map_key_alt = dplyr::coalesce(map_key_nome, map_key_num, map_key_codigo)
          ) %>%
          select(rotulo, map_key, map_key_alt, casos, surtos, populacao, incidencia, status_surto, faixa_incidencia)
      )
    }

    casos_nivel <- df %>%
      group_by(drs) %>%
      summarise(
        casos = sum(casos, na.rm = TRUE),
        surtos = sum(surtos, na.rm = TRUE),
        drs_codigo = first_non_empty(drs_codigo),
        .groups = "drop"
      )

    pop_nivel <- base_pop %>%
      group_by(drs) %>%
      summarise(populacao = sum(populacao, na.rm = TRUE), .groups = "drop")

    casos_nivel %>%
      left_join(pop_nivel, by = "drs") %>%
      mutate(
        populacao = ifelse(populacao <= 0, NA_real_, populacao),
        incidencia = ifelse(populacao > 0, (casos / populacao) * 100000, NA_real_),
        status_surto = ifelse(surtos > 0, "Em surto", "Sem surto"),
        faixa_incidencia = classificar_faixa_incidencia(incidencia),
        rotulo = drs,
        map_key_nome = normalize_shape_key(rotulo, level = "drs"),
        map_key_codigo = normalize_shape_key(drs_codigo, level = "drs"),
        map_key_num = extract_numeric_geo_key(rotulo, level = "drs"),
        map_key = dplyr::coalesce(map_key_codigo, map_key_num, map_key_nome),
        map_key_alt = dplyr::coalesce(map_key_nome, map_key_num, map_key_codigo)
      ) %>%
      select(rotulo, map_key, map_key_alt, casos, surtos, populacao, incidencia, status_surto, faixa_incidencia)
  })

  pontos_base_mapa <- reactive({
    nivel <- nivel_mapa_atual()
    df_filtrado <- dados_ano_referencia()

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

    pal <- colorFactor(
      palette = c("#dbeafe", "#bfdbfe", "#93c5fd", "#60a5fa", "#2563eb", "#1e3a8a"),
      levels = incidencia_faixas,
      ordered = TRUE,
      na.color = "#e5e7eb"
    )

    local_shape <- get_local_shape_layer(nivel)
    shape_source <- "local"

    sanitize_map_label <- function(x) {
      x <- as.character(x)
      x <- trimws(gsub("[\\r\\n\\t]+", " ", x))
      x <- gsub("\\s{2,}", " ", x)
      x[is.na(x) | x == ""] <- "Área sem identificação"
      x <- sub("^([^,;|/]+).*$", "\\1", x)
      x <- trimws(x)
      x[nchar(x) > 80] <- paste0(substr(x[nchar(x) > 80], 1, 80), "...")
      x
    }

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
      lookup_parts <- list(
        df_mapa %>%
          transmute(
            join_key = as.character(map_key),
            rotulo_dado = rotulo,
            casos, surtos, populacao, incidencia, status_surto, faixa_incidencia
          ),
        df_mapa %>%
          transmute(
            join_key = as.character(map_key_alt),
            rotulo_dado = rotulo,
            casos, surtos, populacao, incidencia, status_surto, faixa_incidencia
          ),
        df_mapa %>%
          transmute(
            join_key = normalize_shape_key(rotulo, level = nivel),
            rotulo_dado = rotulo,
            casos, surtos, populacao, incidencia, status_surto, faixa_incidencia
          )
      )

      if (nivel %in% c("gve", "drs")) {
        lookup_parts <- c(
          lookup_parts,
          list(
            df_mapa %>%
              transmute(
                join_key = extract_numeric_geo_key(rotulo, level = nivel),
                rotulo_dado = rotulo,
                casos, surtos, populacao, incidencia, status_surto, faixa_incidencia
              )
          )
        )
      }

      mapa_lookup <- bind_rows(lookup_parts) %>%
        mutate(join_key = trimws(as.character(join_key))) %>%
        filter(!is.na(join_key), join_key != "") %>%
        arrange(desc(casos), desc(incidencia)) %>%
        distinct(join_key, .keep_all = TRUE)

      shp_join <- local_shape
      shape_key_1 <- as.character(shp_join$map_key)
      shape_key_2 <- if ("map_key_alt" %in% names(shp_join)) as.character(shp_join$map_key_alt) else rep(NA_character_, nrow(shp_join))
      shape_label_base <- if ("map_label" %in% names(shp_join)) as.character(shp_join$map_label) else as.character(shape_key_1)
      shape_key_3 <- normalize_shape_key(shape_label_base, level = nivel)
      shape_key_4 <- if (nivel %in% c("gve", "drs")) extract_numeric_geo_key(shape_label_base, level = nivel) else rep(NA_character_, nrow(shp_join))

      lookup_keys <- mapa_lookup$join_key
      idx_1 <- match(shape_key_1, lookup_keys)
      idx_2 <- match(shape_key_2, lookup_keys)
      idx_3 <- match(shape_key_3, lookup_keys)
      idx_4 <- match(shape_key_4, lookup_keys)

      idx_match <- ifelse(
        !is.na(idx_1), idx_1,
        ifelse(!is.na(idx_2), idx_2, ifelse(!is.na(idx_3), idx_3, idx_4))
      )

      shp_join$join_key <- lookup_keys[idx_match]
      shp_join$rotulo_dado <- mapa_lookup$rotulo_dado[idx_match]
      shp_join$casos <- mapa_lookup$casos[idx_match]
      shp_join$surtos <- mapa_lookup$surtos[idx_match]
      shp_join$populacao <- mapa_lookup$populacao[idx_match]
      shp_join$incidencia <- mapa_lookup$incidencia[idx_match]
      shp_join$status_surto <- mapa_lookup$status_surto[idx_match]
      shp_join$faixa_incidencia <- mapa_lookup$faixa_incidencia[idx_match]

      matched_polygons <- sum(!is.na(shp_join$casos))
      if (matched_polygons > 0) {
        shp_join$label_exibicao <- ifelse(
          is.na(shp_join$rotulo_dado) | shp_join$rotulo_dado == "",
          shape_label_base,
          shp_join$rotulo_dado
        )
        shp_join$label_exibicao <- sanitize_map_label(shp_join$label_exibicao)

        shp_join$surto_exibicao <- ifelse(
          is.na(shp_join$status_surto) | shp_join$status_surto == "",
          "Sem dados",
          as.character(shp_join$status_surto)
        )

        shp_join$faixa_exibicao <- ifelse(
          is.na(shp_join$faixa_incidencia) | shp_join$faixa_incidencia == "",
          "Sem dados",
          as.character(shp_join$faixa_incidencia)
        )

        shp_join$layer_id <- ifelse(
          !is.na(shp_join$rotulo_dado) & shp_join$rotulo_dado != "",
          normalize_key(shp_join$rotulo_dado),
          dplyr::coalesce(shp_join$join_key, shape_key_1)
        )
        shp_join$layer_id[is.na(shp_join$layer_id) | shp_join$layer_id == ""] <- paste0("shape_", seq_len(nrow(shp_join)))

        shp_join$tooltip <- ifelse(
          is.na(shp_join$casos),
          paste0("<strong>", shp_join$label_exibicao, "</strong><br>Sem notificações para os filtros"),
          paste0(
            "<strong>", shp_join$label_exibicao, "</strong><br>",
            "Casos: ", format(round(shp_join$casos), big.mark = ".", decimal.mark = ","), "<br>",
            "Incidência: ", format(round(shp_join$incidencia, 2), decimal.mark = ","), "/100 mil<br>",
            "Faixa de incidência: ", shp_join$faixa_exibicao, "<br>",
            "Situação de surto: ", shp_join$surto_exibicao
          )
        )

        titulo_legenda <- ifelse(
          shape_source == "local",
          "Faixa de incidência (/100 mil)",
          "Faixa de incidência (/100 mil) - fallback"
        )

        return(
          leaflet(shp_join) %>%
            addProviderTiles("CartoDB.Positron") %>%
            addPolygons(
              fillColor = ~pal(faixa_incidencia),
              color = "#d1d5db",
              weight = 1,
              fillOpacity = 0.8,
              smoothFactor = 0.2,
              layerId = ~layer_id,
              popup = ~htmltools::HTML(tooltip),
              highlightOptions = highlightOptions(weight = 2, color = "#005ca9", bringToFront = TRUE)
            ) %>%
            addLegend(
              colors = unname(pal(incidencia_faixas)),
              labels = incidencia_faixas,
              title = titulo_legenda,
              position = "bottomright",
              opacity = 0.9
            )
        )
      }
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
        layerId = ~ifelse(!is.na(map_key) & map_key != "", as.character(map_key), as.character(rotulo)),
        radius = ~pmax(6, sqrt(casos) / 1.5),
        stroke = TRUE,
        weight = 1.2,
        color = "#0f172a",
        fillColor = ~pal(faixa_incidencia),
        fillOpacity = 0.85,
        popup = ~paste0(
          "<strong>", titulo_nivel, ": ", rotulo, "</strong><br>",
          "Casos: ", format(round(casos), big.mark = ".", decimal.mark = ","), "<br>",
          "Incidência: ", format(round(incidencia, 2), decimal.mark = ","), "/100 mil<br>",
          "Faixa de incidência: ", faixa_incidencia, "<br>",
          "Situação de surto: ", status_surto
        )
      ) %>%
      addLegend(
        colors = unname(pal(incidencia_faixas)),
        labels = incidencia_faixas,
        title = paste0("Faixa de incidência (/100 mil) - ", toupper(nivel)),
        position = "bottomright",
        opacity = 0.9
      )
  })

  selecionar_municipio_no_filtro <- function(clicked_id) {
    if (is.null(clicked_id) || clicked_id == "") {
      return(invisible(NULL))
    }

    if (nivel_mapa_atual() != "municipio") {
      showNotification(
        "Para filtrar por clique, selecione o mapa no nível Município.",
        type = "message",
        duration = 3
      )
      return(invisible(NULL))
    }

    df_mapa <- dados_mapa_nivel()
    if (nrow(df_mapa) == 0) {
      return(invisible(NULL))
    }

    clicked_key <- normalize_key(clicked_id)
    match_tbl <- df_mapa %>%
      filter(
        map_key == clicked_id |
          map_key_alt == clicked_id |
          normalize_key(map_key) == clicked_key |
          normalize_key(map_key_alt) == clicked_key |
          normalize_key(rotulo) == clicked_key
      )

    if (nrow(match_tbl) == 0) {
      return(invisible(NULL))
    }

    municipio_clicked <- as.character(match_tbl$rotulo[1])
    if (is.na(municipio_clicked) || municipio_clicked == "") {
      return(invisible(NULL))
    }

    updateSelectInput(session, "filtro_drs", selected = "Todas")
    updateSelectInput(session, "filtro_gve", selected = "Todas")
    if (!is.null(input$filtro_uvis)) {
      updateSelectInput(session, "filtro_uvis", selected = "Todas")
    }

    if (requireNamespace("later", quietly = TRUE)) {
      later::later(
        function() updateSelectInput(session, "filtro_municipio", selected = municipio_clicked),
        delay = 0.12
      )
    } else {
      updateSelectInput(session, "filtro_municipio", selected = municipio_clicked)
    }
  }

  observeEvent(input$mapa_shape_click, {
    selecionar_municipio_no_filtro(input$mapa_shape_click$id)
  }, ignoreInit = TRUE)

  observeEvent(input$mapa_marker_click, {
    selecionar_municipio_no_filtro(input$mapa_marker_click$id)
  }, ignoreInit = TRUE)
}
