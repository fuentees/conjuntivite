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
  compute_ano_padrao <- function(df) {
    anos <- sort(unique(df$ano), decreasing = TRUE)
    if (length(anos) > 0) {
      return(as.character(anos[1]))
    }
    as.character(format(Sys.Date(), "%Y"))
  }

  refresh_minutes <- suppressWarnings(as.numeric(Sys.getenv("OFTALMO_REFRESH_MINUTES", unset = "5")))
  refresh_segundos <- if (is.finite(refresh_minutes) && refresh_minutes > 0) refresh_minutes * 60 else 0

  dataset_signature <- function(df) {
    if (is.null(df) || nrow(df) == 0) {
      return("empty")
    }
    ano_max <- suppressWarnings(max(df$ano, na.rm = TRUE))
    semana_max <- suppressWarnings(max(df$semana[df$ano == ano_max], na.rm = TRUE))
    total_casos <- suppressWarnings(sum(df$casos, na.rm = TRUE))
    paste(nrow(df), ano_max, semana_max, total_casos, sep = "|")
  }

  dados_runtime <- reactiveVal(dados_epidemiologicos)
  fonte_runtime <- reactiveVal(dados_fonte)
  assinatura_runtime <- reactiveVal(dataset_signature(dados_epidemiologicos))
  ultima_atualizacao <- reactiveVal(carga_inicial$carregado_em)
  erro_carga <- reactiveVal(carga_inicial$erro)
  ano_choices_cache <- reactiveVal(c("Todos", as.character(sort(unique(dados_epidemiologicos$ano), decreasing = TRUE))))

  dados_base <- reactive({
    dados_runtime()
  })

  anos_ordenados <- reactive({
    sort(unique(dados_base()$ano), decreasing = TRUE)
  })

  if (refresh_segundos > 0) {
    observe({
      invalidateLater(refresh_segundos * 1000, session)

      # O cache é compartilhado: com várias sessões abertas, o banco é consultado
      # no máximo uma vez por intervalo.
      carga <- obter_dados_painel(max_idade_segundos = refresh_segundos)
      erro_carga(carga$erro)
      if (!is.null(carga$erro)) {
        warning("Falha ao atualizar dados do painel: ", carga$erro, call. = FALSE)
      }

      if (is.null(carga$dados) || nrow(carga$dados) == 0) {
        return(invisible(NULL))
      }

      assinatura_nova <- dataset_signature(carga$dados)
      if (!identical(assinatura_nova, isolate(assinatura_runtime()))) {
        dados_runtime(carga$dados)
        assinatura_runtime(assinatura_nova)
      }
      ultima_atualizacao(carga$carregado_em)
    })
  }

  # Última semana epidemiológica com notificação na base inteira.
  ultima_semana_base <- reactive({
    df <- dados_base()
    if (nrow(df) == 0) {
      return(list(ano = NA_integer_, semana = NA_integer_))
    }
    ano_max <- max(df$ano, na.rm = TRUE)
    list(ano = ano_max, semana = max(df$semana[df$ano == ano_max], na.rm = TRUE))
  })

  # Primeiro ano em que um campo detalhado (sexo, faixa etária, surtos, ações) aparece preenchido.
  primeiro_ano_com <- function(df, colunas) {
    anos <- df$ano[rowSums(as.matrix(df[, colunas, drop = FALSE]), na.rm = TRUE) > 0]
    if (length(anos) == 0) NA_integer_ else min(anos)
  }

  primeiro_ano_detalhes <- reactive({
    df <- dados_base()
    list(
      sexo = primeiro_ano_com(df, c("sexo_masc", "sexo_fem")),
      faixa = primeiro_ano_com(df, c("fx_menor1", "fx_1a4", "fx_5a9", "fx_10a14", "fx_15mais")),
      surtos = primeiro_ano_com(df, "surtos"),
      acoes = primeiro_ano_com(df, c("acao_educativa", "treinamentos", "coleta_material", "encaminhamentos"))
    )
  })

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
      df_ref <- dados_base()

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

      anos_atuais <- anos_ordenados()
      if (length(anos_atuais) > 0) {
        return(as.integer(anos_atuais[1]))
      }
      return(as.integer(format(Sys.Date(), "%Y")))
    }

    suppressWarnings(as.integer(input$filtro_ano))
  })

  is_all_years_selected <- reactive({
    identical(input$filtro_ano, "Todos")
  })

  # Última semana a exibir no ano: a última com dados no ano corrente da base,
  # ou o total de semanas epidemiológicas daquele ano.
  semana_final_ano <- function(ano) {
    ultima <- ultima_semana_base()
    if (!is.na(ultima$ano) && identical(as.integer(ano), as.integer(ultima$ano))) {
      return(as.integer(ultima$semana))
    }
    semanas_no_ano_epidemiologico(ano)
  }

  rotulo_ano_referencia <- reactive({
    ano <- selected_year()
    paste0(ano, " (SE 1 a ", semana_final_ano(ano), ")")
  })

  rotulo_periodo <- reactive({
    if (is_all_years_selected()) {
      anos <- anos_ordenados()
      if (length(anos) == 0) {
        return("todos os anos")
      }
      return(paste0(min(anos), " a ", max(anos)))
    }
    rotulo_ano_referencia()
  })

  output$fonte_dados <- renderText({
    ultima <- ultima_semana_base()
    dados_ate <- if (is.na(ultima$ano)) "sem dados" else paste0("SE ", ultima$semana, "/", ultima$ano)
    paste0(
      "Fonte: banco MariaDB · População: IBGE · Dados até ", dados_ate,
      " · Atualizado em ", format(ultima_atualizacao(), "%d/%m/%Y %H:%M")
    )
  })

  output$aviso_carga <- renderUI({
    erro <- erro_carga()
    if (is.null(erro) || erro == "") {
      return(NULL)
    }
    texto <- if (nrow(dados_base()) == 0) {
      "Não foi possível carregar os dados do banco. O painel tentará novamente em alguns minutos."
    } else {
      "A última atualização falhou. O painel mostra os dados da carga anterior."
    }
    # O detalhe técnico fica só no log do servidor, para não expor dados de conexão.
    div(class = "alert alert-warning py-2 mb-3", strong(texto))
  })

  clean_choice_values <- function(values) {
    values <- as.character(values)
    values <- trimws(values)
    values <- values[!is.na(values) & values != "" & tolower(values) != "na"]
    sort(unique(values))
  }

  all_drs_choices <- reactive({
    choices <- clean_choice_values(dados_base()$drs)
    if (length(choices) == 0) choices <- "Não informado"
    choices
  })
  all_gve_choices <- reactive({
    choices <- clean_choice_values(dados_base()$gve)
    if (length(choices) == 0) choices <- "Não informado"
    choices
  })
  all_municipio_choices <- reactive({
    choices <- clean_choice_values(dados_base()$municipio)
    if (length(choices) == 0) choices <- "Não informado"
    choices
  })

  observeEvent(dados_base(), {
    anos_choices <- sort(unique(dados_base()$ano), decreasing = TRUE)
    if (length(anos_choices) == 0) {
      anos_choices <- as.integer(format(Sys.Date(), "%Y"))
    }
    novos_choices <- c("Todos", as.character(anos_choices))

    selected_ano <- if (!is.null(input$filtro_ano) && input$filtro_ano %in% novos_choices) {
      input$filtro_ano
    } else {
      compute_ano_padrao(dados_base())
    }

    needs_update <- !identical(novos_choices, ano_choices_cache()) ||
      is.null(input$filtro_ano) ||
      !(input$filtro_ano %in% novos_choices)

    if (isTRUE(needs_update)) {
      updateSelectInput(
        session,
        "filtro_ano",
        choices = novos_choices,
        selected = selected_ano
      )
      ano_choices_cache(novos_choices)
    }

    drs_choices <- all_drs_choices()
    updateSelectInput(
      session,
      "filtro_drs",
      choices = c("Todas", drs_choices),
      selected = manter_selecao(isolate(input$filtro_drs), drs_choices, "Todas")
    )
  }, ignoreInit = TRUE)

  is_sao_paulo_selected <- reactive({
    !is.null(input$filtro_municipio) &&
      input$filtro_municipio != "Todos" &&
      normalize_key(input$filtro_municipio) == "sao paulo"
  })

  is_municipio_selected <- reactive({
    !is.null(input$filtro_municipio) && input$filtro_municipio != "Todos"
  })

  uvis_choices_for_context <- reactive({
    if (!is_sao_paulo_selected()) {
      return(character())
    }

    df <- if (identical(input$filtro_ano, "Todos")) {
      dados_base()
    } else {
      dados_base() %>% filter(ano == selected_year())
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

  # As opções de DRS, GVE e município vêm da base inteira (não só do ano escolhido),
  # para que trocar o ano não apague o território selecionado.
  manter_selecao <- function(atual, opcoes, padrao) {
    if (!is.null(atual) && atual %in% opcoes) atual else padrao
  }

  observeEvent(list(input$filtro_drs, dados_base()), {
    df <- dados_base()
    if (!is.null(input$filtro_drs) && input$filtro_drs != "Todas") {
      df <- df %>% filter(drs == input$filtro_drs)
    }
    gve_choices <- clean_choice_values(df$gve)
    if (length(gve_choices) == 0) {
      gve_choices <- all_gve_choices()
    }

    updateSelectInput(
      session,
      "filtro_gve",
      choices = c("Todas", gve_choices),
      selected = manter_selecao(input$filtro_gve, gve_choices, "Todas")
    )
  }, ignoreNULL = FALSE)

  observeEvent(list(input$filtro_drs, input$filtro_gve, dados_base()), {
    df <- dados_base()
    if (!is.null(input$filtro_drs) && input$filtro_drs != "Todas") {
      df <- df %>% filter(drs == input$filtro_drs)
    }
    if (!is.null(input$filtro_gve) && input$filtro_gve != "Todas") {
      df <- df %>% filter(gve == input$filtro_gve)
    }
    mun_choices <- clean_choice_values(df$municipio)
    if (length(mun_choices) == 0) {
      mun_choices <- all_municipio_choices()
    }

    updateSelectInput(
      session,
      "filtro_municipio",
      choices = c("Todos", mun_choices),
      selected = manter_selecao(input$filtro_municipio, mun_choices, "Todos")
    )
  }, ignoreNULL = FALSE)

  observeEvent(input$btn_reset, {
    updateSelectInput(session, "filtro_ano", selected = compute_ano_padrao(dados_base()))
    updateSelectInput(session, "filtro_drs", selected = "Todas")
    updateSelectInput(session, "filtro_gve", selected = "Todas")
    updateSelectInput(session, "filtro_municipio", selected = "Todos")
    if (!is.null(input$filtro_uvis)) {
      updateSelectInput(session, "filtro_uvis", selected = "Todas")
    }
  })

  dados_filtrados_dimensoes <- reactive({
    req(input$filtro_ano)
    df <- dados_base()

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

  dados_contexto_periodo <- reactive({
    if (is_all_years_selected()) {
      return(dados_filtrados_dimensoes())
    }
    dados_ano_referencia()
  })

  serie_semanal_atual <- reactive({
    ano_ref <- selected_year()
    df <- dados_ano_referencia() %>%
      group_by(semana) %>%
      summarise(casos = sum(casos, na.rm = TRUE), .groups = "drop")

    tibble::tibble(semana = seq_len(semanas_no_ano_epidemiologico(ano_ref))) %>%
      left_join(df, by = "semana") %>%
      mutate(
        casos = tidyr::replace_na(casos, 0),
        casos = ifelse(semana > semana_final_ano(ano_ref), NA_real_, casos)
      )
  })

  # Denominador: população IBGE dos municípios do território filtrado que já
  # notificaram alguma vez nesta base (rede notificante), somada nos anos do período.
  populacao_periodo <- function(anos) {
    if (is_sao_paulo_selected() && !is.null(input$filtro_uvis) && input$filtro_uvis != "Todas") {
      return(NA_real_)
    }
    codigos <- unique(stats::na.omit(dados_filtrados_dimensoes()$cod_ibge))
    if (length(codigos) == 0 || length(anos) == 0) {
      return(NA_real_)
    }
    pop <- load_populacao_ibge()
    anos_ref <- pmin(pmax(as.integer(anos), min(pop$ano)), max(pop$ano))
    total <- sum(vapply(
      anos_ref,
      function(a) sum(pop$populacao[pop$ano == a & pop$cod_ibge %in% codigos], na.rm = TRUE),
      numeric(1)
    ))
    if (total > 0) total else NA_real_
  }

  anos_periodo <- reactive({
    if (is_all_years_selected()) anos_ordenados() else selected_year()
  })

  canal_anos_excluidos <- local({
    valor <- Sys.getenv("OFTALMO_CANAL_ANOS_EXCLUIDOS", unset = "2011,2020,2021")
    anos <- suppressWarnings(as.integer(trimws(strsplit(valor, ",", fixed = TRUE)[[1]])))
    anos[!is.na(anos)]
  })
  canal_janela_anos <- local({
    valor <- suppressWarnings(as.integer(Sys.getenv("OFTALMO_CANAL_JANELA", unset = "10")))
    if (is.na(valor) || valor < 3) 10L else valor
  })

  # Canal endêmico pelo método dos quartis: mediana e quartis 1 e 3 de cada semana
  # nos anos anteriores ao selecionado (janela móvel, sem os anos atípicos).
  canal_endemico <- reactive({
    req(input$filtro_ano)
    ano_sel <- selected_year()
    n_semanas <- semanas_no_ano_epidemiologico(ano_sel)

    df <- dados_filtrados_dimensoes() %>%
      group_by(ano, semana) %>%
      summarise(casos = sum(casos, na.rm = TRUE), .groups = "drop")

    anos_historicos <- sort(unique(df$ano[df$ano < ano_sel & !(df$ano %in% canal_anos_excluidos)]), decreasing = TRUE)
    anos_historicos <- utils::head(anos_historicos, canal_janela_anos)

    base <- tibble::tibble(semana = seq_len(n_semanas))

    if (length(anos_historicos) == 0) {
      stats_semanal <- base %>% mutate(q1 = NA_real_, mediana = NA_real_, q3 = NA_real_)
    } else {
      # Semanas sem notificação em um ano histórico contam como zero caso.
      historico <- tidyr::expand_grid(ano = anos_historicos, semana = seq_len(n_semanas)) %>%
        left_join(df, by = c("ano", "semana")) %>%
        mutate(casos = tidyr::replace_na(casos, 0))

      stats_semanal <- historico %>%
        group_by(semana) %>%
        summarise(
          q1 = as.numeric(stats::quantile(casos, 0.25, names = FALSE)),
          mediana = as.numeric(stats::median(casos)),
          q3 = as.numeric(stats::quantile(casos, 0.75, names = FALSE)),
          .groups = "drop"
        )
    }

    atual <- df %>%
      filter(ano == ano_sel) %>%
      select(semana, casos)

    resultado <- base %>%
      left_join(stats_semanal, by = "semana") %>%
      left_join(atual, by = "semana") %>%
      mutate(
        casos = tidyr::replace_na(casos, 0),
        casos = ifelse(semana > semana_final_ano(ano_sel), NA_real_, casos)
      )

    attr(resultado, "anos_historicos") <- sort(anos_historicos)
    resultado
  })

  situacao_data <- reactive({
    canal_df <- canal_endemico()
    ano_sel <- selected_year()

    if (length(attr(canal_df, "anos_historicos")) == 0) {
      return(list(status = "SEM HISTÓRICO", descricao = "Sem anos anteriores para comparar"))
    }

    # A semana mais recente da base ainda recebe notificações em atraso; fica fora da avaliação.
    observadas <- canal_df %>% filter(is.finite(casos))
    if (identical(as.integer(ano_sel), as.integer(ultima_semana_base()$ano)) && nrow(observadas) > 1) {
      observadas <- observadas %>% filter(semana < max(semana))
    }

    ultimas <- observadas %>%
      arrange(desc(semana)) %>%
      slice_head(n = 4)

    if (nrow(ultimas) == 0) {
      return(list(status = "SEM DADOS", descricao = "Sem dados para classificação"))
    }

    media_recente <- mean(ultimas$casos, na.rm = TRUE)
    mediana_esperada <- mean(ultimas$mediana, na.rm = TRUE)
    limite_epidemico <- mean(ultimas$q3, na.rm = TRUE)
    periodo <- paste0("SE ", min(ultimas$semana), " a ", max(ultimas$semana))

    if (media_recente > limite_epidemico) {
      return(list(status = "EPIDEMIA", descricao = paste0("Acima do 3º quartil histórico (", periodo, ")")))
    }

    if (media_recente > mediana_esperada) {
      return(list(status = "ALERTA", descricao = paste0("Acima da mediana histórica (", periodo, ")")))
    }

    list(status = "NORMAL", descricao = paste0("Até a mediana histórica (", periodo, ")"))
  })

  output$total_casos <- renderText({
    df <- dados_contexto_periodo()
    format(sum(df$casos, na.rm = TRUE), big.mark = ".", decimal.mark = ",")
  })

  output$total_casos_subtitulo <- renderText({
    if (is_all_years_selected()) {
      return(rotulo_periodo())
    }
    ano_ref <- selected_year()
    semana_ref <- semana_final_ano(ano_ref)
    atual <- sum(dados_ano_referencia()$casos, na.rm = TRUE)
    anterior <- dados_filtrados_dimensoes() %>%
      filter(ano == ano_ref - 1, semana <= semana_ref) %>%
      summarise(casos = sum(casos, na.rm = TRUE)) %>%
      pull(casos)

    if (length(anterior) == 0 || !is.finite(anterior) || anterior <= 0) {
      return(rotulo_ano_referencia())
    }
    variacao <- (atual - anterior) / anterior * 100
    paste0(
      rotulo_ano_referencia(), " · ",
      ifelse(variacao >= 0, "+", ""), format(round(variacao, 1), decimal.mark = ","),
      "% vs mesmo período de ", ano_ref - 1
    )
  })

  incidencia_periodo <- reactive({
    total_casos <- sum(dados_contexto_periodo()$casos, na.rm = TRUE)
    pop <- populacao_periodo(anos_periodo())
    if (!is.finite(pop) || pop <= 0) NA_real_ else total_casos / pop * 100000
  })

  output$incidencia_media <- renderText({
    incidencia <- incidencia_periodo()
    if (!is.finite(incidencia)) {
      return("—")
    }
    paste0(format(round(incidencia, 1), nsmall = 1, decimal.mark = ",", big.mark = "."), "/100 mil")
  })

  output$incidencia_subtitulo <- renderText({
    if (is_sao_paulo_selected() && !is.null(input$filtro_uvis) && input$filtro_uvis != "Todas") {
      return("Sem população IBGE por UVIS")
    }
    if (is_all_years_selected()) {
      return(paste0("Média anual por 100 mil hab., ", rotulo_periodo()))
    }
    paste0("Acumulada por 100 mil hab., ", rotulo_ano_referencia())
  })

  output$num_surtos <- renderText({
    surtos_notificados <- sum(dados_contexto_periodo()$surtos, na.rm = TRUE)
    if (!is.finite(surtos_notificados)) {
      surtos_notificados <- 0
    }

    format(as.integer(round(surtos_notificados)), big.mark = ".", decimal.mark = ",")
  })

  output$num_surtos_subtitulo <- renderText({
    primeiro <- primeiro_ano_detalhes()$surtos
    if (!is_all_years_selected() && !is.na(primeiro) && selected_year() < primeiro) {
      return(paste0("Registro de surtos só existe a partir de ", primeiro))
    }
    paste0("Surtos notificados, ", rotulo_periodo())
  })

  output$situacao_epidemiologica <- renderText({
    situacao_data()$status
  })

  output$situacao_descricao <- renderText({
    situacao_data()$descricao
  })

  output$titulo_serie_temporal <- renderText({
    if (is_all_years_selected()) {
      return(paste0("Casos por ano — ", rotulo_periodo()))
    }
    paste0("Casos por semana epidemiológica — ", rotulo_ano_referencia())
  })

  output$grafico_serie_temporal <- renderPlotly({
    if (is_all_years_selected()) {
      anual <- dados_filtrados_dimensoes() %>%
        group_by(ano) %>%
        summarise(casos = sum(casos, na.rm = TRUE), .groups = "drop") %>%
        arrange(ano)

      if (nrow(anual) == 0) {
        return(empty_plotly("Sem dados para os filtros selecionados"))
      }

      ultima <- ultima_semana_base()
      anual <- anual %>%
        mutate(
          rotulo = ifelse(
            ano == ultima$ano & ultima$semana < semanas_no_ano_epidemiologico(ano),
            paste0(ano, " (até SE ", ultima$semana, ")"),
            as.character(ano)
          )
        )

      return(
        plot_ly(
          anual,
          x = ~ano,
          y = ~casos,
          type = "bar",
          text = ~rotulo,
          textposition = "none",
          marker = list(color = "#1976d2", opacity = 0.9),
          hovertemplate = "%{text}<br>Casos: %{y:,.0f}<extra></extra>"
        ) %>%
          layout(
            separators = ",.",
            margin = list(l = 65, r = 20, t = 10, b = 45),
            plot_bgcolor = "rgba(245,245,245,0.5)",
            paper_bgcolor = "white",
            xaxis = list(title = "Ano", dtick = 1, tickangle = -45, showgrid = FALSE),
            yaxis = list(title = "Casos", showgrid = TRUE, gridcolor = "#e5e7eb")
          )
      )
    }

    ano_ref <- selected_year()
    n_semanas <- semanas_no_ano_epidemiologico(ano_ref)
    semana_limite <- semana_final_ano(ano_ref)
    serie_contexto <- dados_filtrados_dimensoes() %>%
      group_by(ano, semana) %>%
      summarise(casos = sum(casos, na.rm = TRUE), .groups = "drop")

    serie <- tibble::tibble(semana = seq_len(n_semanas)) %>%
      left_join(
        serie_contexto %>%
          filter(ano == ano_ref) %>%
          select(semana, casos),
        by = "semana"
      ) %>%
      mutate(casos = tidyr::replace_na(casos, 0)) %>%
      arrange(semana)

    # A média móvel do início do ano usa as últimas semanas do ano anterior.
    cauda_ano_anterior <- tibble::tibble(semana = seq_len(semanas_no_ano_epidemiologico(ano_ref - 1))) %>%
      left_join(
        serie_contexto %>% filter(ano == ano_ref - 1) %>% select(semana, casos),
        by = "semana"
      ) %>%
      mutate(casos = tidyr::replace_na(casos, 0)) %>%
      pull(casos) %>%
      utils::tail(3)
    if (!any(serie_contexto$ano == ano_ref - 1)) {
      cauda_ano_anterior <- numeric()
    }

    casos_base_mm <- c(cauda_ano_anterior, serie$casos)
    media_movel_continua <- roll_mean(casos_base_mm, k = 4)
    media_movel_atual <- media_movel_continua[(length(cauda_ano_anterior) + 1):length(media_movel_continua)]

    serie_plot <- serie %>%
      mutate(
        casos_plot = ifelse(semana <= semana_limite, casos, NA_real_),
        media_movel_4s = ifelse(semana <= semana_limite, media_movel_atual, NA_real_)
      )

    plot_ly(
      serie_plot,
      x = ~semana,
      y = ~casos_plot,
      type = "bar",
      name = "Casos por semana",
      marker = list(color = "#1976d2", opacity = 0.9),
      hovertemplate = "SE %{x}<br>Casos: %{y:,.0f}<extra></extra>"
    ) %>%
      add_trace(
        data = serie_plot,
        x = ~semana,
        y = ~media_movel_4s,
        type = "scatter",
        mode = "lines",
        name = "Média móvel (4 semanas)",
        line = list(color = "#ef6c00", width = 2.4),
        hovertemplate = "SE %{x}<br>Média móvel 4s: %{y:,.1f}<extra></extra>"
      ) %>%
      layout(
        separators = ",.",
        hovermode = "x unified",
        legend = list(orientation = "h", x = 0, y = 1.08),
        margin = list(l = 55, r = 20, t = 10, b = 45),
        plot_bgcolor = "rgba(245,245,245,0.5)",
        paper_bgcolor = "white",
        xaxis = list(title = "Semana epidemiológica", range = c(0.5, n_semanas + 0.5), showgrid = TRUE, gridcolor = "#e5e7eb"),
        yaxis = list(title = "Casos", showgrid = TRUE, gridcolor = "#e5e7eb")
      )
  })

  output$titulo_canal_endemico <- renderText({
    paste0("Canal endêmico — ", rotulo_ano_referencia())
  })

  output$grafico_canal_endemico <- renderPlotly({
    canal_df <- canal_endemico()
    anos_hist <- attr(canal_df, "anos_historicos")

    if (nrow(canal_df) == 0 || length(anos_hist) == 0) {
      return(empty_plotly("Sem anos anteriores suficientes para construir o canal endêmico"))
    }

    teto <- max(c(canal_df$q3 * 1.4, canal_df$casos), na.rm = TRUE)
    if (!is.finite(teto) || teto <= 0) {
      teto <- 1
    }
    canal_df <- canal_df %>% mutate(teto = teto, zero = 0)

    faixa <- function(p, ymin, ymax, nome, cor) {
      p %>%
        add_ribbons(
          data = canal_df,
          x = ~semana,
          ymin = ymin,
          ymax = ymax,
          name = nome,
          fillcolor = cor,
          line = list(color = "rgba(0,0,0,0)"),
          hoverinfo = "skip"
        )
    }

    anos_texto <- if (length(anos_hist) > 1) paste0(min(anos_hist), "–", max(anos_hist)) else as.character(anos_hist)
    excluidos <- canal_anos_excluidos[canal_anos_excluidos >= min(anos_hist) & canal_anos_excluidos <= max(anos_hist)]
    nota <- paste0(
      "Quartis de ", length(anos_hist), " anos (", anos_texto, ")",
      if (length(excluidos) > 0) paste0(", sem ", paste(excluidos, collapse = ", ")) else "",
      ". A linha vai até a última semana com dados."
    )

    plot_ly() %>%
      faixa(~q3, ~teto, "Epidêmica (> 3º quartil)", "rgba(211, 47, 47, 0.16)") %>%
      faixa(~mediana, ~q3, "Alerta (mediana a 3º quartil)", "rgba(237, 108, 2, 0.22)") %>%
      faixa(~q1, ~mediana, "Segurança (1º quartil a mediana)", "rgba(251, 192, 45, 0.25)") %>%
      faixa(~zero, ~q1, "Êxito (< 1º quartil)", "rgba(46, 125, 50, 0.20)") %>%
      add_trace(
        data = canal_df,
        x = ~semana,
        y = ~mediana,
        type = "scatter",
        mode = "lines",
        name = "Mediana histórica",
        line = list(color = "#607d8b", dash = "dash", width = 1.4),
        hovertemplate = "Mediana: %{y:,.1f}<extra></extra>"
      ) %>%
      add_trace(
        data = canal_df,
        x = ~semana,
        y = ~casos,
        type = "scatter",
        mode = "lines+markers",
        marker = list(size = 4),
        name = paste0("Casos em ", selected_year()),
        line = list(color = "#1f2933", width = 2.6),
        hovertemplate = "Casos: %{y:,.0f}<extra></extra>"
      ) %>%
      layout(
        separators = ",.",
        hovermode = "x unified",
        legend = list(orientation = "h", x = 0, y = -0.22),
        margin = list(l = 55, r = 20, t = 30, b = 40),
        plot_bgcolor = "white",
        paper_bgcolor = "white",
        xaxis = list(title = "Semana epidemiológica", range = c(1, nrow(canal_df)), showgrid = FALSE),
        yaxis = list(title = "Casos", range = c(0, teto), showgrid = TRUE, gridcolor = "#e5e7eb"),
        annotations = list(
          list(
            x = 0.01,
            y = 1.02,
            xref = "paper",
            yref = "paper",
            xanchor = "left",
            yanchor = "bottom",
            text = nota,
            showarrow = FALSE,
            font = list(size = 11, color = "#52606d")
          )
        )
      )
  })

  output$titulo_grafico_casos_territorio <- renderText({
    titulo <- if (is_municipio_selected()) {
      paste0("Casos por unidade de saúde — ", input$filtro_municipio)
    } else if (!is.null(input$filtro_gve) && input$filtro_gve != "Todas") {
      paste0("Casos por município — GVE ", input$filtro_gve)
    } else {
      "Casos por GVE"
    }
    paste0(titulo, ", ", rotulo_periodo())
  })

  output$grafico_casos_gve <- renderPlotly({
    df_base <- dados_contexto_periodo()
    municipio_especifico <- is_municipio_selected()
    gve_especifico <- !is.null(input$filtro_gve) && input$filtro_gve != "Todas"

    if (municipio_especifico) {
      df <- df_base %>%
        mutate(
          unidade_saude = ifelse(
            is.na(unidade_saude) | trimws(unidade_saude) == "",
            "Não informado",
            unidade_saude
          )
        ) %>%
        group_by(unidade_saude) %>%
        summarise(
          casos = sum(casos, na.rm = TRUE),
          surtos = sum(surtos, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(categoria = unidade_saude) %>%
        arrange(desc(casos), desc(surtos))

      msg_empty <- "Sem dados por unidade de saúde para os filtros selecionados"
      x_title <- "Casos"
      y_title <- "Unidade de saúde"
      trace_name <- "Casos por unidade"
      plot_horizontal <- TRUE
    } else if (gve_especifico) {
      df <- df_base %>%
        group_by(municipio) %>%
        summarise(casos = sum(casos, na.rm = TRUE), .groups = "drop") %>%
        mutate(categoria = municipio) %>%
        arrange(desc(casos))

      msg_empty <- "Sem dados por município para os filtros selecionados"
      x_title <- "Município"
      y_title <- "Casos"
      trace_name <- "Casos por município"
      plot_horizontal <- FALSE
    } else {
      df <- df_base %>%
        group_by(gve) %>%
        summarise(casos = sum(casos, na.rm = TRUE), .groups = "drop") %>%
        mutate(categoria = gve) %>%
        arrange(desc(casos))

      msg_empty <- "Sem dados por GVE para os filtros selecionados"
      x_title <- "GVE"
      y_title <- "Casos"
      trace_name <- "Casos por GVE"
      plot_horizontal <- FALSE
    }

    if (nrow(df) == 0) {
      return(empty_plotly(msg_empty))
    }

    if (plot_horizontal) {
      top_n <- min(20, nrow(df))
      df_plot <- utils::head(df, top_n)

      return(
        plot_ly(
          df_plot,
          x = ~casos,
          y = ~reorder(categoria, casos),
          type = "bar",
          orientation = "h",
          marker = list(color = "#005ca9", opacity = 0.9),
          name = trace_name,
          hovertemplate = "Unidade: %{y}<br>Casos: %{x}<br>Surtos: %{customdata}<extra></extra>",
          customdata = ~surtos
        ) %>%
          layout(
            showlegend = FALSE,
            margin = list(l = 230, r = 20, t = 10, b = 45),
            plot_bgcolor = "rgba(245,245,245,0.5)",
            paper_bgcolor = "white",
            xaxis = list(title = x_title, showgrid = TRUE, gridcolor = "#e5e7eb"),
            yaxis = list(title = y_title, showgrid = FALSE)
          )
      )
    }

    plot_ly(
      df,
      x = ~reorder(categoria, -casos),
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
        yaxis = list(title = y_title, showgrid = TRUE, gridcolor = "#e5e7eb")
      )
  })

  rotulo_janela_4s <- reactive({
    janela <- janela_4_semanas()
    paste0("SE ", janela$inicio, " a ", janela$fim, "/", janela$ano)
  })

  output$titulo_tabela_operacional <- renderText({
    if (is_municipio_selected()) {
      paste0("Casos por unidade de saúde — ", input$filtro_municipio, ", ", rotulo_periodo())
    } else {
      paste0("Casos por município — ", rotulo_periodo())
    }
  })

  output$legenda_tabela_operacional <- renderUI({
    janela <- rotulo_janela_4s()
    if (is_municipio_selected()) {
      return(
        tags$small(
          class = "legend-note d-block mt-2",
          HTML(paste0(
            "<strong>Legenda:</strong> \"Últimas 4 semanas\" = ", janela, ". ",
            "Vermelho = surto notificado ou unidade entre as 25% com mais casos nas últimas 4 semanas. ",
            "As colunas de % mostram a participação da unidade no total do município."
          ))
        )
      )
    }

    incid_periodo <- if (is_all_years_selected()) "média anual" else "acumulada no ano"
    tags$small(
      class = "legend-note d-block mt-2",
      HTML(paste0(
        "<strong>Legenda:</strong> incidência por 100 mil hab. com população IBGE (", incid_periodo, "). ",
        "\"Últimas 4 semanas\" = ", janela, ". ",
        "Vermelho = surto notificado ou município entre os 25% com maior incidência nas últimas 4 semanas."
      ))
    )
  })

  output$baixar_tabela <- downloadHandler(
    filename = function() {
      paste0("conjuntivite_", if (is_municipio_selected()) "unidades" else "municipios", "_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      tabela <- tabela_operacional_municipios()
      tabela <- if (is_municipio_selected()) {
        tabela %>%
          transmute(
            GVE = gve, `Município` = municipio, `Unidade de saúde` = unidade_saude, `Período` = ano,
            `Casos no período` = total_ano, `% no período` = round(part_ano_pct, 1),
            `Casos últimas 4 sem.` = total_4s, `% últimas 4 sem.` = round(part_4s_pct, 1),
            Surtos = surtos, `Situação` = alerta
          )
      } else {
        tabela %>%
          transmute(
            GVE = gve, `Município` = municipio, `Código IBGE` = cod_ibge, `Período` = ano,
            `População (IBGE)` = populacao, `Casos no período` = total_ano,
            `Incidência no período` = round(incidencia_anual, 2),
            `Casos últimas 4 sem.` = total_4s, `Incidência últimas 4 sem.` = round(incidencia_4s, 2),
            Surtos = surtos, `Situação` = alerta
          )
      }
      # CSV com ; e BOM UTF-8 abre direto no Excel em português.
      linhas <- utils::capture.output(utils::write.table(
        tabela, sep = ";", dec = ",", row.names = FALSE, na = "", qmethod = "double"
      ))
      texto <- enc2utf8(paste0("\ufeff", paste(linhas, collapse = "\r\n"), "\r\n"))
      writeBin(charToRaw(texto), file)
    }
  )

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

    n_semanas <- nrow(serie)
    semana_limite <- min(semana_final_ano(selected_year()), n_semanas)

    observado <- serie %>%
      filter(semana <= semana_limite) %>%
      mutate(casos = tidyr::replace_na(casos, 0))

    if (nrow(observado) >= 8) {
      ajuste_modelo <- tryCatch(
        suppressWarnings(stats::loess(casos ~ semana, data = observado, span = 0.35, degree = 1, family = "symmetric")),
        error = function(e) NULL
      )
      if (!is.null(ajuste_modelo)) {
        observado$tendencia <- pmax(as.numeric(suppressWarnings(stats::predict(ajuste_modelo, newdata = observado))), 0)
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

    max_semana_previsao <- min(n_semanas, semana_limite + 8)
    semanas_previsao <- if (semana_limite < n_semanas) {
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

  motor_alerta_ia <- reactive({
    ano_ref <- selected_year()
    serie_atual <- serie_semanal_atual() %>% arrange(semana)
    tendencia <- modelo_tendencia()

    n_semanas <- nrow(serie_atual)
    anos_hist <- attr(canal_endemico(), "anos_historicos")
    historico <- dados_filtrados_dimensoes() %>%
      filter(ano %in% anos_hist) %>%
      group_by(ano, semana) %>%
      summarise(casos = sum(casos, na.rm = TRUE), .groups = "drop")
    if (length(anos_hist) > 0) {
      historico <- tidyr::expand_grid(ano = anos_hist, semana = seq_len(n_semanas)) %>%
        left_join(historico, by = c("ano", "semana")) %>%
        mutate(casos = tidyr::replace_na(casos, 0))
    }

    historico_stats <- if (nrow(historico) == 0) {
      media_ref <- mean(serie_atual$casos, na.rm = TRUE)
      dp_ref <- stats::sd(serie_atual$casos, na.rm = TRUE)
      if (!is.finite(media_ref)) media_ref <- 0
      if (!is.finite(dp_ref)) dp_ref <- 0
      tibble::tibble(
        semana = seq_len(n_semanas),
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

    observado <- tibble::tibble(semana = seq_len(n_semanas)) %>%
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
            "Índice de risco ", ia$nivel, ": ",
            ia$mensagem,
            " Índice atual: ",
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

  output$grafico_alerta_ia <- renderPlotly({
    motor <- motor_alerta_ia()
    obs <- motor$observado %>% filter(semana_fechada)
    fut <- motor$futuro

    if (nrow(obs) == 0) {
      return(empty_plotly("Sem dados para calcular o índice de risco"))
    }

    p <- plot_ly() %>%
      add_trace(
        data = obs,
        x = ~semana,
        y = ~prob_surto,
        type = "scatter",
        mode = "lines",
        name = "Índice observado",
        line = list(color = "#6a1b9a", width = 2.4)
      ) %>%
      add_trace(
        x = c(min(obs$semana), max(c(obs$semana, fut$semana))),
        y = c(0.70, 0.70),
        type = "scatter",
        mode = "lines",
        name = "Limiar de alerta (70%)",
        line = list(color = "#d32f2f", dash = "dash", width = 1.7)
      )

    if (nrow(fut) > 0) {
      p <- p %>%
        add_trace(
          data = fut,
          x = ~semana,
          y = ~prob_surto,
          type = "scatter",
          mode = "lines",
          name = "Índice projetado",
          line = list(color = "#ef6c00", dash = "dot", width = 2.2)
        )
    }

    p %>%
      layout(
        hovermode = "x unified",
        legend = list(orientation = "h", x = 0, y = 1.10),
        margin = list(l = 55, r = 20, t = 10, b = 45),
        plot_bgcolor = "rgba(245,245,245,0.5)",
        paper_bgcolor = "white",
        xaxis = list(title = "Semana epidemiológica", showgrid = TRUE, gridcolor = "#e5e7eb"),
        yaxis = list(
          title = "Índice de risco (0 a 100%)",
          tickformat = ".0%",
          range = c(0, 1),
          showgrid = TRUE,
          gridcolor = "#e5e7eb"
        )
      )
  })

  # Sexo, faixa etária e ações só são registrados na base a partir de um certo ano.
  # Os gráficos mostram só os casos com o campo preenchido e dizem a completude.
  mensagem_sem_registro <- function(campo, primeiro_ano) {
    if (is.na(primeiro_ano)) {
      return(paste0("A base não tem registro de ", campo, "."))
    }
    paste0("Sem registro de ", campo, " no período.<br>O campo existe na base a partir de ", primeiro_ano, ".")
  }

  nota_completude <- function(informados, total) {
    if (!is.finite(total) || total <= 0) {
      return("")
    }
    pct <- informados / total * 100
    paste0(
      "Informado em ", format(round(pct, 1), decimal.mark = ","), "% dos casos (",
      format(informados, big.mark = ".", decimal.mark = ","), " de ", format(total, big.mark = ".", decimal.mark = ","), ")"
    )
  }

  anotacao_topo <- function(texto) {
    list(list(
      x = 0.01, y = 1.02, xref = "paper", yref = "paper",
      xanchor = "left", yanchor = "bottom",
      text = texto, showarrow = FALSE,
      font = list(size = 11, color = "#52606d")
    ))
  }

  output$grafico_sexo <- renderPlotly({
    df <- dados_contexto_periodo()
    total_masc <- sum(df$sexo_masc, na.rm = TRUE)
    total_fem <- sum(df$sexo_fem, na.rm = TRUE)
    total_casos <- sum(df$casos, na.rm = TRUE)

    if (total_masc + total_fem <= 0) {
      return(empty_plotly(mensagem_sem_registro("sexo", primeiro_ano_detalhes()$sexo)))
    }

    sexo_df <- tibble::tibble(
      sexo = c("Masculino", "Feminino"),
      casos = c(total_masc, total_fem)
    )

    plot_ly(
      sexo_df,
      labels = ~sexo,
      values = ~casos,
      type = "pie",
      hole = 0.45,
      sort = FALSE,
      marker = list(colors = c("#1976d2", "#ec407a"))
    ) %>%
      layout(
        separators = ",.",
        margin = list(l = 20, r = 20, t = 30, b = 10),
        showlegend = TRUE,
        legend = list(orientation = "h", x = 0, y = -0.05),
        paper_bgcolor = "white",
        annotations = anotacao_topo(nota_completude(total_masc + total_fem, total_casos))
      )
  })

  output$grafico_faixa_etaria <- renderPlotly({
    df <- dados_contexto_periodo()
    faixas <- c("< 1 ano", "1 a 4", "5 a 9", "10 a 14", "15+")
    valores <- c(
      sum(df$fx_menor1, na.rm = TRUE),
      sum(df$fx_1a4, na.rm = TRUE),
      sum(df$fx_5a9, na.rm = TRUE),
      sum(df$fx_10a14, na.rm = TRUE),
      sum(df$fx_15mais, na.rm = TRUE)
    )

    if (sum(valores) <= 0) {
      return(empty_plotly(mensagem_sem_registro("faixa etária", primeiro_ano_detalhes()$faixa)))
    }

    faixa_df <- tibble::tibble(faixa = factor(faixas, levels = faixas), casos = valores)

    plot_ly(
      faixa_df,
      x = ~faixa,
      y = ~casos,
      type = "bar",
      marker = list(color = "#1e88e5"),
      name = "Casos",
      hovertemplate = "%{x}: %{y:,.0f} casos<extra></extra>"
    ) %>%
      layout(
        separators = ",.",
        showlegend = FALSE,
        margin = list(l = 55, r = 20, t = 30, b = 45),
        plot_bgcolor = "rgba(245,245,245,0.5)",
        paper_bgcolor = "white",
        xaxis = list(title = "Faixa etária (anos)", showgrid = FALSE),
        yaxis = list(title = "Casos", showgrid = TRUE, gridcolor = "#e5e7eb"),
        annotations = anotacao_topo(nota_completude(sum(valores), sum(df$casos, na.rm = TRUE)))
      )
  })

  output$grafico_acoes_treinamentos <- renderPlotly({
    df <- dados_contexto_periodo()

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
      return(empty_plotly(mensagem_sem_registro("ações", primeiro_ano_detalhes()$acoes)))
    }

    plot_ly(
      acoes_df,
      x = ~reorder(acao, -quantidade),
      y = ~quantidade,
      type = "bar",
      marker = list(color = "#00897b"),
      name = "Quantidade",
      hovertemplate = "%{x}: %{y:,.0f}<extra></extra>"
    ) %>%
      layout(
        separators = ",.",
        showlegend = FALSE,
        margin = list(l = 55, r = 20, t = 10, b = 90),
        plot_bgcolor = "rgba(245,245,245,0.5)",
        paper_bgcolor = "white",
        xaxis = list(title = "", tickangle = 20, showgrid = FALSE),
        yaxis = list(title = "Quantidade", showgrid = TRUE, gridcolor = "#e5e7eb")
      )
  })

  # Faixas do mapa por quintis da incidência das áreas exibidas: a escala se ajusta
  # ao período (semanas, ano ou média anual) em vez de usar cortes fixos.
  faixas_incidencia <- function(incidencia) {
    valores <- incidencia[is.finite(incidencia)]
    if (length(valores) < 5) {
      cortes <- c(100, 300, 600, 1000)
    } else {
      cortes <- unique(signif(stats::quantile(valores, c(0.2, 0.4, 0.6, 0.8), names = FALSE), 2))
      cortes <- cortes[cortes > 0]
      if (length(cortes) == 0) cortes <- max(valores)
    }
    fmt <- function(x) {
      vapply(x, function(v) format(v, big.mark = ".", decimal.mark = ",", scientific = FALSE, trim = TRUE), character(1))
    }
    rotulos <- c(
      paste0("< ", fmt(cortes[1])),
      if (length(cortes) > 1) paste0(fmt(utils::head(cortes, -1)), " a < ", fmt(cortes[-1])),
      paste0("\u2265 ", fmt(cortes[length(cortes)]))
    )
    list(cortes = cortes, rotulos = rotulos)
  }

  classificar_faixa <- function(incidencia, faixas) {
    classe <- findInterval(incidencia, faixas$cortes) + 1
    ifelse(is.finite(incidencia), faixas$rotulos[classe], "Sem população")
  }

  nivel_mapa_atual <- reactive({
    nivel <- input$nivel_mapa
    if (is.null(nivel) || !(nivel %in% c("municipio", "gve", "drs"))) {
      return("municipio")
    }
    nivel
  })

  # População de cada município somada nos anos do período (pessoas-ano).
  # Com "Todos", a incidência vira média anual.
  populacao_municipios <- function(anos) {
    pop <- load_populacao_ibge()
    anos_ref <- pmin(pmax(as.integer(anos), min(pop$ano)), max(pop$ano))
    tibble::tibble(ano = anos_ref) %>%
      inner_join(pop %>% select(cod_ibge, ano, populacao), by = "ano", relationship = "many-to-many") %>%
      group_by(cod_ibge) %>%
      summarise(populacao = sum(populacao, na.rm = TRUE), .groups = "drop")
  }

  # Janela das "últimas 4 semanas": as 4 semanas mais recentes do ano de referência.
  janela_4_semanas <- reactive({
    ano_ref <- selected_year()
    semana_fim <- semana_final_ano(ano_ref)
    list(ano = ano_ref, inicio = max(1L, as.integer(semana_fim) - 3L), fim = as.integer(semana_fim))
  })

  resumo_municipios <- reactive({
    df <- dados_contexto_periodo()

    if (nrow(df) == 0) {
      return(
        tibble::tibble(
          municipio = character(),
          casos = numeric(),
          surtos = numeric(),
          populacao = numeric(),
          incidencia = numeric(),
          status_surto = character(),
        )
      )
    }

    df %>%
      group_by(municipio, cod_ibge) %>%
      summarise(
        casos = sum(casos, na.rm = TRUE),
        surtos = sum(surtos, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      left_join(populacao_municipios(anos_periodo()), by = "cod_ibge") %>%
      mutate(
        incidencia = ifelse(populacao > 0, (casos / populacao) * 100000, NA_real_),
        status_surto = ifelse(surtos > 0, "Em surto", "Sem surto")
      ) %>%
      arrange(desc(incidencia), desc(casos), municipio)
  })

  tabela_operacional_municipios <- reactive({
    df <- dados_contexto_periodo()
    todos_anos <- is_all_years_selected()
    municipio_especifico <- is_municipio_selected()

    if (nrow(df) == 0) {
      if (municipio_especifico) {
        return(
          tibble::tibble(
            gve = character(),
            municipio = character(),
            unidade_saude = character(),
            ano = character(),
            total_ano = numeric(),
            part_ano_pct = numeric(),
            total_4s = numeric(),
            part_4s_pct = numeric(),
            surtos = numeric(),
            alerta = character(),
            alerta_flag = integer()
          )
        )
      }

      return(
        tibble::tibble(
          gve = character(),
          municipio = character(),
          ano = character(),
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

    janela <- janela_4_semanas()
    df_4s <- dados_filtrados_dimensoes() %>%
      filter(ano == janela$ano, semana >= janela$inicio, semana <= janela$fim)
    rotulo_ano <- if (todos_anos) "Todos" else as.character(selected_year())

    if (municipio_especifico) {
      resumo_ano <- df %>%
        mutate(
          unidade_saude = dplyr::coalesce(dplyr::na_if(trimws(as.character(unidade_saude)), ""), "Não informado"),
          ano = rotulo_ano
        ) %>%
        group_by(gve, municipio, unidade_saude, ano) %>%
        summarise(
          total_ano = sum(casos, na.rm = TRUE),
          surtos = sum(surtos, na.rm = TRUE),
          .groups = "drop"
        )

      resumo_4s <- df_4s %>%
        mutate(
          unidade_saude = dplyr::coalesce(dplyr::na_if(trimws(as.character(unidade_saude)), ""), "Não informado"),
          ano = rotulo_ano
        ) %>%
        group_by(gve, municipio, unidade_saude, ano) %>%
        summarise(
          total_4s = sum(casos, na.rm = TRUE),
          .groups = "drop"
        )

      resumo <- resumo_ano %>%
        left_join(resumo_4s, by = c("gve", "municipio", "unidade_saude", "ano")) %>%
        mutate(total_4s = tidyr::replace_na(total_4s, 0)) %>%
        group_by(ano) %>%
        mutate(
          part_ano_pct = if (sum(total_ano, na.rm = TRUE) > 0) total_ano / sum(total_ano, na.rm = TRUE) * 100 else 0,
          part_4s_pct = if (sum(total_4s, na.rm = TRUE) > 0) total_4s / sum(total_4s, na.rm = TRUE) * 100 else 0
        ) %>%
        ungroup()

      limiar_alerta_unidade <- suppressWarnings(stats::quantile(
        resumo$total_4s[is.finite(resumo$total_4s) & resumo$total_4s > 0],
        probs = 0.75,
        na.rm = TRUE
      ))

      if (!is.finite(limiar_alerta_unidade) || limiar_alerta_unidade <= 0) {
        limiar_alerta_unidade <- Inf
      }

      return(
        resumo %>%
          mutate(
            alerta_flag = ifelse(surtos > 0 | total_4s >= limiar_alerta_unidade, 1L, 0L),
            alerta = ifelse(alerta_flag == 1L, "Alerta", "Estável")
          ) %>%
          arrange(desc(alerta_flag), desc(total_4s), desc(total_ano), unidade_saude)
      )
    }

    resumo_ano <- df %>%
      mutate(ano = rotulo_ano) %>%
      group_by(gve, municipio, cod_ibge, ano) %>%
      summarise(
        total_ano = sum(casos, na.rm = TRUE),
        surtos = sum(surtos, na.rm = TRUE),
        .groups = "drop"
      )

    resumo_4s <- df_4s %>%
      group_by(gve, municipio, cod_ibge) %>%
      summarise(
        total_4s = sum(casos, na.rm = TRUE),
        .groups = "drop"
      )

    pop_periodo <- populacao_municipios(anos_periodo()) %>% rename(pop_periodo = populacao)
    pop_ref <- populacao_municipios(janela$ano)

    resumo <- resumo_ano %>%
      left_join(resumo_4s, by = c("gve", "municipio", "cod_ibge")) %>%
      left_join(pop_periodo, by = "cod_ibge") %>%
      left_join(pop_ref, by = "cod_ibge") %>%
      mutate(
        total_4s = tidyr::replace_na(total_4s, 0),
        incidencia_anual = ifelse(pop_periodo > 0, (total_ano / pop_periodo) * 100000, NA_real_),
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
        alerta = ifelse(alerta_flag == 1L, "Alerta", "Estável")
      ) %>%
      arrange(desc(alerta_flag), desc(incidencia_4s), desc(total_4s), municipio)
  })

  dt_idioma <- list(
    search = "Buscar:",
    info = "_START_ a _END_ de _TOTAL_ linhas",
    infoEmpty = "Nenhuma linha",
    infoFiltered = "(filtrado de _MAX_)",
    zeroRecords = "Nenhum registro encontrado",
    paginate = list(previous = "Anterior", `next` = "Próxima")
  )

  output$tabela_municipios <- DT::renderDT({
    tabela_base <- tabela_operacional_municipios()
    municipio_especifico <- is_municipio_selected()

    if (municipio_especifico) {
      tabela <- tabela_base %>%
        transmute(
          GVE = gve,
          `Município` = municipio,
          `Unidade de saúde` = unidade_saude,
          `Período` = as.character(ano),
          `Casos no período` = total_ano,
          `% no período` = round(part_ano_pct, 1),
          `Casos últimas 4 sem.` = total_4s,
          `% últimas 4 sem.` = round(part_4s_pct, 1),
          Surtos = surtos,
          `Situação` = alerta,
          alerta_flag = alerta_flag
        )

      return(
        DT::datatable(
          tabela,
          rownames = FALSE,
          class = "compact stripe hover",
          options = list(
            pageLength = 12,
            lengthMenu = c(12, 25, 50, 100),
            autoWidth = TRUE,
            scrollX = TRUE,
            order = list(list(10, "desc"), list(6, "desc"), list(4, "desc")),
            dom = "ftip",
            language = dt_idioma,
            columnDefs = list(
              list(visible = FALSE, targets = 10)
            )
          )
        ) %>%
          DT::formatCurrency(
            columns = c("Casos no período", "Casos últimas 4 sem.", "Surtos"),
            currency = "",
            interval = 3,
            mark = ".",
            dec.mark = ",",
            digits = 0
          ) %>%
          DT::formatRound(columns = c("% no período", "% últimas 4 sem."), digits = 1, dec.mark = ",") %>%
          DT::formatStyle(
            "% últimas 4 sem.",
            valueColumns = "alerta_flag",
            backgroundColor = DT::styleEqual(c(1, 0), c("#f8d7da", "#d4edda")),
            color = DT::styleEqual(c(1, 0), c("#7f1d1d", "#14532d")),
            fontWeight = "700"
          ) %>%
          DT::formatStyle(
            "Situação",
            valueColumns = "alerta_flag",
            backgroundColor = DT::styleEqual(c(1, 0), c("#f8d7da", "#d4edda")),
            color = DT::styleEqual(c(1, 0), c("#7f1d1d", "#14532d")),
            fontWeight = "700"
          )
      )
    }

    tabela <- tabela_base %>%
      transmute(
        GVE = gve,
        `Município` = municipio,
        `Período` = as.character(ano),
        `População (IBGE)` = populacao,
        `Casos no período` = total_ano,
        `Incidência no período` = round(incidencia_anual, 2),
        `Casos últimas 4 sem.` = total_4s,
        `Incidência últimas 4 sem.` = round(incidencia_4s, 2),
        `Situação` = alerta,
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
        scrollX = TRUE,
        order = list(list(9, "desc"), list(7, "desc"), list(6, "desc")),
        dom = "ftip",
        language = dt_idioma,
        columnDefs = list(
          list(visible = FALSE, targets = 9)
        )
      )
    ) %>%
      DT::formatCurrency(
        columns = c("População (IBGE)", "Casos no período", "Casos últimas 4 sem."),
        currency = "",
        interval = 3,
        mark = ".",
        dec.mark = ",",
        digits = 0
      ) %>%
      DT::formatCurrency(
        columns = c("Incidência no período", "Incidência últimas 4 sem."),
        currency = "",
        interval = 3,
        mark = ".",
        dec.mark = ",",
        digits = 2
      ) %>%
      DT::formatStyle(
        "Incidência últimas 4 sem.",
        valueColumns = "alerta_flag",
        backgroundColor = DT::styleEqual(c(1, 0), c("#f8d7da", "#d4edda")),
        color = DT::styleEqual(c(1, 0), c("#7f1d1d", "#14532d")),
        fontWeight = "700"
      ) %>%
      DT::formatStyle(
        "Situação",
        valueColumns = "alerta_flag",
        backgroundColor = DT::styleEqual(c(1, 0), c("#f8d7da", "#d4edda")),
        color = DT::styleEqual(c(1, 0), c("#7f1d1d", "#14532d")),
        fontWeight = "700"
      )
  }, server = FALSE)

  # Indicadores por área do mapa. Em GVE e DRS, a população é a soma dos municípios
  # da área que notificam nesta base.
  dados_mapa_nivel <- reactive({
    df <- dados_contexto_periodo()
    nivel <- nivel_mapa_atual()

    vazio <- tibble::tibble(
      rotulo = character(), map_key = character(), casos = numeric(), surtos = numeric(),
      populacao = numeric(), incidencia = numeric(), status_surto = character()
    )
    if (nrow(df) == 0) {
      return(vazio)
    }

    if (nivel == "municipio") {
      return(
        resumo_municipios() %>%
          filter(!is.na(cod_ibge)) %>%
          transmute(rotulo = municipio, map_key = cod_ibge, casos, surtos, populacao, incidencia, status_surto)
      )
    }

    coluna <- if (nivel == "gve") "gve" else "drs"
    rede <- dados_filtrados_dimensoes() %>%
      filter(!is.na(cod_ibge)) %>%
      distinct(area = .data[[coluna]], cod_ibge) %>%
      left_join(populacao_municipios(anos_periodo()), by = "cod_ibge") %>%
      group_by(area) %>%
      summarise(populacao = sum(populacao, na.rm = TRUE), .groups = "drop")

    df %>%
      group_by(area = .data[[coluna]]) %>%
      summarise(casos = sum(casos, na.rm = TRUE), surtos = sum(surtos, na.rm = TRUE), .groups = "drop") %>%
      left_join(rede, by = "area") %>%
      mutate(
        populacao = ifelse(populacao > 0, populacao, NA_real_),
        incidencia = ifelse(populacao > 0, (casos / populacao) * 100000, NA_real_),
        status_surto = ifelse(surtos > 0, "Em surto", "Sem surto"),
        rotulo = area,
        map_key = normalize_shape_key(area, level = nivel)
      ) %>%
      select(rotulo, map_key, casos, surtos, populacao, incidencia, status_surto)
  })

  output$fonte_geografica <- renderText({
    shape <- get_local_shape_layer(nivel_mapa_atual())
    if (is.null(shape)) {
      return("Shapefile não encontrado")
    }
    paste0("Malha: ", basename(attr(shape, "shape_path")))
  })

  cores_faixas <- c("#dbeafe", "#93c5fd", "#3b82f6", "#1d4ed8", "#1e3a8a")

  # O mapa base é desenhado uma vez; filtros só trocam as camadas (mantém zoom e posição).
  output$mapa <- renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 5)) %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = -48.6, lat = -22.3, zoom = 6)
  })

  observe({
    nivel <- nivel_mapa_atual()
    df_mapa <- dados_mapa_nivel()
    shape <- get_local_shape_layer(nivel)
    proxy <- leafletProxy("mapa") %>%
      clearShapes() %>%
      clearControls()

    if (is.null(shape) || nrow(shape) == 0) {
      return(invisible(NULL))
    }

    shape_key <- if (nivel == "municipio") as.character(shape$cod_ibge) else as.character(shape$map_key)
    idx <- match(shape_key, df_mapa$map_key)

    # Com filtro de território, áreas fora dele ficam apagadas em vez de "sem notificação".
    filtro_territorio <- any(c(
      !is.null(input$filtro_drs) && input$filtro_drs != "Todas",
      !is.null(input$filtro_gve) && input$filtro_gve != "Todas",
      is_municipio_selected()
    ))
    territorio <- dados_filtrados_dimensoes()
    chaves_territorio <- if (nivel == "municipio") {
      unique(territorio$cod_ibge)
    } else {
      unique(normalize_shape_key(territorio[[if (nivel == "gve") "gve" else "drs"]], level = nivel))
    }
    fora_filtro <- is.na(idx) & filtro_territorio & !(shape_key %in% chaves_territorio)

    shape$rotulo <- dplyr::coalesce(df_mapa$rotulo[idx], as.character(shape$map_label))
    shape$casos <- df_mapa$casos[idx]
    shape$incidencia <- df_mapa$incidencia[idx]
    shape$status_surto <- df_mapa$status_surto[idx]
    faixas <- faixas_incidencia(df_mapa$incidencia)
    shape$faixa <- ifelse(is.na(idx), "Sem notificação", classificar_faixa(shape$incidencia, faixas))
    niveis <- c(faixas$rotulos, "Sem notificação", "Sem população")
    paleta <- colorFactor(
      palette = c(cores_faixas[round(seq(1, length(cores_faixas), length.out = length(faixas$rotulos)))], "#e5e7eb", "#fde68a"),
      levels = niveis,
      ordered = TRUE,
      na.color = "#e5e7eb"
    )
    shape$layer_id <- paste0(nivel, "::", shape_key)

    formatar_num <- function(x, digitos = 0) {
      ifelse(is.finite(x), format(round(x, digitos), big.mark = ".", decimal.mark = ",", nsmall = digitos), "—")
    }
    shape$opacidade <- ifelse(fora_filtro, 0.15, 0.85)
    shape$popup <- ifelse(
      is.na(idx),
      paste0(
        "<strong>", htmltools::htmlEscape(shape$rotulo), "</strong><br>",
        ifelse(fora_filtro, "Fora do filtro selecionado", "Sem notificação no período")
      ),
      paste0(
        "<strong>", htmltools::htmlEscape(shape$rotulo), "</strong><br>",
        "Casos: ", formatar_num(shape$casos), "<br>",
        "Incidência: ", formatar_num(shape$incidencia, 1), " por 100 mil hab.<br>",
        "Faixa: ", shape$faixa, "<br>",
        "Surto: ", shape$status_surto
      )
    )

    titulo_nivel <- switch(nivel, municipio = "município", gve = "GVE", drs = "DRS")
    proxy %>%
      addPolygons(
        data = shape,
        fillColor = ~paleta(faixa),
        color = "#9ca3af",
        weight = if (nivel == "municipio") 0.5 else 1.2,
        fillOpacity = ~opacidade,
        smoothFactor = 0.3,
        layerId = ~layer_id,
        label = ~rotulo,
        popup = ~popup,
        highlightOptions = highlightOptions(weight = 2.5, color = "#005ca9", bringToFront = TRUE)
      ) %>%
      addLegend(
        position = "bottomright",
        colors = paleta(niveis),
        labels = niveis,
        title = paste0("Incidência por ", titulo_nivel, "<br>(por 100 mil hab.)"),
        opacity = 0.9
      )
  })

  # Clique no mapa aplica o filtro da área clicada (município, GVE ou DRS).
  selecionar_area_no_filtro <- function(clicked_id) {
    if (is.null(clicked_id) || !grepl("::", clicked_id, fixed = TRUE)) {
      return(invisible(NULL))
    }
    partes <- strsplit(clicked_id, "::", fixed = TRUE)[[1]]
    nivel <- partes[1]
    chave <- partes[2]
    base <- dados_base()

    if (nivel == "municipio") {
      linha <- base %>% filter(cod_ibge == chave) %>% slice_head(n = 1)
      if (nrow(linha) == 0) {
        showNotification("Município sem notificações nesta base.", type = "message", duration = 3)
        return(invisible(NULL))
      }
      updateSelectInput(session, "filtro_drs", selected = linha$drs)
      updateSelectInput(session, "filtro_gve", choices = c("Todas", clean_choice_values(base$gve[base$drs == linha$drs])), selected = linha$gve)
      updateSelectInput(session, "filtro_municipio", choices = c("Todos", clean_choice_values(base$municipio[base$gve == linha$gve])), selected = linha$municipio)
      return(invisible(NULL))
    }

    coluna <- if (nivel == "gve") "gve" else "drs"
    valores <- unique(base[[coluna]])
    valor <- valores[normalize_shape_key(valores, level = nivel) == chave][1]
    if (is.na(valor)) {
      return(invisible(NULL))
    }
    if (nivel == "gve") {
      drs_valor <- base$drs[base$gve == valor][1]
      updateSelectInput(session, "filtro_drs", selected = drs_valor)
      updateSelectInput(session, "filtro_gve", choices = c("Todas", clean_choice_values(base$gve[base$drs == drs_valor])), selected = valor)
    } else {
      updateSelectInput(session, "filtro_drs", selected = valor)
      updateSelectInput(session, "filtro_gve", selected = "Todas")
    }
    updateSelectInput(session, "filtro_municipio", selected = "Todos")
  }

  observeEvent(input$mapa_shape_click, {
    selecionar_area_no_filtro(input$mapa_shape_click$id)
  }, ignoreInit = TRUE)
}
