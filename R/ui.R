custom_theme <- bs_theme(
  version = 5,
  preset = "bootstrap",
  primary = "#0066cc",
  secondary = "#6c757d",
  success = "#4caf50",
  danger = "#f44336",
  warning = "#ff9800",
  info = "#2196f3",
  light = "#f8f9fa",
  dark = "#212529",
  base_font = font_collection(
    font_google("Inter"),
    font_google("Segoe UI"),
    "sans-serif"
  )
)

ui <- page_navbar(
  theme = custom_theme,
  title = div(
    class = "d-flex align-items-center gap-2",
    div(style = "width: 40px; height: 40px; background: linear-gradient(135deg, #0066cc 0%, #004d99 100%); border-radius: 8px; display: flex; align-items: center; justify-content: center;",
        span("🔬", style = "font-size: 20px;")),
    div(
      h4("Vigilância Epidemiológica", class = "mb-0", style = "font-weight: 600;"),
      small("Conjuntivites", class = "text-muted")
    )
  ),
  nav_panel(
    "Painel Principal",

    layout_sidebar(
      sidebar = sidebar(
        title = "Filtros",
        open = "always",
        class = "bg-light",

        input_dark := selectInput(
          "filtro_ano",
          label = "Ano",
          choices = c(2024, 2023, 2022),
          selected = 2024
        ),

        selectInput(
          "filtro_drs",
          label = "Divisão Regional de Saúde (DRS)",
          choices = c("Todas", sort(drs_list))
        ),

        selectInput(
          "filtro_gve",
          label = "Grupo de Vigilância Epidemiológica (GVE)",
          choices = "Todas"
        ),

        selectInput(
          "filtro_municipio",
          label = "Município",
          choices = "Todos"
        ),

        hr(class = "my-3"),

        div(
          class = "d-grid gap-2",
          actionButton(
            "btn_reset",
            "Limpar Filtros",
            class = "btn-outline-secondary btn-sm",
            icon = icon("redo")
          )
        )
      ),

      main = navset_card_tab(
        title = NULL,

        nav_panel(
          "Indicadores",

          layout_columns(
            col_widths = c(6, 6, 6, 6),

            value_box(
              title = "Total de Casos",
              value = textOutput("total_casos"),
              theme = "primary",
              class = "h-100",
              showcase = icon("chart-bar")
            ),

            value_box(
              title = "Incidência",
              value = textOutput("incidencia_media"),
              subtitle = "Por 100 mil habitantes",
              theme = "info",
              class = "h-100",
              showcase = icon("percent")
            ),

            value_box(
              title = "Número de Surtos",
              value = textOutput("num_surtos"),
              subtitle = "Semanas epidêmicas críticas",
              theme = "warning",
              class = "h-100",
              showcase = icon("triangle-exclamation")
            ),

            value_box(
              title = "Situação Epidemiológica",
              value = textOutput("situacao_epidemiologica"),
              subtitle = textOutput("situacao_descricao"),
              theme = "danger",
              class = "h-100",
              showcase = icon("circle-exclamation")
            )
          ),

          br(),

          layout_columns(
            col_widths = c(12, 12),

            card(
              card_header("Série Temporal - Casos por Semana Epidemiológica"),
              plotlyOutput("grafico_serie_temporal", height = "400px"),
              full_screen = TRUE
            ),

            card(
              card_header("Canal Endêmico"),
              plotlyOutput("grafico_canal_endemico", height = "400px"),
              full_screen = TRUE
            )
          ),

          br(),

          layout_columns(
            col_widths = c(6, 6),

            card(
              card_header("Diagrama de Controle"),
              plotlyOutput("grafico_diagrama_controle", height = "350px"),
              full_screen = TRUE
            ),

            card(
              card_header("Casos por GVE"),
              plotlyOutput("grafico_casos_gve", height = "350px"),
              full_screen = TRUE
            )
          )
        ),

        nav_panel(
          "Mapa",

          card(
            card_header("Distribuição Geográfica de Casos"),
            leafletOutput("mapa", height = "700px"),
            full_screen = TRUE,
            class = "overflow-hidden"
          )
        ),

        nav_panel(
          "Sobre",

          layout_columns(
            col_widths = 12,

            card(
              card_header("Informações do Sistema"),
              HTML(
                "
                <h5>Painel de Vigilância Epidemiológica de Conjuntivites</h5>
                <p>Sistema de monitoramento em tempo real de casos de conjuntivites com ferramentas
                avançadas de análise epidemiológica.</p>

                <h6 class='mt-4'>Funcionalidades</h6>
                <ul>
                  <li><strong>Filtros Encadeados:</strong> Selecione DRS, GVE e Município com atualização automática</li>
                  <li><strong>Indicadores Dinâmicos:</strong> Visualize casos, incidência e situação epidemiológica</li>
                  <li><strong>Gráficos Interativos:</strong> Série temporal, canal endêmico e diagrama de controle</li>
                  <li><strong>Mapa Coroplético:</strong> Distribuição geográfica com zoom e detalhes</li>
                </ul>

                <h6 class='mt-4'>Dados</h6>
                <p>Sistema contém dados simulados para demonstração com histórico de 2022 a 2024.</p>
              "
              )
            )
          )
        )
      )
    )
  ),

  footer = div(
    class = "text-center text-muted py-3 border-top mt-5 small",
    "© 2024 Secretaria de Saúde - Vigilância Epidemiológica | Desenvolvido com Shiny e Bootstrap 5"
  )
)
