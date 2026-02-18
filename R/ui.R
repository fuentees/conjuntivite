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

ensure_attached_package("bslib")
ensure_attached_package("plotly")
ensure_attached_package("leaflet")

custom_theme <- bs_theme(
  version = 5,
  preset = "bootstrap",
  primary = "#005ca9",
  secondary = "#6c757d",
  success = "#2e7d32",
  danger = "#d32f2f",
  warning = "#ed6c02",
  info = "#0288d1",
  light = "#f4f6f8",
  dark = "#1f2933",
  base_font = font_collection(
    font_google("Inter"),
    "Segoe UI",
    "sans-serif"
  )
)

anos_disponiveis <- sort(unique(dados_epidemiologicos$ano), decreasing = TRUE)
if (length(anos_disponiveis) == 0) {
  anos_disponiveis <- year(Sys.Date())
}

ui <- tagList(
  div(
    class = "sp-gov-banner-wrapper",
    tags$img(
      src = "banner_governo_sp.svg",
      class = "sp-gov-banner-img",
      alt = "Banner institucional da Secretaria de Estado da Saúde de São Paulo"
    )
  ),
  page_navbar(
    theme = custom_theme,
    title = div(
      class = "d-flex align-items-center gap-2",
      div(
        style = "width: 38px; height: 38px; border-radius: 10px; background: linear-gradient(135deg, #005ca9, #1976d2); display:flex; align-items:center; justify-content:center;",
        icon("eye", style = "color: white;")
      ),
      div(
        div("Painel de Vigilância Epidemiológica", style = "font-weight: 700; line-height: 1.1;"),
        tags$small("Conjuntivites", class = "text-muted")
      )
    ),
    bg = "#ffffff",
    inverse = FALSE,
    underlines = TRUE,
    collapsible = TRUE,
    header = tags$head(
      tags$style(
        HTML(
          "
          .sp-gov-banner-wrapper {
            width: 100%;
            background: #00508f;
            border-bottom: 1px solid rgba(255, 255, 255, 0.1);
          }
          .sp-gov-banner-img {
            width: 100%;
            max-height: 92px;
            object-fit: cover;
            display: block;
          }
          .app-container {
            background: #f4f6f8;
            min-height: calc(100vh - 56px);
            padding-bottom: 2rem;
          }
          .filter-card,
          .custom-card,
          .value-box {
            border: 0;
            border-radius: 16px !important;
            box-shadow: 0 4px 18px rgba(32, 50, 68, 0.08);
            transition: transform 0.2s ease, box-shadow 0.2s ease;
          }
          .filter-card:hover,
          .custom-card:hover,
          .value-box:hover {
            transform: translateY(-1px);
            box-shadow: 0 8px 24px rgba(32, 50, 68, 0.12);
          }
          .bslib-value-box .value-box-title {
            font-size: 0.95rem;
            font-weight: 600;
          }
          .bslib-value-box .value-box-value {
            font-size: 1.35rem;
            font-weight: 700;
          }
          .text-chip {
            font-size: 0.8rem;
            padding: 0.2rem 0.65rem;
            border-radius: 999px;
            background: #e8f1fb;
            color: #005ca9;
            font-weight: 600;
          }
          "
        )
      )
    ),
    nav_panel(
    "Painel",
    div(
      class = "app-container",
      div(
        class = "container-fluid pt-3",
        card(
          class = "filter-card mb-3",
          card_body(
            layout_columns(
              col_widths = c(2, 3, 3, 3, 1),
              selectInput(
                "filtro_ano",
                label = "Ano",
                choices = anos_disponiveis,
                selected = anos_disponiveis[1]
              ),
              selectInput(
                "filtro_drs",
                label = "DRS",
                choices = c("Todas", sort(unique(dados_epidemiologicos$drs)))
              ),
              selectInput(
                "filtro_gve",
                label = "GVE",
                choices = "Todas"
              ),
              selectInput(
                "filtro_municipio",
                label = "Município",
                choices = "Todos"
              ),
              div(
                class = "d-flex align-items-end h-100",
                actionButton(
                  "btn_reset",
                  label = "Limpar",
                  class = "btn-outline-secondary w-100",
                  icon = icon("rotate-left")
                )
              )
            ),
            div(
              class = "d-flex justify-content-end mt-1",
              span(textOutput("fonte_dados", inline = TRUE), class = "text-chip")
            )
          )
        ),
        layout_columns(
          col_widths = c(3, 3, 3, 3),
          value_box(
            title = "Total de casos",
            value = textOutput("total_casos"),
            showcase = icon("users"),
            theme = "primary",
            class = "h-100"
          ),
          value_box(
            title = "Incidência",
            value = textOutput("incidencia_media"),
            subtitle = "Casos por 100 mil hab.",
            showcase = icon("wave-square"),
            theme = "info",
            class = "h-100"
          ),
          value_box(
            title = "Número de surtos",
            value = textOutput("num_surtos"),
            subtitle = "Semanas acima do limite",
            showcase = icon("triangle-exclamation"),
            theme = "warning",
            class = "h-100"
          ),
          value_box(
            title = "Situação epidemiológica",
            value = textOutput("situacao_epidemiologica"),
            subtitle = textOutput("situacao_descricao"),
            showcase = icon("circle-exclamation"),
            theme = "danger",
            class = "h-100"
          )
        ),
        br(),
        layout_columns(
          col_widths = c(6, 6),
          card(
            class = "custom-card",
            card_header("Série temporal — casos por semana epidemiológica"),
            plotlyOutput("grafico_serie_temporal", height = "360px"),
            full_screen = TRUE
          ),
          card(
            class = "custom-card",
            card_header("Canal endêmico"),
            plotlyOutput("grafico_canal_endemico", height = "360px"),
            full_screen = TRUE
          )
        ),
        br(),
        layout_columns(
          col_widths = c(6, 6),
          card(
            class = "custom-card",
            card_header("Diagrama de controle"),
            plotlyOutput("grafico_diagrama_controle", height = "350px"),
            full_screen = TRUE
          ),
          card(
            class = "custom-card",
            card_header("Casos por GVE"),
            plotlyOutput("grafico_casos_gve", height = "350px"),
            full_screen = TRUE
          )
        ),
        br(),
        card(
          class = "custom-card overflow-hidden",
          card_header("Mapa interativo territorial"),
          card_body(
            layout_columns(
              col_widths = c(4, 8),
              selectInput(
                "nivel_mapa",
                label = "Nível geográfico do mapa",
                choices = c("Município" = "municipio", "GVE" = "gve", "DRS" = "drs"),
                selected = "municipio"
              ),
              div(
                class = "d-flex justify-content-end align-items-end h-100",
                span(textOutput("fonte_geografica", inline = TRUE), class = "text-chip")
              )
            ),
            leafletOutput("mapa", height = "620px")
          ),
          full_screen = TRUE
        )
      )
    )
    ),
    nav_panel(
    "Sobre",
    div(
      class = "container-fluid py-3",
      card(
        class = "custom-card",
        card_header("Sobre o sistema"),
        card_body(
          HTML(
            "
            <h5>Painel de Vigilância Epidemiológica de Conjuntivites</h5>
            <p class='mb-2'>
              Sistema com foco em monitoramento semanal e suporte à decisão com indicadores,
              canal endêmico, diagrama de controle e mapa interativo.
            </p>
            <ul>
              <li><strong>Filtros encadeados:</strong> Ano, DRS, GVE e Município.</li>
              <li><strong>Dados:</strong> integração com MariaDB e fallback automático para mock data.</li>
              <li><strong>Análises:</strong> série temporal, canal endêmico, surtos e situação epidemiológica.</li>
            </ul>
            "
          )
        )
      )
    )
    ),
    footer = div(
      class = "text-center text-muted py-3 border-top small",
      "Secretaria de Saúde - Vigilância Epidemiológica | Desenvolvido com Shiny"
    )
  )
)
