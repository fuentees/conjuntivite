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
ensure_attached_package("DT")

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
drs_disponiveis <- sort(unique(as.character(dados_epidemiologicos$drs)))
drs_disponiveis <- drs_disponiveis[!is.na(drs_disponiveis) & trimws(drs_disponiveis) != "" & tolower(drs_disponiveis) != "na"]

banner_topo_src <- if (file.exists(file.path("www", "banner_governo_sp.png"))) {
  "banner_governo_sp.png"
} else {
  "banner_governo_sp.svg"
}

ui <- tagList(
  div(
    class = "sp-gov-banner-wrapper",
    tags$img(
      src = banner_topo_src,
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
    navbar_options = navbar_options(
      bg = "#ffffff",
      theme = "light",
      collapsible = TRUE
    ),
    header = tags$head(
      tags$style(
        HTML(
          "
          .sp-gov-banner-wrapper {
            width: 100%;
            background: #00508f;
            display: flex;
            align-items: center;
            justify-content: center;
            border-bottom: 1px solid rgba(255, 255, 255, 0.1);
            overflow: hidden;
          }
          .sp-gov-banner-img {
            width: 100%;
            height: 112px;
            max-height: none;
            object-fit: cover;
            object-position: center;
            display: block;
          }
          .app-container {
            background: #f4f6f8;
            min-height: calc(100vh - 56px);
            padding-bottom: 2rem;
          }
          .navbar .navbar-nav {
            gap: 0.35rem;
          }
          .navbar .navbar-nav .nav-link {
            border-radius: 10px;
            color: #243b53 !important;
            font-weight: 500;
            padding: 0.45rem 0.9rem !important;
            transition: background-color 0.2s ease, color 0.2s ease;
          }
          .navbar .navbar-nav .nav-link:hover {
            background: #eef4fb;
            color: #005ca9 !important;
          }
          .navbar .navbar-nav .nav-link.active {
            background: #e5effb;
            color: #005ca9 !important;
            font-weight: 700;
            border-bottom: none;
            box-shadow: inset 0 -2px 0 #005ca9;
          }
          .filter-card {
            position: relative;
            z-index: 20;
            overflow: visible !important;
          }
          .filter-card .card-body {
            overflow: visible !important;
          }
          .filter-card .shiny-input-container,
          .filter-card .selectize-control {
            z-index: 30;
          }
          .selectize-dropdown,
          .selectize-dropdown-content {
            z-index: 3000 !important;
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
          #tabela_municipios table.dataTable tbody td,
          #tabela_municipios table.dataTable thead th {
            white-space: nowrap;
            font-size: 0.86rem;
          }
          @media (max-width: 768px) {
            .sp-gov-banner-img {
              height: 72px;
            }
          }
          "
        )
      )
    ),
    nav_panel(
    title = tagList(icon("chart-line"), " Painel"),
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
                choices = c("Todos", as.character(anos_disponiveis)),
                selected = "Todos",
                selectize = FALSE
              ),
              selectInput(
                "filtro_drs",
                label = "DRS",
                choices = c("Todas", drs_disponiveis),
                selectize = FALSE
              ),
              selectInput(
                "filtro_gve",
                label = "GVE",
                choices = "Todas",
                selectize = FALSE
              ),
              selectInput(
                "filtro_municipio",
                label = "Município",
                choices = "Todos",
                selectize = FALSE
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
            uiOutput("uvis_filter_ui"),
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
          value_box(
            title = "Risco preditivo (IA)",
            value = textOutput("risco_ia_nivel"),
            subtitle = textOutput("risco_ia_prob"),
            showcase = icon("brain"),
            theme = "warning",
            class = "h-100"
          ),
          value_box(
            title = "Semanas críticas previstas",
            value = textOutput("semanas_criticas_previstas"),
            subtitle = textOutput("resumo_tendencia_curta"),
            showcase = icon("chart-line"),
            theme = "danger",
            class = "h-100"
          )
        ),
        br(),
        layout_columns(
          col_widths = c(6, 6),
          card(
            class = "custom-card",
            card_header("Modelo de tendência semanal (projeção curta)"),
            plotlyOutput("grafico_tendencia", height = "350px"),
            full_screen = TRUE
          ),
          card(
            class = "custom-card",
            card_header("Motor de alerta preditivo de surtos (IA)"),
            plotlyOutput("grafico_alerta_ia", height = "350px"),
            full_screen = TRUE
          )
        ),
        br(),
        layout_columns(
          col_widths = c(4, 4, 4),
          card(
            class = "custom-card",
            card_header("Distribuição por sexo"),
            plotlyOutput("grafico_sexo", height = "320px"),
            full_screen = TRUE
          ),
          card(
            class = "custom-card",
            card_header("Distribuição por faixa etária"),
            plotlyOutput("grafico_faixa_etaria", height = "320px"),
            full_screen = TRUE
          ),
          card(
            class = "custom-card",
            card_header("Ações e treinamentos realizados"),
            plotlyOutput("grafico_acoes_treinamentos", height = "320px"),
            full_screen = TRUE
          )
        ),
        br(),
        layout_columns(
          col_widths = c(6, 6),
          card(
            class = "custom-card",
            card_header("Série temporal — casos por semana epidemiológica (barras)"),
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
          col_widths = c(12),
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
        ),
        br(),
        card(
          class = "custom-card",
          card_header("Tabela operacional por município (alerta rápido)"),
          card_body(
            DT::DTOutput("tabela_municipios")
          ),
          full_screen = TRUE
        )
      )
    )
    ),
    nav_panel(
    title = tagList(icon("info-circle"), " Sobre"),
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
              canal endêmico, tendência preditiva e mapa interativo.
            </p>
            <ul>
              <li><strong>Filtros encadeados:</strong> Ano, DRS, GVE e Município.</li>
              <li><strong>Dados:</strong> integração com MariaDB (com opção de fallback via variável de ambiente).</li>
              <li><strong>Análises:</strong> série temporal, canal endêmico, tendência e alerta preditivo de surtos.</li>
              <li><strong>Painel operacional:</strong> sexo, faixa etária, ações/treinamentos e tabela de municípios com alerta visual.</li>
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
