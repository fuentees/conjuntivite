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
canal_excluidos_padrao <- Sys.getenv("OFTALMO_CANAL_ANOS_EXCLUIDOS", unset = "2011,2020,2021")
if (length(anos_disponiveis) == 0) {
  anos_disponiveis <- year(Sys.Date())
}
ano_preselecionado <- as.character(max(anos_disponiveis, na.rm = TRUE))
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
    ),
    div(
      class = "sp-gov-overlay",
      "Painel de Vigilância Epidemiológica das Conjuntivites"
    )
  ),
  page_navbar(
    theme = custom_theme,
    title = NULL,
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
            position: relative;
          }
          .sp-gov-banner-img {
            width: 100%;
            height: auto;
            max-height: none;
            object-fit: unset;
            object-position: initial;
            display: block;
          }
          .sp-gov-overlay {
            position: absolute;
            left: 22px;
            bottom: 14px;
            background: transparent;
            border: none;
            border-radius: 0;
            padding: 0;
            color: #ffffff;
            text-align: left;
            font-weight: 700;
            font-size: 1.2rem;
            line-height: 1.1;
            text-shadow: 0 1px 2px rgba(0, 0, 0, 0.45);
            max-width: calc(100% - 44px);
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
          .value-box-subtitle {
            display: block;
            font-size: 0.78rem;
            opacity: 0.9;
            line-height: 1.25;
          }
          .legend-note {
            font-size: 0.82rem;
            color: #52606d;
            line-height: 1.35;
          }
          #tabela_municipios table.dataTable tbody td,
          #tabela_municipios table.dataTable thead th {
            white-space: nowrap;
            font-size: 0.86rem;
          }
          @media (max-width: 768px) {
            .sp-gov-banner-img {
              height: auto;
            }
            .sp-gov-overlay {
              left: 10px;
              bottom: 8px;
              right: 8px;
              padding: 0;
              font-size: 0.85rem;
              line-height: 1.2;
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
        uiOutput("aviso_carga"),
        card(
          class = "filter-card mb-3",
          card_body(
            layout_columns(
              col_widths = c(2, 3, 3, 3, 1),
              selectInput(
                "filtro_ano",
                label = "Ano",
                choices = c("Todos", as.character(anos_disponiveis)),
                selected = ano_preselecionado,
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
            layout_columns(
              col_widths = c(5, 7),
              sliderInput(
                "filtro_semanas",
                label = "Semanas epidemiológicas",
                min = 1,
                max = 53,
                value = c(1, 53),
                step = 1,
                ticks = FALSE,
                width = "100%"
              ),
              uiOutput("uvis_filter_ui")
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
            tags$small(class = "value-box-subtitle", textOutput("total_casos_subtitulo", inline = TRUE)),
            showcase = icon("users"),
            theme = "primary",
            class = "h-100"
          ),
          value_box(
            title = "Incidência",
            value = textOutput("incidencia_media"),
            tags$small(class = "value-box-subtitle", textOutput("incidencia_subtitulo", inline = TRUE)),
            showcase = icon("wave-square"),
            theme = "info",
            class = "h-100"
          ),
          value_box(
            title = "Número de surtos",
            value = textOutput("num_surtos"),
            tags$small(class = "value-box-subtitle", textOutput("num_surtos_subtitulo", inline = TRUE)),
            showcase = icon("triangle-exclamation"),
            theme = "warning",
            class = "h-100"
          ),
          value_box(
            title = "Situação epidemiológica",
            value = textOutput("situacao_epidemiologica"),
            tags$small(class = "value-box-subtitle", textOutput("situacao_descricao", inline = TRUE)),
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
            card_header(textOutput("titulo_serie_temporal", inline = TRUE)),
            plotlyOutput("grafico_serie_temporal", height = "360px"),
            full_screen = TRUE
          ),
          card(
            class = "custom-card",
            card_header(textOutput("titulo_canal_endemico", inline = TRUE)),
            plotlyOutput("grafico_canal_endemico", height = "360px"),
            full_screen = TRUE
          )
        ),
        br(),
        layout_columns(
          col_widths = c(6, 6),
          card(
            class = "custom-card",
            card_header("Tendência semanal e projeção de 8 semanas"),
            plotlyOutput("grafico_tendencia", height = "350px"),
            full_screen = TRUE
          ),
          card(
            class = "custom-card",
            card_header("Índice de risco de surto (heurístico)"),
            plotlyOutput("grafico_alerta_ia", height = "350px"),
            full_screen = TRUE
          )
        ),
        br(),
        layout_columns(
          col_widths = c(12),
          card(
            class = "custom-card",
            card_header(
              class = "d-flex flex-wrap justify-content-between align-items-center gap-2",
              textOutput("titulo_grafico_casos_territorio", inline = TRUE),
              div(
                class = "d-flex flex-wrap gap-3 small",
                conditionalPanel(
                  condition = "input.filtro_gve == 'Todas' && input.filtro_municipio == 'Todos'",
                  radioButtons(
                    "agrupar_territorio",
                    label = NULL,
                    choices = c("Por GVE" = "gve", "Por DRS" = "drs", "Por município" = "municipio"),
                    selected = "gve",
                    inline = TRUE
                  )
                ),
                radioButtons(
                  "metrica_territorio",
                  label = NULL,
                  choices = c("Incidência" = "incidencia", "Casos" = "casos"),
                  selected = "incidencia",
                  inline = TRUE
                )
              )
            ),
            plotlyOutput("grafico_casos_gve", height = "420px"),
            full_screen = TRUE
          )
        ),
        br(),
        layout_columns(
          col_widths = c(7, 5),
          card(
            class = "custom-card",
            card_header(
              class = "d-flex justify-content-between align-items-center",
              textOutput("titulo_tabela_operacional", inline = TRUE),
              downloadButton("baixar_tabela", "Baixar CSV", class = "btn-sm btn-outline-primary")
            ),
            card_body(
              fillable = FALSE,
              DT::DTOutput("tabela_municipios"),
              uiOutput("legenda_tabela_operacional")
            ),
            full_screen = TRUE
          ),
          card(
            class = "custom-card overflow-hidden",
            card_header("Mapa interativo territorial"),
            card_body(
              layout_columns(
                col_widths = c(6, 6),
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
              leafletOutput("mapa", height = "470px"),
              tags$small(
                class = "legend-note d-block mt-2",
                "Tons mais escuros indicam maior incidência no período. Clique em uma área para filtrar o painel por ela."
              )
            ),
            full_screen = TRUE
          )
        ),
        br(),
        card(
          class = "custom-card",
          card_header(textOutput("titulo_silencio", inline = TRUE)),
          card_body(
            fillable = FALSE,
            uiOutput("resumo_silencio"),
            DT::DTOutput("tabela_silencio"),
            tags$small(
              class = "legend-note d-block mt-2",
              "Conta como notificação qualquer registro do município na semana, inclusive notificação negativa (zero casos). ",
              "A situação vale para a semana mais recente da base e não depende do filtro de ano."
            )
          ),
          full_screen = TRUE
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
        card_header("Sobre o painel e metodologia"),
        card_body(
          tags$h5("Painel de Vigilância Epidemiológica das Conjuntivites"),
          tags$p(
            "Monitoramento semanal das notificações de conjuntivite no estado de São Paulo, ",
            "por DRS, GVE, município e unidade de saúde."
          ),
          tags$h6(class = "mt-3", "Fontes"),
          tags$ul(
            tags$li(tags$strong("Casos, surtos e ações: "), "base de notificação de oftalmologia (MariaDB), atualizada automaticamente a cada 5 minutos."),
            tags$li(
              tags$strong("População: "),
              "IBGE por município e ano: Contagem 2007, Censos 2010 e 2022, estimativas anuais (SIDRA 6579), ",
              "população TCU 2023 e estimativas 2024 e 2025. Anos sem dado do IBGE usam o ano mais próximo (ex.: 2026 usa 2025)."
            ),
            tags$li(tags$strong("Malhas: "), "municípios do IBGE, GVE da SES-SP e DRS formadas pela união das GVE.")
          ),
          tags$h6(class = "mt-3", "Indicadores"),
          tags$ul(
            tags$li(
              tags$strong("Incidência: "),
              "casos ÷ população × 100 mil. O denominador soma a população dos municípios do território filtrado que notificam nesta base. ",
              "Com o ano selecionado, é a incidência acumulada até a última semana com dados. Com \"Todos\", é a média anual do período."
            ),
            tags$li(
              tags$strong("Canal endêmico: "),
              paste0(
                "método dos quartis. Para cada semana epidemiológica, calcula o 1º quartil, a mediana e o 3º quartil dos casos ",
                "nos até 10 anos anteriores ao ano selecionado, sem os anos atípicos (",
                gsub(",", ", ", canal_excluidos_padrao, fixed = TRUE), "). ",
                "2011 teve um surto excepcional na capital e 2020–2021 tiveram queda de notificações na pandemia."
              )
            ),
            tags$li(
              tags$strong("Situação epidemiológica: "),
              "compara a média das últimas 4 semanas com o canal endêmico. Acima do 3º quartil = Epidemia; ",
              "acima da mediana = Alerta; até a mediana = Normal. A semana mais recente fica de fora no ano corrente, ",
              "porque ainda recebe notificações em atraso."
            ),
            tags$li(
              tags$strong("Índice de risco de surto: "),
              "indicador heurístico (não é um modelo treinado). Combina o quanto os casos da semana se afastam da média histórica ",
              "(z-score) com o crescimento da média móvel de 4 semanas, numa escala de 0 a 100%. Acima de 70% indica risco alto."
            ),
            tags$li(
              tags$strong("Tendência: "),
              "suavização LOESS das semanas observadas e projeção linear das 8 semanas seguintes, limitada ao fim do ano epidemiológico."
            ),
            tags$li(
              tags$strong("Filtro de semanas: "),
              "restringe cards, tabela, mapa, ranking e perfil às semanas escolhidas. Série, canal endêmico e tendência ",
              "continuam mostrando o ano inteiro, com as semanas escolhidas destacadas."
            ),
            tags$li(
              tags$strong("Ranking de territórios: "),
              "compara GVE, DRS ou municípios por incidência (padrão) ou por casos. A incidência evita que regiões grandes ",
              "pareçam piores só por terem mais habitantes."
            ),
            tags$li(
              tags$strong("Silêncio epidemiológico: "),
              "semanas desde a última notificação de cada município do território, contadas a partir da semana mais recente da base. ",
              "Inclui os municípios que nunca notificaram."
            ),
            tags$li(
              tags$strong("Semana epidemiológica: "),
              "de domingo a sábado. A SE 1 é a primeira semana com pelo menos 4 dias no ano; alguns anos têm 53 semanas."
            )
          ),
          tags$h6(class = "mt-3", "Limitações dos dados"),
          tags$ul(
            tags$li("Alguns municípios não têm nenhuma notificação nesta base e aparecem como \"Sem notificação\" no mapa."),
            tags$li("Sexo, faixa etária, surtos e ações só são registrados nas notificações mais recentes. Os gráficos mostram a porcentagem de casos com o campo preenchido."),
            tags$li("A incidência por UVIS não é calculada, porque o IBGE não publica população por UVIS.")
          )
        )
      )
    )
    ),
    footer = div(
      class = "text-center text-muted py-3 border-top small",
      "Secretaria de Estado da Saúde de São Paulo · Vigilância Epidemiológica · População: IBGE"
    )
  )
)
