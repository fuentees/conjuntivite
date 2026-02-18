# Painel de Vigilância Epidemiológica de Conjuntivites

Um aplicativo Shiny desenvolvido em R para monitoramento em tempo real de casos de conjuntivites em instituições de saúde.

## Características

### Interface
- **Design moderno e responsivo** - Interface limpa com tema azul institucional
- **Layout horizontal** - Filtros posicionados no topo com Cards informativos
- **Elementos visuais** - Cantos arredondados, sombras suaves e transições

### Filtros Interativos
- Seleção por Ano
- Seleção por Divisão Regional de Saúde (DRS)
- Seleção por Grupo de Vigilância Epidemiológica (GVE) - encadeada ao DRS
- Seleção por Município - encadeada ao GVE

### Indicadores
- **Total de Casos** - Soma de casos no período
- **Incidência** - Taxa por 100 mil habitantes
- **Número de Surtos** - Semanas epidêmicas críticas
- **Situação Epidemiológica** - Normal, Alerta ou Epidemia

### Gráficos Interativos
1. **Série Temporal** - Casos por semana com plotly
2. **Canal Endêmico** - Limites históricos e ano atual
3. **Diagrama de Controle** - Análise de variação
4. **Casos por GVE** - Distribuição por gestão

### Mapa
- Mapa coroplético com leaflet
- Círculos proporcionais ao número de casos
- Cores indicando intensidade de incidência
- Tooltips com informações detalhadas

## Instalação

```r
# Instalar dependências
install.packages(c("shiny", "shinyjs", "dplyr", "ggplot2", "plotly", "leaflet", "lubridate", "tidyr"))

# Executar o app
shiny::runApp(".")
```

## Estrutura do Projeto

```
project/
├── app.R              # Arquivo principal que executa o Shiny
├── R/
│   ├── data.R         # Geração e estrutura dos dados simulados
│   ├── ui.R           # Interface do usuário
│   └── server.R       # Lógica reativa do servidor
└── README.md          # Este arquivo
```

## Dados

O aplicativo utiliza dados simulados com a seguinte estrutura:
- **ano**: 2022, 2023, 2024
- **semana**: 1-52 (Semanas Epidemiológicas)
- **DRS**: 5 Divisões Regionais
- **GVE**: 15 Grupos de Vigilância
- **municipio**: Diversos municípios por GVE
- **casos**: Número simulado de casos
- **incidência**: Taxa calculada por 100 mil habitantes

## Funcionalidades Reativas

Todos os elementos reagem automaticamente aos filtros:
- Filtros encadeados atualizam opções subsequentes
- Gráficos se atualizam em tempo real
- Indicadores recalculam automaticamente
- Mapa filtra e recolore conforme seleção

## Tecnologias

- **R 4.3.0+**
- **Shiny** - Framework web
- **Plotly** - Gráficos interativos
- **Leaflet** - Mapas
- **dplyr** - Manipulação de dados
- **ggplot2** - Visualizações estáticas
