# Painel de Vigilância Epidemiológica de Conjuntivites

Aplicativo Shiny para monitoramento semanal de conjuntivites com interface moderna, filtros encadeados e análise epidemiológica em tempo real.

## Destaques

- Banner institucional obrigatorio no topo (`www/banner_governo_sp.svg`).
- Layout clean, responsivo, tema claro e filtros horizontais no topo.
- Cards com indicadores principais:
  - Total de casos
  - Incidência por 100 mil habitantes
  - Número de surtos (semanas acima do limite superior)
  - Situação epidemiológica (Normal / Alerta / Epidemia)
- Gráficos interativos (plotly):
  1. Série temporal por semana epidemiológica
  2. Canal endêmico (média histórica + limites + ano selecionado)
  3. Diagrama de controle
  4. Casos por GVE
- Mapa interativo por município (leaflet):
  - Coroplético com polígonos quando `geobr` + `sf` estiverem disponíveis
  - Fallback automático para marcadores quando o shape não estiver disponível

## Fonte de dados

O app tenta carregar dados do banco MariaDB (`OftalmoNotifica9a60`) usando `functions/ConectDb.R`.
Se a conexão falhar, usa automaticamente dados simulados (mock data), sem interromper o painel.

### Campos esperados da base

- `DtNotificacao` (data de notificação)
- `MunicipioNotificacao` (município)
- `dominio_gve` (GVE)
- `TotalCaso` (casos)
- `NuSurto` e/ou `Surto` (informações de surto)

Esses campos são transformados para a estrutura interna:

`ano`, `semana`, `drs`, `gve`, `municipio`, `casos`, `populacao`, `incidencia`, `surtos`

## Configuração de conexão (opcional por variável de ambiente)

O arquivo `functions/ConectDb.R` já vem com defaults, mas você pode sobrescrever por ambiente:

```bash
export OFTALMO_DB_HOST="192.168.1.204"
export OFTALMO_DB_NAME="cevesp_2k17"
export OFTALMO_DB_USER="vfontes"
export OFTALMO_DB_PASSWORD="Saude@123Oftal"
```

## Instalação

```r
install.packages(c(
  "shiny", "bslib", "dplyr", "plotly", "leaflet",
  "lubridate", "tidyr", "DBI", "RMariaDB", "geobr", "sf"
))

shiny::runApp(".")
```

> `geobr` e `sf` são recomendados para coroplético com polígonos.
> Sem eles, o mapa funciona em modo fallback com marcadores.

## Estrutura

```text
project/
├── app.R
├── functions/
│   └── ConectDb.R     # conexão e consulta ao MariaDB
├── R/
│   ├── data.R         # ingestão (DB + mock), transformação e utilitários
│   ├── ui.R           # interface
│   └── server.R       # reatividade, indicadores e gráficos
└── README.md
```

## Comportamento reativo

- DRS -> atualiza GVE
- GVE -> atualiza Município
- Todos os gráficos, indicadores e mapa reagem aos filtros em tempo real.

## Stack

- R 4.3+
- Shiny + bslib
- dplyr / tidyr / lubridate
- plotly
- leaflet
- DBI + RMariaDB
