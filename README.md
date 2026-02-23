# Painel de Vigilância Epidemiológica de Conjuntivites

Aplicativo Shiny para monitoramento semanal de conjuntivites com interface moderna, filtros encadeados e análise epidemiológica em tempo real.

## Destaques

- Banner institucional obrigatorio no topo (`www/banner_governo_sp.svg`).
- Layout clean, responsivo, tema claro e filtros horizontais no topo.
- Cards com indicadores principais:
  - Total de casos
  - Incidência por 100 mil habitantes
  - Número de surtos notificados (conforme filtros ativos)
  - Situação epidemiológica (Normal / Alerta / Epidemia)
- Gráficos interativos (plotly):
  1. Série temporal por semana epidemiológica (barras)
  2. Canal endêmico (média histórica + limites + ano selecionado, com referência sem 2011)
  3. Casos por GVE
  4. Modelo de tendência semanal com projeção curta
  5. Distribuição por sexo e faixa etária
  6. Ações/treinamentos realizados
- Mapa interativo territorial (leaflet):
  - Níveis: Município, GVE e DRS
  - Vinculado aos filtros de Ano, DRS, GVE e Município
  - Cores por faixa de incidência (derivadas da tabela de municípios)
  - Clique no município aplica filtro automático de município no painel
  - Prioriza shapefile local por nível
  - Fallback automático para geobr (município) ou marcadores (sem shape)
- Tabela interativa de municípios:
  - GVE, município, ano, população, total anual e total das últimas 4 semanas
  - Incidência acumulada anual + incidência das últimas 4 semanas
  - Formatação numérica brasileira (milhares com `.`)
  - Alerta visual em verde/vermelho baseado na incidência de 4 semanas
  - Sincronização com o mapa por incidência
- IA executada em segundo plano:
  - Geração automática de alertas preditivos sem painel visual dedicado
  - Notificação operacional quando risco preditivo for ALTO/CRÍTICO

## Fonte de dados

O app carrega dados do banco MariaDB (`OftalmoNotifica9a60`) usando `functions/ConectDb.R`.

Por padrão, o app está em modo **DB-first estrito** (sem mock).
Se você quiser liberar fallback para dados simulados, configure:

```bash
export OFTALMO_ALLOW_MOCK="true"
```

### Campos esperados da base

- `DtNotificacao` (data de notificação)
- `MunicipioNotificacao` (município)
- `UVIS` (somente para registros do Município de São Paulo, quando disponível)
- `dominio_gve` (GVE)
- `dominio_drs` (DRS, quando disponível)
- `TotalCaso` (casos)
- `NuSurto` e/ou `Surto` (informações de surto)
- `SexMasc` e `SexFem` (perfil por sexo)
- `FxMenorUmAno`, `FxUmQuatro`, `FxCincoNove`, `FxDezQuatorze`, `FxQuizeOuMais` (faixa etária)
- `NuAcaoEducativa`, `NuTreinamento`, `NuColetaMaterialBio`, `NuEncamimento` (ações)

Também são reconhecidos automaticamente formatos comuns da sua base, como:
`GVE_NOME`, `DRS_NOME`, `ANO`, `SemEpidemio`.

Esses campos são transformados para a estrutura interna:

`ano`, `semana`, `drs`, `gve`, `municipio`, `casos`, `populacao`, `incidencia`, `surtos`,
`sexo_masc`, `sexo_fem`, `fx_menor1`, `fx_1a4`, `fx_5a9`, `fx_10a14`, `fx_15mais`,
`acao_educativa`, `treinamentos`, `coleta_material`, `encaminhamentos`

## Configuração de conexão (opcional por variável de ambiente)

O arquivo `functions/ConectDb.R` já vem com defaults, mas você pode sobrescrever por ambiente:

```bash
export OFTALMO_DB_HOST="192.168.1.204"
export OFTALMO_DB_PORT="3306"
export OFTALMO_DB_NAME="cevesp_2k17"
export OFTALMO_DB_USER="vfontes"
export OFTALMO_DB_PASSWORD="Saude@123Oftal"
```

Se os nomes das colunas no banco forem diferentes do dicionário, você pode mapear:

```bash
export OFTALMO_COL_GVE="nome_real_coluna_gve"
export OFTALMO_COL_DRS="nome_real_coluna_drs"
export OFTALMO_COL_MUNICIPIO="nome_real_coluna_municipio"
export OFTALMO_COL_UVIS="nome_real_coluna_uvis"
export OFTALMO_COL_CASOS="nome_real_coluna_casos"
export OFTALMO_COL_DATA="nome_real_coluna_data_notificacao"
export OFTALMO_COL_ANO="nome_real_coluna_ano"
export OFTALMO_COL_SEMANA="nome_real_coluna_semana_epidemiologica"
export OFTALMO_COL_SEXO_MASC="nome_real_coluna_sexo_masculino"
export OFTALMO_COL_SEXO_FEM="nome_real_coluna_sexo_feminino"
export OFTALMO_COL_FX_MENOR1="nome_real_coluna_fx_menor_1"
export OFTALMO_COL_FX_1A4="nome_real_coluna_fx_1_4"
export OFTALMO_COL_FX_5A9="nome_real_coluna_fx_5_9"
export OFTALMO_COL_FX_10A14="nome_real_coluna_fx_10_14"
export OFTALMO_COL_FX_15MAIS="nome_real_coluna_fx_15_mais"
export OFTALMO_COL_ACAO_EDUCATIVA="nome_real_coluna_acao_educativa"
export OFTALMO_COL_TREINAMENTO="nome_real_coluna_treinamento"
export OFTALMO_COL_COLETA_MATERIAL="nome_real_coluna_coleta_material"
export OFTALMO_COL_ENCAMINHAMENTO="nome_real_coluna_encaminhamento"
export OFTALMO_COL_MEDIDA_ADOTADA="nome_real_coluna_medida_adotada"
```

## Instalação

```r
install.packages(c(
  "shiny", "bslib", "dplyr", "plotly", "leaflet",
  "lubridate", "tidyr", "DT", "DBI", "RMariaDB", "geobr", "sf"
))

shiny::runApp(".")
```

> `geobr` e `sf` são recomendados para coroplético com polígonos.
> Sem eles, o mapa funciona em modo fallback com marcadores.

Para usar o banner oficial em imagem, basta adicionar:

- `www/banner_governo_sp.png` (tem prioridade sobre o SVG padrão)

## Mapas locais (shapefile)

Você pode colocar os arquivos geográficos locais para cada nível:

- `shapes/municipio/`
- `shapes/gve/`
- `shapes/drs/`

Também funciona com nomes flexíveis (ex.: `GVE-shape.shp`, `drs_regional.shp`), desde que o arquivo esteja em uma dessas pastas ou em `shapes/`.

Arquivos esperados na pasta da camada:

- `.shp`
- `.shx`
- `.dbf`
- `.prj` (recomendado)
- `.qix` (opcional)

Também é possível informar caminho via variável de ambiente:

```bash
export MAPA_SHAPE_MUNICIPIO="/caminho/municipios.shp"
export MAPA_SHAPE_GVE="/caminho/gve.shp"
export MAPA_SHAPE_DRS="/caminho/drs.shp"
```

> Dica: a variável de ambiente pode apontar para um arquivo `.shp` **ou para uma pasta** contendo os shapefiles.

Mais detalhes em `shapes/README.md`.

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
├── shapes/
│   ├── municipio/      # shapefile de município (opcional)
│   ├── gve/            # shapefile de GVE (opcional)
│   ├── drs/            # shapefile de DRS (opcional)
│   └── README.md
├── www/
│   └── banner_governo_sp.svg
└── README.md
```

## Comportamento reativo

- Ano inicia pré-selecionado em **Todos**.
- DRS -> atualiza GVE
- GVE -> atualiza Município
- Município São Paulo -> exibe filtro adicional de UVIS
- Bloco "Casos por território": mostra por GVE quando filtro GVE = Todas; ao selecionar um GVE, passa a mostrar por município.
- Todos os gráficos, indicadores e mapa reagem aos filtros em tempo real.

## Stack

- R 4.3+
- Shiny + bslib
- dplyr / tidyr / lubridate
- plotly
- leaflet
- DBI + RMariaDB
