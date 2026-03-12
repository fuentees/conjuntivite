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
  1. Série temporal por semana epidemiológica (barras + média móvel contínua de 4 semanas)
  2. Canal endêmico (média histórica + limites + ano selecionado, com referência sem 2011)
  3. Casos por GVE
  4. Modelo de tendência semanal com projeção curta
  5. Motor preditivo de risco de surto (IA)
  6. Distribuição por sexo e faixa etária
  7. Ações/treinamentos realizados
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
- IA executada em segundo plano (com visualização de projeções):
  - Gráficos de tendência e probabilidade preditiva de surto
  - Notificação operacional quando risco preditivo for ALTO/CRÍTICO

## Fonte de dados

O app usa somente MariaDB como fonte de dados.

- dplyr / tidyr / lubridate
- plotly
- leaflet
- DBI + RMariaDB
