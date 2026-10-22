# Painel de Vigilância Epidemiológica de Conjuntivites

Aplicativo Shiny para o monitoramento semanal das notificações de conjuntivite no estado de São Paulo, por DRS, GVE, município e unidade de saúde.

## O que o painel mostra

- **Cards:** total de casos (com variação contra o mesmo período do ano anterior), incidência por 100 mil habitantes, surtos notificados e situação epidemiológica (Normal, Alerta ou Epidemia).
- **Série temporal:** casos por semana epidemiológica com média móvel de 4 semanas. Com "Todos" os anos, mostra casos por ano.
- **Canal endêmico:** método dos quartis (1º quartil, mediana e 3º quartil) dos até 10 anos anteriores, sem anos atípicos.
- **Tendência e índice de risco:** suavização LOESS com projeção de 8 semanas e um índice heurístico de risco de surto.
- **Casos por território:** por GVE, por município (com uma GVE selecionada) ou por unidade de saúde (com um município selecionado).
- **Tabela:** casos e incidência no período e nas últimas 4 semanas, com alerta visual e botão para baixar CSV.
- **Mapa:** municípios, GVE ou DRS coloridos por incidência. Clicar em uma área filtra o painel por ela.
- **Perfil:** sexo, faixa etária e ações realizadas, com a porcentagem de casos que têm o campo preenchido.

A aba **Sobre** explica a metodologia de cada indicador.

## Como rodar

1. Abra `conjuntivite.Rproj` no RStudio.
2. Rode `renv::restore()` para instalar os pacotes nas versões do `renv.lock`.
3. Rode `shiny::runApp()`.

O computador precisa alcançar o banco MariaDB da rede interna (diretamente ou por VPN).

## Dados

| Dado | Fonte | Arquivo |
|---|---|---|
| Casos, surtos, sexo, faixa etária e ações | Tabela `OftalmoNotifica9a60` do banco MariaDB | `functions/ConectDb.R` |
| População por município e ano (2007 a 2025) | IBGE: Contagem 2007, Censos 2010 e 2022, estimativas (SIDRA 6579), TCU 2023, estimativas 2024 e 2025 | `data/populacao_ibge_sp.csv` |
| Nomes oficiais de GVE e DRS | Malha de GVE da SES-SP | `data/regionais_sp.csv` |
| Malhas do mapa | IBGE (municípios), SES-SP (GVE), união das GVE (DRS) | `shapes/` |

A coluna `Populacao` do banco não é usada, porque não contém habitantes. O painel liga cada município à tabela do IBGE pelo código IBGE (coluna configurável em `OFTALMO_COL_COD_IBGE`) ou pelo nome. Anos sem população do IBGE usam o ano mais próximo.

A tabela de população foi extraída da série oficial do IBGE organizada no pacote [brpop](https://github.com/rfsaldanha/brpop). Para atualizar quando sair uma nova estimativa, acrescente as linhas do novo ano em `data/populacao_ibge_sp.csv` (colunas `cod_ibge`, `municipio`, `ano`, `populacao`, `fonte`).

## Configuração opcional

As variáveis abaixo podem ir no `.Renviron` do projeto.

| Variável | Padrão | Para que serve |
|---|---|---|
| `OFTALMO_REFRESH_MINUTES` | `5` | Intervalo de atualização dos dados. O banco é consultado no máximo uma vez por intervalo, para todos os usuários. |
| `OFTALMO_CANAL_ANOS_EXCLUIDOS` | `2011,2020,2021` | Anos atípicos fora do canal endêmico. |
| `OFTALMO_CANAL_JANELA` | `10` | Quantos anos anteriores entram no canal endêmico. |
| `OFTALMO_COL_COD_IBGE` | (vazio) | Nome da coluna do banco com o código IBGE do município. |
| `OFTALMO_POPULACAO_CSV` | `data/populacao_ibge_sp.csv` | Outra tabela de população. |
| `MAPA_SHAPE_MUNICIPIO`, `MAPA_SHAPE_GVE`, `MAPA_SHAPE_DRS` | arquivos em `shapes/` | Outras malhas para o mapa. |
| `OFTALMO_DB_HOST`, `OFTALMO_DB_PORT`, `OFTALMO_DB_NAME`, `OFTALMO_DB_USER`, `OFTALMO_DB_PASSWORD` | ver `functions/ConectDb.R` | Conexão com o banco. |

## Estrutura

```
app.R                  # ponto de entrada
R/data.R               # leitura do banco, população IBGE, calendário epidemiológico, malhas
R/ui.R                 # interface
R/server.R             # indicadores, gráficos, tabela e mapa
functions/ConectDb.R   # conexão com o MariaDB
data/                  # população IBGE e nomes das regionais
shapes/                # malhas de município, GVE e DRS
www/                   # banner institucional
```
