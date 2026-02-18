# Estrutura de arquivos geográficos (SHAPEFILE)

Coloque aqui os arquivos geográficos para habilitar mapas por nível:

- `shapes/municipio/` -> mapa de municípios
- `shapes/gve/` -> mapa por GVE
- `shapes/drs/` -> mapa por DRS

Cada camada pode ter qualquer nome de arquivo `.shp`, desde que os arquivos auxiliares estejam junto:

- `.shp`
- `.shx`
- `.dbf`
- `.prj` (recomendado)
- `.qix` (opcional, melhora performance)

## Como o app localiza os shapes

1. Variável de ambiente (prioridade mais alta):
   - `MAPA_SHAPE_MUNICIPIO`
   - `MAPA_SHAPE_GVE`
   - `MAPA_SHAPE_DRS`
2. Caminho padrão:
   - `shapes/municipio/municipio.shp`
   - `shapes/gve/gve.shp`
   - `shapes/drs/drs.shp`
3. Primeiro `.shp` encontrado na pasta do nível.

## Coluna-chave para junção (detecção automática)

O app tenta detectar automaticamente a coluna de chave por nível:

- Município: `municipio`, `name_muni`, `nm_mun`, `nome`, etc.
- GVE: `gve`, `dominio_gve`, `nome_gve`, etc.
- DRS: `drs`, `nome_drs`, `regional_saude`, etc.

Se o nome da coluna for diferente, o app usa fallback para a primeira coluna textual.

## Dica de qualidade

Prefira valores de nome compatíveis com os filtros do painel (mesma grafia geral), pois isso melhora a taxa de junção entre dados e geometrias.
