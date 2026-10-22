library(dplyr)
library(lubridate)
library(tidyr)

if (file.exists("functions/ConectDb.R")) {
  tryCatch(
    source("functions/ConectDb.R"),
    error = function(e) NULL
  )
}

# Remove acentos de forma explícita: iconv(..., "ASCII//TRANSLIT") dá resultados
# diferentes no Windows, Linux e macOS e quebrava o casamento de nomes.
acentos_origem <- "\u00E1\u00E0\u00E2\u00E3\u00E4\u00E9\u00E8\u00EA\u00EB\u00ED\u00EC\u00EE\u00EF\u00F3\u00F2\u00F4\u00F5\u00F6\u00FA\u00F9\u00FB\u00FC\u00E7\u00F1\u00C1\u00C0\u00C2\u00C3\u00C4\u00C9\u00C8\u00CA\u00CB\u00CD\u00CC\u00CE\u00CF\u00D3\u00D2\u00D4\u00D5\u00D6\u00DA\u00D9\u00DB\u00DC\u00C7\u00D1"
acentos_destino <- "aaaaaeeeeiiiiooooouuuucnAAAAAEEEEIIIIOOOOOUUUUCN"

normalize_key <- function(x) {
  x <- enc2utf8(trimws(as.character(x)))
  x <- ifelse(is.na(x), "", x)
  x <- chartr(acentos_origem, acentos_destino, x)
  x <- iconv(x, from = "UTF-8", to = "ASCII", sub = "")
  x <- ifelse(is.na(x), "", x)
  x <- gsub("[^a-z0-9]+", " ", tolower(x))
  trimws(x)
}


pick_existing_column <- function(df, candidates) {
  cols <- names(df)
  idx <- match(tolower(candidates), tolower(cols))
  idx <- idx[!is.na(idx)]
  if (length(idx) == 0) {
    return(NULL)
  }
  cols[idx[1]]
}

pick_existing_column_regex <- function(df, regex_candidates) {
  cols <- names(df)
  cols_lower <- tolower(cols)

  for (pattern in regex_candidates) {
    matches <- grepl(pattern, cols_lower, perl = TRUE)
    if (any(matches)) {
      return(cols[which(matches)[1]])
    }
  }

  NULL
}

pick_column_with_override <- function(
  df,
  override_env_var,
  candidates = character(),
  regex_candidates = character()
) {
  cols <- names(df)
  cols_lower <- tolower(cols)

  override_value <- trimws(Sys.getenv(override_env_var, unset = ""))
  if (override_value != "") {
    idx <- match(tolower(override_value), cols_lower)
    if (!is.na(idx)) {
      return(cols[idx])
    }
  }

  exact_match <- pick_existing_column(df, candidates)
  if (!is.null(exact_match)) {
    return(exact_match)
  }

  if (length(regex_candidates) > 0) {
    regex_match <- pick_existing_column_regex(df, regex_candidates)
    if (!is.null(regex_match)) {
      return(regex_match)
    }
  }

  NULL
}

format_columns_preview <- function(cols, max_items = 30) {
  cols <- as.character(cols)
  if (length(cols) <= max_items) {
    return(paste(cols, collapse = ", "))
  }
  paste0(
    paste(cols[1:max_items], collapse = ", "),
    ", ... (total: ",
    length(cols),
    " colunas)"
  )
}

title_case_safe <- function(x) {
  x <- trimws(as.character(x))
  x[tolower(x) %in% c("", "na", "n/a", "null")] <- "Não informado"
  tools::toTitleCase(tolower(x))
}

sanitize_dimension_values <- function(values, default = "Não informado") {
  values <- trimws(as.character(values))
  values[is.na(values) | values == "" | tolower(values) %in% c("na", "n/a", "null")] <- default
  values
}

sanitize_numeric_values <- function(values, default = 0) {
  values <- suppressWarnings(as.numeric(values))
  values[is.na(values)] <- default
  values
}

sanitize_geo_code <- function(values) {
  values_chr <- trimws(as.character(values))
  values_chr[is.na(values_chr) | values_chr == "" | tolower(values_chr) %in% c("na", "n/a", "null")] <- NA_character_

  numeric_part <- ifelse(
    !is.na(values_chr) & grepl("\\d+", values_chr),
    suppressWarnings(as.integer(sub(".*?(\\d+).*", "\\1", values_chr))),
    NA_integer_
  )

  out <- ifelse(!is.na(numeric_part), as.character(numeric_part), values_chr)
  out[is.na(out) | out == ""] <- NA_character_
  out
}

sanitize_count_or_flag <- function(values, default = 0) {
  values_chr <- trimws(tolower(as.character(values)))
  values_chr[is.na(values_chr)] <- ""

  numeric_guess <- suppressWarnings(as.numeric(gsub(",", ".", values_chr, fixed = TRUE)))
  flag_guess <- ifelse(values_chr %in% c("1", "true", "sim", "s", "yes", "y", "x"), 1, 0)

  out <- ifelse(!is.na(numeric_guess), numeric_guess, flag_guess)
  out[is.na(out)] <- default
  pmax(round(out), 0)
}

infer_drs_from_gve_text <- function(gve_values) {
  gve_values <- as.character(gve_values)
  extracted <- rep(NA_character_, length(gve_values))

  has_drs <- grepl("\\bDRS\\b", gve_values, ignore.case = TRUE)
  extracted[has_drs] <- gsub(
    ".*\\b(DRS\\s*[^\\-|/|;]+).*",
    "\\1",
    gve_values[has_drs],
    ignore.case = TRUE
  )

  extracted <- trimws(extracted)
  extracted[extracted == ""] <- NA_character_
  extracted
}

standardize_gve <- function(gve_raw) {
  gve_raw <- as.character(gve_raw)
  result <- trimws(gve_raw)
  result[result == "" | is.na(result)] <- "Não informado"
  needs_prefix <- !grepl("^gve\\b", tolower(result)) & result != "Não informado"
  result[needs_prefix] <- paste("GVE", result[needs_prefix])
  result
}

standardize_drs <- function(drs_raw) {
  drs_raw <- as.character(drs_raw)
  result <- trimws(drs_raw)
  result[result == "" | is.na(result)] <- "Não informado"
  needs_prefix <- !grepl("^drs\\b", tolower(result)) & result != "Não informado"
  result[needs_prefix] <- paste("DRS", result[needs_prefix])
  result
}

standardize_municipio <- function(municipio_raw) {
  municipio_raw <- as.character(municipio_raw)
  title_case_safe(municipio_raw)
}

map_drs_from_gve <- function(gve, drs_raw = NULL) {
  drs_from_gve <- rep(NA_character_, length(as.character(gve)))
  if (!is.null(drs_raw)) {
    drs_from_gve <- trimws(as.character(drs_raw))
    drs_from_gve[drs_from_gve == ""] <- NA_character_
  }

  drs_from_gve[is.na(drs_from_gve)] <- "DRS Não informado"
  drs_from_gve
}

# ---------------------------------------------------------------------------
# Malhas do mapa
# ---------------------------------------------------------------------------

shape_file_envvars <- c(
  municipio = "MAPA_SHAPE_MUNICIPIO",
  gve = "MAPA_SHAPE_GVE",
  drs = "MAPA_SHAPE_DRS"
)

shape_default_paths <- c(
  municipio = file.path("shapes", "municipio", "municipios_sp.shp"),
  gve = file.path("shapes", "gve", "GVE.shp"),
  drs = file.path("shapes", "drs", "drs.shp")
)

# Coluna com o nome (ou código) da área em cada malha.
shape_key_columns <- c(
  municipio = "CD_MUN",
  gve = "GVE",
  drs = "DRS"
)

find_shape_file <- function(level = c("municipio", "gve", "drs")) {
  level <- match.arg(level)
  configured_path <- trimws(Sys.getenv(unname(shape_file_envvars[level]), unset = ""))
  path <- if (configured_path != "") configured_path else unname(shape_default_paths[level])
  if (file.exists(path)) path else NULL
}

normalize_shape_key <- function(values, level = c("municipio", "gve", "drs")) {
  level <- match.arg(level)

  standardized <- switch(
    level,
    municipio = standardize_municipio(values),
    gve = standardize_gve(values),
    drs = standardize_drs(values)
  )

  normalize_key(standardized)
}

get_local_shape_layer <- local({
  cache <- new.env(parent = emptyenv())

  function(level = c("municipio", "gve", "drs")) {
    level <- match.arg(level)

    if (exists(level, envir = cache, inherits = FALSE)) {
      return(get(level, envir = cache, inherits = FALSE))
    }

    if (!requireNamespace("sf", quietly = TRUE)) {
      return(NULL)
    }

    shp_path <- find_shape_file(level)
    if (is.null(shp_path)) {
      warning("Shapefile do nível '", level, "' não encontrado.", call. = FALSE)
      return(NULL)
    }

    shp <- tryCatch(
      sf::st_read(dsn = shp_path, quiet = TRUE, stringsAsFactors = FALSE, options = "ENCODING=UTF-8"),
      error = function(e) NULL
    )
    if (is.null(shp) || nrow(shp) == 0) {
      return(NULL)
    }

    key_col <- unname(shape_key_columns[level])
    if (!(key_col %in% names(shp))) {
      warning("Shapefile '", shp_path, "' sem a coluna ", key_col, ".", call. = FALSE)
      return(NULL)
    }

    shp <- sf::st_transform(shp, 4326)
    if (level == "municipio") {
      shp$cod_ibge <- as.character(shp[[key_col]])
      shp$map_label <- as.character(shp$NM_MUN)
      shp$map_key <- shp$cod_ibge
    } else {
      shp$map_label <- as.character(shp[[key_col]])
      shp$map_key <- normalize_shape_key(shp$map_label, level = level)
    }

    attr(shp, "shape_path") <- shp_path
    assign(level, shp, envir = cache)
    shp
  }
})

parse_notification_date <- function(value) {
  x <- trimws(as.character(value))
  x[x %in% c("", "NA", "N/A", "NULL", "NaN")] <- NA_character_

  if (length(x) == 0) {
    return(as.Date(character()))
  }

  # Compatibiliza formatos ISO e timezone para strptime/as.POSIXct.
  x_norm <- gsub("T", " ", x, fixed = TRUE)
  x_norm <- sub("Z$", "+0000", x_norm)
  x_norm <- sub("([+-][0-9]{2}):([0-9]{2})$", "\\1\\2", x_norm)

  parsed <- as.POSIXct(rep(NA_real_, length(x_norm)), origin = "1970-01-01", tz = "UTC")
  formats <- c(
    "%Y-%m-%d %H:%M:%OS%z",
    "%Y-%m-%d %H:%M:%S%z",
    "%Y-%m-%d %H:%M%z",
    "%Y-%m-%d %H:%M:%OS",
    "%Y-%m-%d %H:%M:%S",
    "%Y-%m-%d %H:%M",
    "%Y-%m-%d",
    "%Y/%m/%d %H:%M:%S",
    "%Y/%m/%d %H:%M",
    "%Y/%m/%d",
    "%d/%m/%Y %H:%M:%S",
    "%d/%m/%Y %H:%M",
    "%d/%m/%Y",
    "%d/%m/%y %H:%M:%S",
    "%d/%m/%y %H:%M",
    "%d/%m/%y",
    "%d-%m-%Y %H:%M:%S",
    "%d-%m-%Y %H:%M",
    "%d-%m-%Y",
    "%m/%d/%Y %H:%M:%S",
    "%m/%d/%Y %H:%M",
    "%m/%d/%Y"
  )

  for (fmt in formats) {
    idx <- which(!is.na(x_norm) & is.na(parsed))
    if (length(idx) == 0) break
    tried <- suppressWarnings(as.POSIXct(x_norm[idx], format = fmt, tz = "UTC"))
    ok <- !is.na(tried)
    if (any(ok)) {
      parsed[idx[ok]] <- tried[ok]
    }
  }

  as.Date(parsed)
}

prepare_oftalmo_dataset <- function(raw_data) {
  if (is.null(raw_data) || nrow(raw_data) == 0) {
    return(NULL)
  }

  ano_col <- pick_column_with_override(
    raw_data,
    override_env_var = "OFTALMO_COL_ANO",
    candidates = c("ANO", "ano", "Ano"),
    regex_candidates = c("^ano$")
  )
  semana_col <- pick_column_with_override(
    raw_data,
    override_env_var = "OFTALMO_COL_SEMANA",
    candidates = c("SemEpidemio", "semepidemio"),
    regex_candidates = c("^semepidemio$")
  )
  municipio_col <- pick_column_with_override(
    raw_data,
    override_env_var = "OFTALMO_COL_MUNICIPIO",
    candidates = c("MunicipioNotificacao", "municipio_notificacao", "municipio"),
    regex_candidates = c("municipio", "munic")
  )
  gve_col <- pick_column_with_override(
    raw_data,
    override_env_var = "OFTALMO_COL_GVE",
    candidates = c("GVE_NOME", "gve_nome", "dominio_gve", "gve", "gve_numero"),
    regex_candidates = c("gve", "grupo.*vigil", "vigilancia.*epid")
  )
  gve_code_col <- pick_column_with_override(
    raw_data,
    override_env_var = "OFTALMO_COL_GVE_CODIGO",
    candidates = c("gve_numero", "GVE_NUMERO", "CodMacroGVE", "cod_macro_gve", "cod_gve", "codigo_gve"),
    regex_candidates = c("gve.*num", "cod.*gve", "macro.*gve")
  )
  uvis_col <- pick_column_with_override(
    raw_data,
    override_env_var = "OFTALMO_COL_UVIS",
    candidates = c("UVIS", "uvis"),
    regex_candidates = c("uvis")
  )
  unidade_saude_col <- pick_column_with_override(
    raw_data,
    override_env_var = "OFTALMO_COL_UNIDADE",
    candidates = c(
      "Unid_notificacao",
      "unid_notificacao",
      "UnidadeNotificacao",
      "unidade_notificacao",
      "UnidadeSaude",
      "unidade_saude",
      "Unidade",
      "unidade",
      "nCNES"
    ),
    regex_candidates = c("unid", "unidade", "cnes")
  )
  drs_col <- pick_column_with_override(
    raw_data,
    override_env_var = "OFTALMO_COL_DRS",
    candidates = c("DRS_NOME", "drs_nome", "dominio_drs", "drs", "DRS", "drs_numero"),
    regex_candidates = c("\\bdrs\\b", "diretoria.*regional", "regional.*saude")
  )
  drs_code_col <- pick_column_with_override(
    raw_data,
    override_env_var = "OFTALMO_COL_DRS_CODIGO",
    candidates = c("drs_numero", "DRS_NUMERO", "cod_drs", "codigo_drs"),
    regex_candidates = c("drs.*num", "cod.*drs")
  )
  casos_col <- pick_column_with_override(
    raw_data,
    override_env_var = "OFTALMO_COL_CASOS",
    candidates = c("TotalCaso", "total_caso", "casos", "totalcaso"),
    regex_candidates = c("total.*caso", "\\bcasos\\b", "qtd.*cas")
  )
  # A população não vem do banco: a coluna "Populacao" da tabela não contém
  # habitantes. O denominador vem da tabela IBGE (ver anexar_populacao_ibge).
  cod_ibge_col <- pick_column_with_override(
    raw_data,
    override_env_var = "OFTALMO_COL_COD_IBGE",
    candidates = c(
      "CodIBGE", "cod_ibge", "codigo_ibge", "CodMunIBGE", "CodMunicipioIBGE",
      "CodMunicipioNotificacao", "cod_municipio_ibge", "CD_MUN", "ibge"
    )
  )
  surto_col <- pick_column_with_override(
    raw_data,
    override_env_var = "OFTALMO_COL_NU_SURTO",
    candidates = c("NuSurto", "nu_surto"),
    regex_candidates = c("nu.*surto", "numero.*surto")
  )
  surto_flag_col <- pick_column_with_override(
    raw_data,
    override_env_var = "OFTALMO_COL_SURTO_FLAG",
    candidates = c("Surto", "surto"),
    regex_candidates = c("^surto$")
  )
  sex_masc_col <- pick_column_with_override(
    raw_data,
    override_env_var = "OFTALMO_COL_SEXO_MASC",
    candidates = c("SexMasc", "sex_masc", "sexo_masc", "masculino"),
    regex_candidates = c("sex.*masc", "sexo.*masc", "masculino")
  )
  sex_fem_col <- pick_column_with_override(
    raw_data,
    override_env_var = "OFTALMO_COL_SEXO_FEM",
    candidates = c("SexFem", "sex_fem", "sexo_fem", "feminino"),
    regex_candidates = c("sex.*fem", "sexo.*fem", "feminino")
  )
  fx_menor1_col <- pick_column_with_override(
    raw_data,
    override_env_var = "OFTALMO_COL_FX_MENOR1",
    candidates = c("FxMenorUmAno", "fx_menor_um_ano", "fx_menor1", "faixa_menor1"),
    regex_candidates = c("fx.*menor", "faixa.*menor", "menor.*ano")
  )
  fx_1a4_col <- pick_column_with_override(
    raw_data,
    override_env_var = "OFTALMO_COL_FX_1A4",
    candidates = c("FxUmQuatro", "fx_um_quatro", "fx_1a4", "faixa_1a4"),
    regex_candidates = c("fx.*um.*quatro", "fx.*1.*4", "faixa.*1.*4")
  )
  fx_5a9_col <- pick_column_with_override(
    raw_data,
    override_env_var = "OFTALMO_COL_FX_5A9",
    candidates = c("FxCincoNove", "fx_cinco_nove", "fx_5a9", "faixa_5a9"),
    regex_candidates = c("fx.*cinco.*nove", "fx.*5.*9", "faixa.*5.*9")
  )
  fx_10a14_col <- pick_column_with_override(
    raw_data,
    override_env_var = "OFTALMO_COL_FX_10A14",
    candidates = c("FxDezQuatorze", "fx_dez_quatorze", "fx_10a14", "faixa_10a14"),
    regex_candidates = c("fx.*dez.*quatorze", "fx.*10.*14", "faixa.*10.*14")
  )
  fx_15mais_col <- pick_column_with_override(
    raw_data,
    override_env_var = "OFTALMO_COL_FX_15MAIS",
    candidates = c("FxQuizeOuMais", "FxQuinzeOuMais", "fx_15mais", "faixa_15_mais"),
    regex_candidates = c("fx.*quin", "fx.*15", "faixa.*15")
  )
  acao_educativa_col <- pick_column_with_override(
    raw_data,
    override_env_var = "OFTALMO_COL_ACAO_EDUCATIVA",
    candidates = c("NuAcaoEducativa", "nu_acao_educativa", "acao_educativa"),
    regex_candidates = c("acao.*educ")
  )
  treinamento_col <- pick_column_with_override(
    raw_data,
    override_env_var = "OFTALMO_COL_TREINAMENTO",
    candidates = c("NuTreinamento", "nu_treinamento", "treinamento"),
    regex_candidates = c("treinamento")
  )
  coleta_material_col <- pick_column_with_override(
    raw_data,
    override_env_var = "OFTALMO_COL_COLETA_MATERIAL",
    candidates = c("NuColetaMaterialBio", "nu_coleta_material_bio", "coleta_material"),
    regex_candidates = c("coleta.*material")
  )
  encaminhamento_col <- pick_column_with_override(
    raw_data,
    override_env_var = "OFTALMO_COL_ENCAMINHAMENTO",
    candidates = c("NuEncamimento", "NuEncaminhamento", "nu_encaminhamento", "encaminhamento"),
    regex_candidates = c("encam", "encaminha")
  )
  medida_adotada_col <- pick_column_with_override(
    raw_data,
    override_env_var = "OFTALMO_COL_MEDIDA_ADOTADA",
    candidates = c("MedidaAdotada", "medida_adotada"),
    regex_candidates = c("medida.*adot")
  )

  missing_columns <- character()
  if (is.null(ano_col)) missing_columns <- c(missing_columns, "ANO")
  if (is.null(semana_col)) missing_columns <- c(missing_columns, "SemEpidemio")
  if (is.null(municipio_col)) missing_columns <- c(missing_columns, "MunicipioNotificacao")
  if (is.null(gve_col)) missing_columns <- c(missing_columns, "dominio_gve")
  if (is.null(casos_col)) missing_columns <- c(missing_columns, "TotalCaso")

  if (length(missing_columns) > 0) {
    cols_preview <- format_columns_preview(names(raw_data))
    stop(
      paste0(
        "Colunas obrigatórias não encontradas na tabela: ",
        paste(missing_columns, collapse = ", "),
        ". Verifique os nomes reais no banco. ",
        "Colunas disponíveis: ", cols_preview, ". ",
        "Se necessário, configure OFTALMO_COL_ANO/OFTALMO_COL_SEMANA/",
        "OFTALMO_COL_GVE/OFTALMO_COL_MUNICIPIO/OFTALMO_COL_CASOS."
      )
    )
  }

  df <- raw_data %>%
    mutate(
      ano_registro = suppressWarnings(as.integer(.data[[ano_col]])),
      semana_registro = suppressWarnings(as.integer(.data[[semana_col]])),
      municipio = sanitize_dimension_values(.data[[municipio_col]]),
      gve = sanitize_dimension_values(.data[[gve_col]]),
      gve_codigo = if (!is.null(gve_code_col)) sanitize_geo_code(.data[[gve_code_col]]) else NA_character_,
      uvis = if (!is.null(uvis_col)) sanitize_dimension_values(.data[[uvis_col]], default = NA_character_) else NA_character_,
      uvis = ifelse(normalize_key(municipio) == "sao paulo", uvis, NA_character_),
      unidade_saude = if (!is.null(unidade_saude_col)) {
        sanitize_dimension_values(.data[[unidade_saude_col]])
      } else {
        "Não informado"
      },
      drs = if (!is.null(drs_col)) sanitize_dimension_values(.data[[drs_col]]) else "Não informado",
      drs_codigo = if (!is.null(drs_code_col)) sanitize_geo_code(.data[[drs_code_col]]) else NA_character_,
      drs = ifelse(
        drs == "Não informado",
        sanitize_dimension_values(infer_drs_from_gve_text(gve), default = "Não informado"),
        drs
      ),
      casos = sanitize_numeric_values(.data[[casos_col]], default = 0),
      casos = ifelse(is.na(casos), 0, pmax(round(casos), 0)),
      cod_ibge = if (!is.null(cod_ibge_col)) as.character(.data[[cod_ibge_col]]) else NA_character_,
      surto_n = if (!is.null(surto_col)) sanitize_numeric_values(.data[[surto_col]], default = 0) else 0,
      surto_n = ifelse(is.na(surto_n), 0, pmax(round(surto_n), 0)),
      surto_flag = if (!is.null(surto_flag_col)) {
        ifelse(tolower(as.character(.data[[surto_flag_col]])) %in% c("1", "true", "sim", "s"), 1, 0)
      } else {
        0
      },
      sexo_masc = if (!is.null(sex_masc_col)) sanitize_numeric_values(.data[[sex_masc_col]], default = 0) else 0,
      sexo_fem = if (!is.null(sex_fem_col)) sanitize_numeric_values(.data[[sex_fem_col]], default = 0) else 0,
      fx_menor1 = if (!is.null(fx_menor1_col)) sanitize_numeric_values(.data[[fx_menor1_col]], default = 0) else 0,
      fx_1a4 = if (!is.null(fx_1a4_col)) sanitize_numeric_values(.data[[fx_1a4_col]], default = 0) else 0,
      fx_5a9 = if (!is.null(fx_5a9_col)) sanitize_numeric_values(.data[[fx_5a9_col]], default = 0) else 0,
      fx_10a14 = if (!is.null(fx_10a14_col)) sanitize_numeric_values(.data[[fx_10a14_col]], default = 0) else 0,
      fx_15mais = if (!is.null(fx_15mais_col)) sanitize_numeric_values(.data[[fx_15mais_col]], default = 0) else 0,
      acao_educativa = if (!is.null(acao_educativa_col)) {
        sanitize_count_or_flag(.data[[acao_educativa_col]], default = 0)
      } else if (!is.null(medida_adotada_col)) {
        sanitize_count_or_flag(.data[[medida_adotada_col]], default = 0)
      } else {
        0
      },
      treinamentos = if (!is.null(treinamento_col)) sanitize_count_or_flag(.data[[treinamento_col]], default = 0) else 0,
      coleta_material = if (!is.null(coleta_material_col)) sanitize_count_or_flag(.data[[coleta_material_col]], default = 0) else 0,
      encaminhamentos = if (!is.null(encaminhamento_col)) sanitize_count_or_flag(.data[[encaminhamento_col]], default = 0) else 0
    ) %>%
    mutate(
      gve_codigo = sanitize_geo_code(gve_codigo),
      drs_codigo = sanitize_geo_code(drs_codigo),
      ano = ano_registro,
      semana = semana_registro,
      semana = pmin(pmax(semana, 1), 53),
      # NuSurto já conta os surtos; a flag Surto só vale quando NuSurto está vazio.
      surtos = ifelse(surto_n > 0, surto_n, surto_flag),
      sexo_masc = ifelse(is.na(sexo_masc), 0, pmax(round(sexo_masc), 0)),
      sexo_fem = ifelse(is.na(sexo_fem), 0, pmax(round(sexo_fem), 0)),
      fx_menor1 = ifelse(is.na(fx_menor1), 0, pmax(round(fx_menor1), 0)),
      fx_1a4 = ifelse(is.na(fx_1a4), 0, pmax(round(fx_1a4), 0)),
      fx_5a9 = ifelse(is.na(fx_5a9), 0, pmax(round(fx_5a9), 0)),
      fx_10a14 = ifelse(is.na(fx_10a14), 0, pmax(round(fx_10a14), 0)),
      fx_15mais = ifelse(is.na(fx_15mais), 0, pmax(round(fx_15mais), 0)),
      acao_educativa = ifelse(is.na(acao_educativa), 0, pmax(round(acao_educativa), 0)),
      treinamentos = ifelse(is.na(treinamentos), 0, pmax(round(treinamentos), 0)),
      coleta_material = ifelse(is.na(coleta_material), 0, pmax(round(coleta_material), 0)),
      encaminhamentos = ifelse(is.na(encaminhamentos), 0, pmax(round(encaminhamentos), 0))
    ) %>%
    filter(!is.na(ano), !is.na(semana)) %>%
    group_by(ano, semana, drs, drs_codigo, gve, gve_codigo, municipio, cod_ibge, uvis, unidade_saude) %>%
    summarise(
      casos = sum(casos, na.rm = TRUE),
      surtos = sum(surtos, na.rm = TRUE),
      sexo_masc = sum(sexo_masc, na.rm = TRUE),
      sexo_fem = sum(sexo_fem, na.rm = TRUE),
      fx_menor1 = sum(fx_menor1, na.rm = TRUE),
      fx_1a4 = sum(fx_1a4, na.rm = TRUE),
      fx_5a9 = sum(fx_5a9, na.rm = TRUE),
      fx_10a14 = sum(fx_10a14, na.rm = TRUE),
      fx_15mais = sum(fx_15mais, na.rm = TRUE),
      acao_educativa = sum(acao_educativa, na.rm = TRUE),
      treinamentos = sum(treinamentos, na.rm = TRUE),
      coleta_material = sum(coleta_material, na.rm = TRUE),
      encaminhamentos = sum(encaminhamentos, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(ano, semana, drs, gve, municipio, uvis, unidade_saude)

  if (nrow(df) == 0) {
    return(NULL)
  }

  df
}

ensure_json_dependency <- function() {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop(
      "Pacote 'jsonlite' não está instalado. Execute install.packages('jsonlite').",
      call. = FALSE
    )
  }
}

is_internal_oftalmo_dataset <- function(df) {
  required_cols <- c("ano", "semana", "drs", "gve", "municipio", "casos")
  all(required_cols %in% tolower(names(df)))
}

normalize_internal_oftalmo_dataset <- function(df, aggregate = TRUE) {
  if (is.null(df) || nrow(df) == 0) {
    return(NULL)
  }

  df <- tibble::as_tibble(df)

  rename_first_match <- function(data, target, candidates) {
    if (target %in% names(data)) {
      return(data)
    }

    cols <- names(data)
    idx <- match(tolower(candidates), tolower(cols))
    idx <- idx[!is.na(idx)]
    if (length(idx) > 0) {
      names(data)[idx[1]] <- target
    }
    data
  }

  df <- rename_first_match(df, "ano", c("ano", "year"))
  df <- rename_first_match(df, "semana", c("semana", "sem_epi", "week"))
  df <- rename_first_match(df, "drs", c("drs", "dominio_drs", "drs_nome"))
  df <- rename_first_match(df, "drs_codigo", c("drs_codigo", "drs_numero", "cod_drs", "codigo_drs"))
  df <- rename_first_match(df, "gve", c("gve", "dominio_gve", "gve_nome"))
  df <- rename_first_match(df, "gve_codigo", c("gve_codigo", "gve_numero", "cod_gve", "codigo_gve"))
  df <- rename_first_match(df, "municipio", c("municipio", "municipionotificacao"))
  df <- rename_first_match(df, "uvis", c("uvis"))
  df <- rename_first_match(
    df,
    "unidade_saude",
    c(
      "unidade_saude",
      "unidade",
      "unidade_notificacao",
      "unid_notificacao",
      "unidadenotificacao",
      "ncnes"
    )
  )
  df <- rename_first_match(df, "casos", c("casos", "totalcaso", "total_caso"))
  df <- rename_first_match(df, "populacao", c("populacao", "pop"))
  df <- rename_first_match(df, "incidencia", c("incidencia"))
  df <- rename_first_match(df, "surtos", c("surtos", "surto"))
  df <- rename_first_match(df, "sexo_masc", c("sexo_masc", "sexomasc", "sexmasc"))
  df <- rename_first_match(df, "sexo_fem", c("sexo_fem", "sexofem", "sexfem"))
  df <- rename_first_match(df, "fx_menor1", c("fx_menor1"))
  df <- rename_first_match(df, "fx_1a4", c("fx_1a4"))
  df <- rename_first_match(df, "fx_5a9", c("fx_5a9"))
  df <- rename_first_match(df, "fx_10a14", c("fx_10a14"))
  df <- rename_first_match(df, "fx_15mais", c("fx_15mais"))
  df <- rename_first_match(df, "acao_educativa", c("acao_educativa", "nuacaoeducativa"))
  df <- rename_first_match(df, "treinamentos", c("treinamentos", "nutreinamento"))
  df <- rename_first_match(df, "coleta_material", c("coleta_material", "nucoletamaterialbio"))
  df <- rename_first_match(df, "encaminhamentos", c("encaminhamentos", "nuencamimento", "nuencaminhamento"))

  add_default_col <- function(data, col_name, default_value) {
    if (!(col_name %in% names(data))) {
      data[[col_name]] <- rep(default_value, nrow(data))
    }
    data
  }

  df <- add_default_col(df, "drs_codigo", NA_character_)
  df <- add_default_col(df, "gve_codigo", NA_character_)
  df <- add_default_col(df, "uvis", NA_character_)
  df <- add_default_col(df, "unidade_saude", "Não informado")
  df <- add_default_col(df, "populacao", NA_real_)
  df <- add_default_col(df, "incidencia", NA_real_)
  df <- add_default_col(df, "surtos", 0)
  df <- add_default_col(df, "sexo_masc", 0)
  df <- add_default_col(df, "sexo_fem", 0)
  df <- add_default_col(df, "fx_menor1", 0)
  df <- add_default_col(df, "fx_1a4", 0)
  df <- add_default_col(df, "fx_5a9", 0)
  df <- add_default_col(df, "fx_10a14", 0)
  df <- add_default_col(df, "fx_15mais", 0)
  df <- add_default_col(df, "acao_educativa", 0)
  df <- add_default_col(df, "treinamentos", 0)
  df <- add_default_col(df, "coleta_material", 0)
  df <- add_default_col(df, "encaminhamentos", 0)

  df <- df %>%
    mutate(
      ano = suppressWarnings(as.integer(ano)),
      semana = suppressWarnings(as.integer(semana)),
      semana = pmin(pmax(semana, 1), 53),
      drs = sanitize_dimension_values(drs),
      drs_codigo = sanitize_geo_code(drs_codigo),
      gve = sanitize_dimension_values(gve),
      gve_codigo = sanitize_geo_code(gve_codigo),
      municipio = sanitize_dimension_values(municipio),
      uvis = sanitize_dimension_values(uvis, default = NA_character_),
      uvis = ifelse(normalize_key(municipio) == "sao paulo", uvis, NA_character_),
      unidade_saude = sanitize_dimension_values(unidade_saude),
      casos = ifelse(is.na(casos), 0, pmax(round(sanitize_numeric_values(casos, default = 0)), 0)),
      populacao = sanitize_numeric_values(populacao, default = NA_real_),
      populacao = ifelse(is.na(populacao) | !is.finite(populacao) | populacao <= 0, NA_real_, populacao),
      surtos = ifelse(is.na(surtos), 0, pmax(round(sanitize_numeric_values(surtos, default = 0)), 0)),
      sexo_masc = ifelse(is.na(sexo_masc), 0, pmax(round(sanitize_numeric_values(sexo_masc, default = 0)), 0)),
      sexo_fem = ifelse(is.na(sexo_fem), 0, pmax(round(sanitize_numeric_values(sexo_fem, default = 0)), 0)),
      fx_menor1 = ifelse(is.na(fx_menor1), 0, pmax(round(sanitize_numeric_values(fx_menor1, default = 0)), 0)),
      fx_1a4 = ifelse(is.na(fx_1a4), 0, pmax(round(sanitize_numeric_values(fx_1a4, default = 0)), 0)),
      fx_5a9 = ifelse(is.na(fx_5a9), 0, pmax(round(sanitize_numeric_values(fx_5a9, default = 0)), 0)),
      fx_10a14 = ifelse(is.na(fx_10a14), 0, pmax(round(sanitize_numeric_values(fx_10a14, default = 0)), 0)),
      fx_15mais = ifelse(is.na(fx_15mais), 0, pmax(round(sanitize_numeric_values(fx_15mais, default = 0)), 0)),
      acao_educativa = ifelse(is.na(acao_educativa), 0, pmax(round(sanitize_numeric_values(acao_educativa, default = 0)), 0)),
      treinamentos = ifelse(is.na(treinamentos), 0, pmax(round(sanitize_numeric_values(treinamentos, default = 0)), 0)),
      coleta_material = ifelse(is.na(coleta_material), 0, pmax(round(sanitize_numeric_values(coleta_material, default = 0)), 0)),
      encaminhamentos = ifelse(is.na(encaminhamentos), 0, pmax(round(sanitize_numeric_values(encaminhamentos, default = 0)), 0))
    ) %>%
    filter(!is.na(ano), !is.na(semana))

  if (isTRUE(aggregate)) {
    df <- df %>%
      group_by(ano, semana, drs, drs_codigo, gve, gve_codigo, municipio, uvis, unidade_saude, populacao) %>%
      summarise(
        casos = sum(casos, na.rm = TRUE),
        surtos = sum(surtos, na.rm = TRUE),
        sexo_masc = sum(sexo_masc, na.rm = TRUE),
        sexo_fem = sum(sexo_fem, na.rm = TRUE),
        fx_menor1 = sum(fx_menor1, na.rm = TRUE),
        fx_1a4 = sum(fx_1a4, na.rm = TRUE),
        fx_5a9 = sum(fx_5a9, na.rm = TRUE),
        fx_10a14 = sum(fx_10a14, na.rm = TRUE),
        fx_15mais = sum(fx_15mais, na.rm = TRUE),
        acao_educativa = sum(acao_educativa, na.rm = TRUE),
        treinamentos = sum(treinamentos, na.rm = TRUE),
        coleta_material = sum(coleta_material, na.rm = TRUE),
        encaminhamentos = sum(encaminhamentos, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        incidencia = ifelse(populacao > 0, round((casos / populacao) * 100000, 2), NA_real_)
      )
  } else {
    df <- df %>%
      mutate(
        incidencia = ifelse(
          !is.finite(incidencia) | is.na(incidencia),
          ifelse(populacao > 0, round((casos / populacao) * 100000, 2), NA_real_),
          incidencia
        )
      )
  }

  df <- df %>%
    arrange(ano, semana, drs, gve, municipio, uvis, unidade_saude)

  if (nrow(df) == 0) {
    return(NULL)
  }

  df
}

load_oftalmo_json_data <- function(json_path = "", json_url = "") {
  json_path <- trimws(as.character(json_path))
  json_url <- trimws(as.character(json_url))

  if (json_path == "" && json_url == "") {
    return(NULL)
  }

  ensure_json_dependency()

  payload <- NULL
  source_used <- NULL

  if (json_path != "") {
    if (!file.exists(json_path)) {
      stop(paste0("Arquivo JSON não encontrado: ", json_path))
    }
    payload <- jsonlite::fromJSON(json_path, simplifyDataFrame = TRUE, flatten = TRUE)
    source_used <- json_path
  } else if (json_url != "") {
    payload <- jsonlite::fromJSON(json_url, simplifyDataFrame = TRUE, flatten = TRUE)
    source_used <- json_url
  }

  extract_records <- function(x) {
    if (is.data.frame(x)) {
      return(x)
    }
    if (is.list(x)) {
      for (nm in c("data", "records", "dados", "items")) {
        if (!is.null(x[[nm]])) {
          return(x[[nm]])
        }
      }
    }
    x
  }

  records <- extract_records(payload)
  if (is.null(records)) {
    return(NULL)
  }

  if (is.data.frame(records)) {
    raw_df <- records
  } else {
    raw_df <- tryCatch(
      as.data.frame(records, stringsAsFactors = FALSE),
      error = function(e) NULL
    )
  }

  if (is.null(raw_df) || nrow(raw_df) == 0) {
    return(NULL)
  }

  is_exported_snapshot <- is.list(payload) &&
    !is.null(payload$schema_version) &&
    !is.null(payload$data)

  prepared <- if (is_internal_oftalmo_dataset(raw_df)) {
    normalize_internal_oftalmo_dataset(raw_df, aggregate = !is_exported_snapshot)
  } else {
    prepare_oftalmo_dataset(raw_df)
  }

  recent_years <- suppressWarnings(as.integer(Sys.getenv("OFTALMO_JSON_RECENT_YEARS", unset = "0")))
  if (
    !is.null(prepared) &&
      nrow(prepared) > 0 &&
      is.finite(recent_years) &&
      recent_years > 0 &&
      "ano" %in% names(prepared)
  ) {
    anos_disponiveis <- sort(unique(prepared$ano), decreasing = TRUE)
    anos_manter <- utils::head(anos_disponiveis, recent_years)
    prepared <- prepared %>% filter(ano %in% anos_manter)
  }

  if (!is.null(prepared)) {
    attr(prepared, "json_source") <- source_used
  }

  prepared
}

load_epidemiological_data <- function(
  prefer_db = TRUE,
  prefer_json = FALSE,
  enable_json = TRUE,
  json_path = "",
  json_url = ""
) {
  db_error <- NULL
  db_empty <- FALSE
  json_error <- NULL
  json_empty <- FALSE

  json_path <- trimws(as.character(json_path))
  json_url <- trimws(as.character(json_url))
  has_json_source <- json_path != "" || json_url != ""

  if (prefer_json && !has_json_source) {
    json_error <- "Modo JSON ativo, mas OFTALMO_JSON_PATH/OFTALMO_JSON_URL não foram configurados."
  }

  try_load_db <- function() {
    if (!prefer_db || !exists("load_oftalmo_raw_data", mode = "function")) {
      return(NULL)
    }

    db_data <- tryCatch(
      {
        raw <- load_oftalmo_raw_data()
        prepared <- prepare_oftalmo_dataset(raw)
        if (is.null(prepared) || nrow(prepared) == 0) {
          db_empty <<- TRUE
        }
        prepared
      },
      error = function(e) {
        db_error <<- conditionMessage(e)
        NULL
      }
    )

    if (!is.null(db_data) && nrow(db_data) > 0) {
      attr(db_data, "data_source") <- "database"
      return(db_data)
    }

    NULL
  }

  try_load_json <- function() {
    if (!enable_json || !has_json_source) {
      return(NULL)
    }

    json_data <- tryCatch(
      {
        prepared <- load_oftalmo_json_data(json_path = json_path, json_url = json_url)
        if (is.null(prepared) || nrow(prepared) == 0) {
          json_empty <<- TRUE
        }
        prepared
      },
      error = function(e) {
        json_error <<- conditionMessage(e)
        NULL
      }
    )

    if (!is.null(json_data) && nrow(json_data) > 0) {
      attr(json_data, "data_source") <- "json"
      return(json_data)
    }

    NULL
  }

  source_order <- if (prefer_json) c("json", "db") else c("db", "json")
  for (source_name in source_order) {
    data_loaded <- if (identical(source_name, "db")) {
      try_load_db()
    } else {
      try_load_json()
    }

    if (!is.null(data_loaded) && nrow(data_loaded) > 0) {
      return(data_loaded)
    }
  }

  reasons <- character()

  if (prefer_db) {
    db_reason <- if (!is.null(db_error)) {
      paste("Banco: erro na conexão/consulta -", db_error)
    } else if (db_empty) {
      "Banco: consulta executada, mas sem dados válidos para o painel."
    } else if (!exists("load_oftalmo_raw_data", mode = "function")) {
      "Banco: função load_oftalmo_raw_data não encontrada."
    } else {
      "Banco: sem retorno de dados."
    }
    reasons <- c(reasons, db_reason)
  }

  if (enable_json) {
    json_reason <- if (!has_json_source) {
      "JSON: OFTALMO_JSON_PATH/OFTALMO_JSON_URL não configurados."
    } else if (!is.null(json_error)) {
      paste("JSON: erro na leitura/processamento -", json_error)
    } else if (json_empty) {
      "JSON: arquivo/url lido, mas sem dados válidos para o painel."
    } else {
      "JSON: sem retorno de dados."
    }
    reasons <- c(reasons, json_reason)
  }

  if (length(reasons) == 0) {
    reasons <- "Nenhuma fonte de dados foi habilitada (DB/JSON)."
  }

  stop(
    paste(
      "Não foi possível carregar dados para o painel.",
      paste(reasons, collapse = " "),
      "Não há fallback para dados fictícios."
    )
  )
}

# ---------------------------------------------------------------------------
# Calendário epidemiológico
# ---------------------------------------------------------------------------

# A SE 1 começa no domingo da semana que contém pelo menos 4 dias de janeiro.
inicio_ano_epidemiologico <- function(ano) {
  jan1 <- as.Date(sprintf("%04d-01-01", as.integer(ano)))
  dia_semana <- as.integer(format(jan1, "%w"))
  jan1 + ifelse(dia_semana <= 3, -dia_semana, 7 - dia_semana)
}

semanas_no_ano_epidemiologico <- function(ano) {
  as.integer((inicio_ano_epidemiologico(as.integer(ano) + 1) - inicio_ano_epidemiologico(ano)) / 7)
}

semana_epidemiologica_da_data <- function(data = Sys.Date()) {
  data <- as.Date(data)
  ano <- as.integer(format(data, "%Y"))
  ano <- ifelse(data < inicio_ano_epidemiologico(ano), ano - 1L, ano)
  ano <- ifelse(data >= inicio_ano_epidemiologico(ano + 1L), ano + 1L, ano)
  semana <- as.integer(as.numeric(data - inicio_ano_epidemiologico(ano)) %/% 7) + 1L
  list(ano = ano, semana = semana)
}

# ---------------------------------------------------------------------------
# População IBGE por município e ano
# ---------------------------------------------------------------------------

# Grafias usadas na base de notificação que diferem do nome oficial do IBGE.
municipio_aliases <- c(
  "sao luis do paraitinga" = "sao luiz do paraitinga",
  "embu" = "embu das artes",
  "moji mirim" = "mogi mirim",
  "moji guacu" = "mogi guacu",
  "moji das cruzes" = "mogi das cruzes",
  "ipaucu" = "ipaussu",
  "brodosqui" = "brodowski",
  "florinia" = "florinea",
  "santa barbara doeste" = "santa barbara d oeste"
)

populacao_ibge_path <- function() {
  path <- trimws(Sys.getenv("OFTALMO_POPULACAO_CSV", unset = ""))
  if (path == "") {
    path <- file.path("data", "populacao_ibge_sp.csv")
  }
  path
}

load_populacao_ibge <- local({
  cached <- NULL

  function() {
    if (!is.null(cached)) {
      return(cached)
    }

    path <- populacao_ibge_path()
    if (!file.exists(path)) {
      stop(
        "Tabela de população IBGE não encontrada em '", path, "'. ",
        "Configure OFTALMO_POPULACAO_CSV ou restaure data/populacao_ibge_sp.csv.",
        call. = FALSE
      )
    }

    pop <- utils::read.csv(
      path,
      stringsAsFactors = FALSE,
      encoding = "UTF-8",
      colClasses = c(cod_ibge = "character")
    )

    cached <<- tibble::as_tibble(pop) %>%
      mutate(
        ano = as.integer(ano),
        populacao = as.numeric(populacao),
        municipio_chave = normalize_key(municipio)
      )
    cached
  }
})

normalize_municipio_chave <- function(municipio) {
  chave <- normalize_key(municipio)
  idx <- match(chave, names(municipio_aliases))
  chave[!is.na(idx)] <- unname(municipio_aliases[idx[!is.na(idx)]])
  chave
}

normalize_cod_ibge <- function(codigo, referencia) {
  codigo <- gsub("\\D", "", as.character(codigo))
  codigo[is.na(codigo) | !(nchar(codigo) %in% c(6, 7)) | !startsWith(codigo, "35")] <- NA_character_

  # Códigos de 6 dígitos (sem o dígito verificador) são completados pela tabela IBGE.
  seis <- !is.na(codigo) & nchar(codigo) == 6
  if (any(seis)) {
    ref_seis <- substr(referencia, 1, 6)
    codigo[seis] <- referencia[match(codigo[seis], ref_seis)]
  }
  codigo[!is.na(codigo) & !(codigo %in% referencia)] <- NA_character_
  codigo
}

# Substitui a população vinda do banco pela população IBGE do município no ano.
# Anos fora da série (ex.: ano corrente ainda sem estimativa) usam o ano mais próximo.
anexar_populacao_ibge <- function(df) {
  if (is.null(df) || nrow(df) == 0) {
    return(df)
  }

  pop <- load_populacao_ibge()
  ref_municipios <- pop %>%
    distinct(cod_ibge, municipio, municipio_chave)

  if (!("cod_ibge" %in% names(df))) {
    df$cod_ibge <- NA_character_
  }

  df <- df %>%
    select(-any_of(c("populacao", "incidencia", "populacao_ano_ref"))) %>%
    mutate(
      cod_ibge = normalize_cod_ibge(cod_ibge, ref_municipios$cod_ibge),
      cod_ibge = dplyr::coalesce(
        cod_ibge,
        ref_municipios$cod_ibge[match(normalize_municipio_chave(municipio), ref_municipios$municipio_chave)]
      ),
      municipio = dplyr::coalesce(
        ref_municipios$municipio[match(cod_ibge, ref_municipios$cod_ibge)],
        municipio
      )
    )

  anos_pop <- sort(unique(pop$ano))
  df <- df %>%
    mutate(
      populacao_ano_ref = pmin(pmax(ano, min(anos_pop)), max(anos_pop))
    ) %>%
    left_join(
      pop %>% select(cod_ibge, populacao_ano_ref = ano, populacao),
      by = c("cod_ibge", "populacao_ano_ref")
    ) %>%
    mutate(
      incidencia = ifelse(populacao > 0, round((casos / populacao) * 100000, 2), NA_real_)
    )

  sem_codigo <- sort(unique(df$municipio[is.na(df$cod_ibge) & df$municipio != "Não informado"]))
  if (length(sem_codigo) > 0) {
    warning(
      "Municípios sem correspondência na tabela IBGE (sem população): ",
      paste(sem_codigo, collapse = ", "),
      call. = FALSE
    )
  }
  attr(df, "municipios_sem_ibge") <- sem_codigo

  df
}

# ---------------------------------------------------------------------------
# Carga dos dados (cache compartilhado entre sessões)
# ---------------------------------------------------------------------------

dataset_vazio <- function() {
  tibble::tibble(
    ano = integer(), semana = integer(), drs = character(), drs_codigo = character(),
    gve = character(), gve_codigo = character(), municipio = character(), cod_ibge = character(),
    uvis = character(), unidade_saude = character(), casos = numeric(), surtos = numeric(),
    sexo_masc = numeric(), sexo_fem = numeric(), fx_menor1 = numeric(), fx_1a4 = numeric(),
    fx_5a9 = numeric(), fx_10a14 = numeric(), fx_15mais = numeric(), acao_educativa = numeric(),
    treinamentos = numeric(), coleta_material = numeric(), encaminhamentos = numeric(),
    populacao_ano_ref = integer(), populacao = numeric(), incidencia = numeric()
  )
}

# Usa os nomes oficiais de GVE e DRS (com acentos) e a DRS de cada GVE da malha
# estadual, corrigindo códigos de DRS inconsistentes na base.
padronizar_regionais <- function(df) {
  if (is.null(df) || nrow(df) == 0) {
    return(df)
  }
  path <- file.path("data", "regionais_sp.csv")
  if (!file.exists(path)) {
    return(df)
  }
  ref <- utils::read.csv(path, stringsAsFactors = FALSE, encoding = "UTF-8")
  ref$chave <- normalize_shape_key(ref$gve, level = "gve")

  idx <- match(normalize_shape_key(df$gve, level = "gve"), ref$chave)
  df$gve <- ifelse(is.na(idx), df$gve, ref$gve[idx])
  df$drs <- ifelse(is.na(idx), df$drs, ref$drs[idx])
  df
}

carregar_dados_painel <- function() {
  dados <- load_epidemiological_data(
    prefer_db = TRUE,
    prefer_json = FALSE,
    enable_json = FALSE,
    json_path = "",
    json_url = ""
  )
  dados %>%
    padronizar_regionais() %>%
    anexar_populacao_ibge()
}

# Uma única consulta ao banco por intervalo, mesmo com várias sessões abertas.
dataset_cache <- new.env(parent = emptyenv())

obter_dados_painel <- function(max_idade_segundos = 0) {
  carregado_em <- dataset_cache$carregado_em
  cache_valido <- !is.null(dataset_cache$dados) &&
    !is.null(carregado_em) &&
    difftime(Sys.time(), carregado_em, units = "secs") < max_idade_segundos

  if (!cache_valido) {
    resultado <- tryCatch(
      list(dados = carregar_dados_painel(), erro = NULL),
      error = function(e) list(dados = NULL, erro = conditionMessage(e))
    )

    if (!is.null(resultado$dados) && nrow(resultado$dados) > 0) {
      dataset_cache$dados <- resultado$dados
      dataset_cache$carregado_em <- Sys.time()
      dataset_cache$erro <- NULL
    } else {
      dataset_cache$erro <- if (is.null(resultado$erro)) "Consulta sem dados." else resultado$erro
      if (is.null(dataset_cache$dados)) {
        dataset_cache$dados <- dataset_vazio()
      }
      if (is.null(dataset_cache$carregado_em)) {
        dataset_cache$carregado_em <- Sys.time()
      }
    }
  }

  list(
    dados = dataset_cache$dados,
    carregado_em = dataset_cache$carregado_em,
    erro = dataset_cache$erro
  )
}

carga_inicial <- obter_dados_painel()
dados_epidemiologicos <- carga_inicial$dados
dados_fonte <- "database"
if (!is.null(carga_inicial$erro)) {
  warning("Falha ao carregar dados do painel: ", carga_inicial$erro, call. = FALSE)
}
