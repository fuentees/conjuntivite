library(dplyr)
library(lubridate)
library(tidyr)

if (file.exists("functions/ConectDb.R")) {
  tryCatch(
    source("functions/ConectDb.R"),
    error = function(e) NULL
  )
}

normalize_key <- function(x) {
  x <- trimws(as.character(x))
  x <- ifelse(is.na(x), "", x)
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x <- tolower(gsub("[^a-z0-9]+", " ", x))
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

shape_file_envvars <- c(
  municipio = "MAPA_SHAPE_MUNICIPIO",
  gve = "MAPA_SHAPE_GVE",
  drs = "MAPA_SHAPE_DRS"
)

shape_default_paths <- c(
  municipio = file.path("shapes", "municipio", "municipio.shp"),
  gve = file.path("shapes", "gve", "gve.shp"),
  drs = file.path("shapes", "drs", "drs.shp")
)

shape_level_tokens <- function(level = c("municipio", "gve", "drs")) {
  level <- match.arg(level)

  switch(
    level,
    municipio = c("municipio", "muni", "ibge", "cidade"),
    gve = c("gve", "grupo", "vigilancia"),
    drs = c("drs", "diretoria", "regional", "saude")
  )
}

find_shape_file <- function(level = c("municipio", "gve", "drs")) {
  level <- match.arg(level)
  env_name <- unname(shape_file_envvars[level])
  configured_path <- trimws(Sys.getenv(env_name, unset = ""))
  default_path <- unname(shape_default_paths[level])
  default_dir <- dirname(default_path)

  normalize_existing_path <- function(path) {
    if (is.null(path) || path == "" || !file.exists(path)) {
      return(path)
    }
    normalizePath(path, winslash = "/", mustWork = FALSE)
  }

  collect_shape_files <- function(path_candidate, recursive = FALSE) {
    if (is.null(path_candidate) || path_candidate == "") {
      return(character())
    }

    path_candidate <- trimws(path_candidate)
    if (path_candidate == "") {
      return(character())
    }

    if (file.exists(path_candidate) &&
      !dir.exists(path_candidate) &&
      grepl("\\.shp$", path_candidate, ignore.case = TRUE)) {
      return(path_candidate)
    }

    if (!file.exists(path_candidate) && !grepl("\\.shp$", path_candidate, ignore.case = TRUE)) {
      with_ext <- paste0(path_candidate, ".shp")
      if (file.exists(with_ext)) {
        return(with_ext)
      }
    }

    if (dir.exists(path_candidate)) {
      return(
        list.files(
          path = path_candidate,
          pattern = "\\.shp$",
          full.names = TRUE,
          recursive = recursive,
          ignore.case = TRUE
        )
      )
    }

    character()
  }

  candidate_files <- character()
  candidate_files <- c(candidate_files, collect_shape_files(configured_path, recursive = TRUE))
  candidate_files <- c(candidate_files, collect_shape_files(default_path, recursive = FALSE))
  candidate_files <- c(candidate_files, collect_shape_files(default_dir, recursive = FALSE))

  search_roots <- c(
    file.path("shapes", level),
    file.path("shapes", toupper(level)),
    file.path("shapes", tools::toTitleCase(level)),
    "shapes",
    file.path("mapas", level),
    "mapas",
    "."
  )

  for (root in unique(search_roots)) {
    candidate_files <- c(candidate_files, collect_shape_files(root, recursive = root != "."))
  }

  candidate_files <- unique(candidate_files[file.exists(candidate_files)])
  if (length(candidate_files) == 0) {
    return(NULL)
  }

  tokens <- shape_level_tokens(level)
  configured_norm <- normalize_existing_path(configured_path)
  default_norm <- normalize_existing_path(default_path)
  default_dir_norm <- normalize_existing_path(default_dir)

  has_token <- function(text_value) {
    text_value <- normalize_key(text_value)
    any(vapply(tokens, function(tok) grepl(tok, text_value, fixed = TRUE), logical(1)))
  }

  score_shape_file <- function(path) {
    path_norm <- normalize_existing_path(path)
    base_name <- tools::file_path_sans_ext(basename(path))
    dir_name <- dirname(path)

    score <- 0

    if (!is.null(configured_norm) && configured_norm != "") {
      if (!dir.exists(configured_path) && identical(path_norm, configured_norm)) {
        score <- score + 1000
      } else if (dir.exists(configured_path)) {
        configured_dir_norm <- normalize_existing_path(configured_path)
        if (!is.null(configured_dir_norm) && configured_dir_norm != "" &&
          startsWith(path_norm, configured_dir_norm)) {
          score <- score + 700
        }
      }
    }

    if (!is.null(default_norm) && default_norm != "" && identical(path_norm, default_norm)) {
      score <- score + 500
    }

    if (!is.null(default_dir_norm) && default_dir_norm != "" && startsWith(path_norm, default_dir_norm)) {
      score <- score + 120
    }

    if (has_token(base_name)) {
      score <- score + 250
    }

    if (has_token(dir_name)) {
      score <- score + 120
    }

    score - (nchar(path_norm) / 1000)
  }

  scores <- vapply(candidate_files, score_shape_file, numeric(1))
  candidate_files[which.max(scores)]
}

get_shape_candidate_columns <- function(level = c("municipio", "gve", "drs")) {
  level <- match.arg(level)

  switch(
    level,
    municipio = c(
      "municipio", "municipio_nome", "municipio_nm", "nome_municipio",
      "name_muni", "nm_mun", "nm_municip", "nm_municipio",
      "cd_mun", "cod_mun", "cod_ibge", "codigo_ibge", "nome", "name"
    ),
    gve = c(
      "gve", "dominio_gve", "nome_gve", "nm_gve", "grupo_vigilancia",
      "grupo_vig", "gve_numero", "cod_gve", "codigo_gve", "id_gve",
      "cd_gve", "codmacro", "cod_macro", "codmacrogve", "cod_macro_gve",
      "name", "nome"
    ),
    drs = c(
      "drs", "nome_drs", "nm_drs", "diretoria_regional",
      "regional_saude", "drs_numero", "cod_drs", "codigo_drs", "id_drs",
      "cd_drs", "name", "nome"
    )
  )
}

guess_shape_key_column <- function(shp, level = c("municipio", "gve", "drs")) {
  level <- match.arg(level)
  candidates <- get_shape_candidate_columns(level)

  cols <- names(shp)
  cols_lower <- tolower(cols)
  idx <- match(tolower(candidates), cols_lower)
  idx <- idx[!is.na(idx)]

  if (length(idx) > 0) {
    return(cols[idx[1]])
  }

  # Fallback para primeira coluna textual quando o nome da chave não for conhecido.
  char_cols <- cols[vapply(shp, function(x) is.character(x) || is.factor(x), logical(1))]
  if (length(char_cols) > 0) {
    return(char_cols[1])
  }

  # Fallback para primeira coluna não-geométrica (inclui códigos numéricos).
  non_geom_cols <- cols[!vapply(shp, function(x) inherits(x, "sfc"), logical(1))]
  if (length(non_geom_cols) > 0) {
    return(non_geom_cols[1])
  }

  NULL
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

    if (!requireNamespace("sf", quietly = TRUE)) {
      return(NULL)
    }

    shp_path <- find_shape_file(level)
    if (is.null(shp_path)) {
      return(NULL)
    }

    file_info <- file.info(shp_path)
    cache_key <- paste(level, shp_path, as.character(file_info$mtime), sep = "::")

    if (exists(cache_key, envir = cache, inherits = FALSE)) {
      return(get(cache_key, envir = cache, inherits = FALSE))
    }

    shp <- tryCatch(
      {
        sf::st_read(
          dsn = shp_path,
          quiet = TRUE,
          stringsAsFactors = FALSE,
          options = "ENCODING=UTF-8"
        )
      },
      error = function(e) NULL
    )

    if (is.null(shp) || nrow(shp) == 0) {
      return(NULL)
    }

    key_col <- guess_shape_key_column(shp, level)
    if (is.null(key_col)) {
      return(NULL)
    }

    candidate_cols <- get_shape_candidate_columns(level)
    shp_cols <- names(shp)
    shp_cols_lower <- tolower(shp_cols)
    idx_candidates <- match(tolower(candidate_cols), shp_cols_lower)
    idx_candidates <- idx_candidates[!is.na(idx_candidates)]
    candidate_matches <- unique(shp_cols[idx_candidates])
    candidate_matches <- setdiff(candidate_matches, key_col)

    alt_key_col <- NULL
    if (length(candidate_matches) > 0) {
      alt_key_col <- candidate_matches[1]
    } else {
      char_cols <- shp_cols[
        vapply(shp, function(x) is.character(x) || is.factor(x), logical(1)) &
          shp_cols != key_col
      ]
      if (length(char_cols) > 0) {
        alt_key_col <- char_cols[1]
      }
    }

    shp <- shp %>%
      mutate(
        map_key = normalize_shape_key(.data[[key_col]], level = level),
        map_key_alt = if (!is.null(alt_key_col)) normalize_shape_key(.data[[alt_key_col]], level = level) else NA_character_,
        map_label = as.character(.data[[key_col]])
      )

    attr(shp, "shape_level") <- level
    attr(shp, "shape_path") <- shp_path
    attr(shp, "shape_key_column") <- key_col
    attr(shp, "shape_key_alt_column") <- alt_key_col

    assign(cache_key, shp, envir = cache)
    shp
  }
})

parse_notification_date <- function(value) {
  value <- as.character(value)
  parsed <- suppressWarnings(
    parse_date_time(
      value,
      orders = c(
        "d/m/Y H:M:S", "d/m/Y H:M", "d/m/Y",
        "d/m/y H:M:S", "d/m/y H:M", "d/m/y",
        "Y-m-d H:M:S", "Y-m-d H:M", "Y-m-d",
        "Y/m/d H:M:S", "Y/m/d H:M", "Y/m/d",
        "d-m-Y H:M:S", "d-m-Y H:M", "d-m-Y",
        "m/d/Y H:M:S", "m/d/Y H:M", "m/d/Y"
      ),
      quiet = TRUE
    )
  )

  as.Date(parsed)
}

prepare_oftalmo_dataset <- function(raw_data) {
  if (is.null(raw_data) || nrow(raw_data) == 0) {
    return(NULL)
  }

  date_col <- pick_column_with_override(
    raw_data,
    override_env_var = "OFTALMO_COL_DATA",
    candidates = c("DtNotificacao", "dt_notificacao", "data_notificacao", "created_at", "DtUpdate"),
    regex_candidates = c("^dt.*not", "^data.*not")
  )
  ano_col <- pick_column_with_override(
    raw_data,
    override_env_var = "OFTALMO_COL_ANO",
    candidates = c("ANO", "ano", "Ano", "AnoNotificacao", "ano_notificacao"),
    regex_candidates = c("^ano$", "ano.*not", "year")
  )
  semana_col <- pick_column_with_override(
    raw_data,
    override_env_var = "OFTALMO_COL_SEMANA",
    candidates = c("SemEpidemio", "semepidemio", "semana", "Semana", "SE", "SemanaEpidemiologica", "semana_epidemiologica"),
    regex_candidates = c("^semana$", "semana.*epi", "^se$", "semepid")
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
  populacao_col <- pick_column_with_override(
    raw_data,
    override_env_var = "OFTALMO_COL_POPULACAO",
    candidates = c("Populacao", "populacao", "pop", "population", "Pop"),
    regex_candidates = c("popul")
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
        "Se necessário, configure OFTALMO_COL_GVE/OFTALMO_COL_MUNICIPIO/OFTALMO_COL_CASOS."
      )
    )
  }

  df <- raw_data %>%
    mutate(
      data_notificacao = if (!is.null(date_col)) parse_notification_date(.data[[date_col]]) else as.Date(NA),
      ano_registro = if (!is.null(ano_col)) suppressWarnings(as.integer(.data[[ano_col]])) else NA_integer_,
      semana_registro = if (!is.null(semana_col)) suppressWarnings(as.integer(.data[[semana_col]])) else NA_integer_,
      municipio = sanitize_dimension_values(.data[[municipio_col]]),
      gve = sanitize_dimension_values(.data[[gve_col]]),
      gve_codigo = if (!is.null(gve_code_col)) sanitize_geo_code(.data[[gve_code_col]]) else NA_character_,
      uvis = if (!is.null(uvis_col)) sanitize_dimension_values(.data[[uvis_col]], default = NA_character_) else NA_character_,
      uvis = ifelse(normalize_key(municipio) == "sao paulo", uvis, NA_character_),
      drs = if (!is.null(drs_col)) sanitize_dimension_values(.data[[drs_col]]) else "Não informado",
      drs_codigo = if (!is.null(drs_code_col)) sanitize_geo_code(.data[[drs_code_col]]) else NA_character_,
      drs = ifelse(
        drs == "Não informado",
        sanitize_dimension_values(infer_drs_from_gve_text(gve), default = "Não informado"),
        drs
      ),
      casos = sanitize_numeric_values(.data[[casos_col]], default = 0),
      casos = ifelse(is.na(casos), 0, pmax(round(casos), 0)),
      populacao = if (!is.null(populacao_col)) sanitize_numeric_values(.data[[populacao_col]], default = NA_real_) else NA_real_,
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
      ano = dplyr::coalesce(ano_registro, year(data_notificacao)),
      semana = dplyr::coalesce(semana_registro, isoweek(data_notificacao)),
      semana = pmin(pmax(semana, 1), 53),
      populacao = ifelse(!is.finite(populacao) | populacao <= 0, NA_real_, populacao),
      surtos = surto_n + surto_flag,
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
    group_by(ano, semana, drs, drs_codigo, gve, gve_codigo, municipio, uvis, populacao) %>%
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
    ) %>%
    arrange(ano, semana, drs, gve, municipio, uvis)

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
      group_by(ano, semana, drs, drs_codigo, gve, gve_codigo, municipio, uvis, populacao) %>%
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
    arrange(ano, semana, drs, gve, municipio, uvis)

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

data_mode <- tolower(trimws(Sys.getenv("OFTALMO_DATA_MODE", unset = "auto")))
if (!(data_mode %in% c("auto", "db", "json"))) {
  data_mode <- "auto"
}

json_path <- trimws(Sys.getenv("OFTALMO_JSON_PATH", unset = ""))
json_url <- trimws(Sys.getenv("OFTALMO_JSON_URL", unset = ""))

dados_epidemiologicos <- load_epidemiological_data(
  prefer_db = data_mode %in% c("auto", "db"),
  prefer_json = data_mode == "json",
  enable_json = data_mode %in% c("auto", "json"),
  json_path = json_path,
  json_url = json_url
)
dados_fonte <- attr(dados_epidemiologicos, "data_source")
if (is.null(dados_fonte) || dados_fonte == "") {
  dados_fonte <- if (data_mode == "json") {
    "json"
  } else if (data_mode == "db") {
    "database"
  } else {
    "auto"
  }
}

get_gve_by_drs <- function(drs) {
  sort(unique(dados_epidemiologicos$gve[dados_epidemiologicos$drs == drs]))
}

get_municipios_by_gve <- function(gve) {
  sort(unique(dados_epidemiologicos$municipio[dados_epidemiologicos$gve == gve]))
}

get_municipio_coords <- function(municipios) {
  base <- data.frame(municipio = unique(as.character(municipios)), stringsAsFactors = FALSE)
  coords <- base %>%
    mutate(
      lat = NA_real_,
      lng = NA_real_
    )

  if (!requireNamespace("geobr", quietly = TRUE) || !requireNamespace("sf", quietly = TRUE)) {
    return(coords)
  }

  muni_sf <- tryCatch(
    geobr::read_municipality(
      code_muni = "SP",
      year = 2020,
      simplified = TRUE,
      showProgress = FALSE
    ),
    error = function(e) NULL
  )

  if (is.null(muni_sf) || nrow(muni_sf) == 0) {
    return(coords)
  }

  centroids <- tryCatch(
    sf::st_point_on_surface(muni_sf),
    error = function(e) NULL
  )

  if (is.null(centroids)) {
    return(coords)
  }

  xy <- sf::st_coordinates(centroids)
  geobr_coords <- muni_sf %>%
    mutate(
      municipio_norm = normalize_key(name_muni),
      lat = as.numeric(xy[, "Y"]),
      lng = as.numeric(xy[, "X"])
    ) %>%
    sf::st_drop_geometry() %>%
    select(municipio_norm, lat, lng)

  coords %>%
    mutate(municipio_norm = normalize_key(municipio)) %>%
    left_join(geobr_coords, by = "municipio_norm", suffix = c("", "_geobr")) %>%
    transmute(
      municipio,
      lat = lat_geobr,
      lng = lng_geobr
    )
}

get_sp_municipality_shapes <- local({
  cached_shapes <- NULL

  function() {
    if (!is.null(cached_shapes)) {
      return(cached_shapes)
    }

    if (!requireNamespace("geobr", quietly = TRUE) || !requireNamespace("sf", quietly = TRUE)) {
      return(NULL)
    }

    cached_shapes <<- tryCatch(
      {
        shp <- geobr::read_municipality(
          code_muni = "SP",
          year = 2020,
          simplified = TRUE,
          showProgress = FALSE
        )

        shp %>%
          mutate(municipio_norm = normalize_key(name_muni))
      },
      error = function(e) NULL
    )

    cached_shapes
  }
})
