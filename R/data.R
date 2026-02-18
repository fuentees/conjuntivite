library(dplyr)
library(lubridate)
library(tidyr)

if (file.exists("functions/ConectDb.R")) {
  tryCatch(
    source("functions/ConectDb.R"),
    error = function(e) NULL
  )
}

set.seed(42)

drs_list <- c(
  "DRS Ribeirão Preto",
  "DRS Campinas",
  "DRS São Paulo",
  "DRS Bauru",
  "DRS Araçatuba"
)

drs_gve_mapping <- list(
  "DRS Ribeirão Preto" = c("GVE Ribeirão Preto", "GVE Araraquara", "GVE São Carlos"),
  "DRS Campinas" = c("GVE Campinas", "GVE Piracicaba", "GVE Sorocaba"),
  "DRS São Paulo" = c("GVE Santo Amaro", "GVE Vila Mariana", "GVE Pinheiros"),
  "DRS Bauru" = c("GVE Bauru", "GVE Avaré", "GVE Jaú"),
  "DRS Araçatuba" = c("GVE Araçatuba", "GVE Birigui", "GVE Andradina")
)

gve_municipio_mapping <- list(
  "GVE Ribeirão Preto" = c("Ribeirão Preto", "Cravinhos", "Serrana"),
  "GVE Araraquara" = c("Araraquara", "Boa Esperança", "Matão"),
  "GVE São Carlos" = c("São Carlos", "Ibaté", "Brotas"),
  "GVE Campinas" = c("Campinas", "Valinhos", "Vinhedo"),
  "GVE Piracicaba" = c("Piracicaba", "Santa Maria da Serra", "Limeira"),
  "GVE Sorocaba" = c("Sorocaba", "Salto", "Boituva"),
  "GVE Santo Amaro" = c("São Paulo", "Santo André", "São Bernardo"),
  "GVE Vila Mariana" = c("São Paulo", "Taboão da Serra", "Embu-Guaçu"),
  "GVE Pinheiros" = c("São Paulo", "Osasco", "Diadema"),
  "GVE Bauru" = c("Bauru", "Agudos", "Pederneiras"),
  "GVE Avaré" = c("Avaré", "Arandu", "Itatinga"),
  "GVE Jaú" = c("Jaú", "Mineiros do Tietê", "Barra Bonita"),
  "GVE Araçatuba" = c("Araçatuba", "Castilho", "Guzolândia"),
  "GVE Birigui" = c("Birigui", "Penápolis", "Luiziânia"),
  "GVE Andradina" = c("Andradina", "Lavínia", "Murutinga do Sul")
)

known_gves <- unlist(drs_gve_mapping, use.names = FALSE)
gve_drs_lookup <- setNames(rep(names(drs_gve_mapping), lengths(drs_gve_mapping)), known_gves)
known_drs <- unique(names(drs_gve_mapping))

municipio_populacao <- c(
  "São Paulo" = 11451999,
  "Campinas" = 1207609,
  "Ribeirão Preto" = 720116,
  "Sorocaba" = 723574,
  "Santo André" = 748919,
  "São Bernardo" = 849874,
  "Osasco" = 742593,
  "Piracicaba" = 429905,
  "Bauru" = 381416,
  "Araçatuba" = 198129,
  "Araraquara" = 242228,
  "São Carlos" = 256915,
  "Limeira" = 310783,
  "Jaú" = 151881,
  "Andradina" = 58000,
  "Birigui" = 124883,
  "Diadema" = 429550,
  "Taboão da Serra" = 293652,
  "Valinhos" = 131210,
  "Vinhedo" = 82240
)

normalize_key <- function(x) {
  x <- trimws(as.character(x))
  x <- ifelse(is.na(x), "", x)
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x <- tolower(gsub("[^a-z0-9]+", " ", x))
  trimws(x)
}

known_gve_lookup <- setNames(known_gves, normalize_key(known_gves))
known_drs_lookup <- setNames(known_drs, normalize_key(known_drs))
known_municipios <- unique(unlist(gve_municipio_mapping, use.names = FALSE))
known_municipio_lookup <- setNames(known_municipios, normalize_key(known_municipios))

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
  key <- normalize_key(gve_raw)

  result <- known_gve_lookup[key]
  result <- as.character(result)

  missing <- is.na(result) | result == ""
  if (any(missing)) {
    fallback <- trimws(gve_raw[missing])
    fallback[fallback == "" | is.na(fallback)] <- "Não informado"
    needs_prefix <- !grepl("^gve\\b", tolower(fallback))
    fallback[needs_prefix] <- paste("GVE", fallback[needs_prefix])
    result[missing] <- fallback
  }

  result
}

standardize_drs <- function(drs_raw) {
  drs_raw <- as.character(drs_raw)
  key <- normalize_key(drs_raw)

  result <- known_drs_lookup[key]
  result <- as.character(result)

  missing <- is.na(result) | result == ""
  if (any(missing)) {
    fallback <- trimws(drs_raw[missing])
    fallback[fallback == "" | is.na(fallback)] <- "Não informado"
    needs_prefix <- !grepl("^drs\\b", tolower(fallback))
    fallback[needs_prefix] <- paste("DRS", fallback[needs_prefix])
    result[missing] <- fallback
  }

  result
}

standardize_municipio <- function(municipio_raw) {
  municipio_raw <- as.character(municipio_raw)
  key <- normalize_key(municipio_raw)

  result <- known_municipio_lookup[key]
  result <- as.character(result)

  missing <- is.na(result) | result == ""
  if (any(missing)) {
    result[missing] <- title_case_safe(municipio_raw[missing])
  }

  result
}

infer_population <- function(municipios) {
  municipios <- as.character(municipios)
  pop <- municipio_populacao[municipios]
  pop <- as.numeric(pop)

  missing <- is.na(pop) | pop <= 0
  if (any(missing)) {
    hash_base <- vapply(
      municipios[missing],
      function(x) sum(utf8ToInt(enc2utf8(x))),
      numeric(1)
    )
    pop[missing] <- as.numeric(80000 + (hash_base %% 420000))
  }

  round(pop)
}

map_drs_from_gve <- function(gve, drs_raw = NULL) {
  gve <- as.character(gve)
  drs_from_gve <- unname(gve_drs_lookup[gve])

  if (!is.null(drs_raw)) {
    drs_raw <- as.character(drs_raw)
    drs_raw <- trimws(drs_raw)
    drs_raw[drs_raw == ""] <- NA_character_
    drs_from_gve[is.na(drs_from_gve)] <- drs_raw[is.na(drs_from_gve)]
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

find_shape_file <- function(level = c("municipio", "gve", "drs")) {
  level <- match.arg(level)
  env_name <- unname(shape_file_envvars[level])
  configured_path <- Sys.getenv(env_name, unset = "")

  candidates <- c(configured_path, unname(shape_default_paths[level]))
  candidates <- candidates[candidates != ""]

  for (path in unique(candidates)) {
    if (file.exists(path) && grepl("\\.shp$", path, ignore.case = TRUE)) {
      return(path)
    }
  }

  default_dir <- dirname(unname(shape_default_paths[level]))
  if (!dir.exists(default_dir)) {
    return(NULL)
  }

  shp_files <- list.files(
    path = default_dir,
    pattern = "\\.shp$",
    full.names = TRUE,
    ignore.case = TRUE
  )

  if (length(shp_files) == 0) {
    return(NULL)
  }

  shp_files[1]
}

get_shape_candidate_columns <- function(level = c("municipio", "gve", "drs")) {
  level <- match.arg(level)

  switch(
    level,
    municipio = c(
      "municipio", "municipio_nome", "municipio_nm", "name_muni",
      "nm_mun", "nm_municip", "nome", "name"
    ),
    gve = c(
      "gve", "dominio_gve", "nome_gve", "nm_gve", "grupo_vigilancia",
      "grupo_vig", "name", "nome"
    ),
    drs = c(
      "drs", "nome_drs", "nm_drs", "diretoria_regional",
      "regional_saude", "name", "nome"
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

    shp <- shp %>%
      mutate(
        map_key = normalize_shape_key(.data[[key_col]], level = level),
        map_label = as.character(.data[[key_col]])
      )

    attr(shp, "shape_level") <- level
    attr(shp, "shape_path") <- shp_path
    attr(shp, "shape_key_column") <- key_col

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
  casos_col <- pick_column_with_override(
    raw_data,
    override_env_var = "OFTALMO_COL_CASOS",
    candidates = c("TotalCaso", "total_caso", "casos", "totalcaso"),
    regex_candidates = c("total.*caso", "\\bcasos\\b", "qtd.*cas")
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
      uvis = if (!is.null(uvis_col)) sanitize_dimension_values(.data[[uvis_col]], default = NA_character_) else NA_character_,
      uvis = ifelse(normalize_key(municipio) == "sao paulo", uvis, NA_character_),
      drs = if (!is.null(drs_col)) sanitize_dimension_values(.data[[drs_col]]) else "Não informado",
      drs = ifelse(
        drs == "Não informado",
        sanitize_dimension_values(infer_drs_from_gve_text(gve), default = "Não informado"),
        drs
      ),
      casos = sanitize_numeric_values(.data[[casos_col]], default = 0),
      casos = ifelse(is.na(casos), 0, pmax(round(casos), 0)),
      surto_n = if (!is.null(surto_col)) sanitize_numeric_values(.data[[surto_col]], default = 0) else 0,
      surto_n = ifelse(is.na(surto_n), 0, pmax(round(surto_n), 0)),
      surto_flag = if (!is.null(surto_flag_col)) {
        ifelse(tolower(as.character(.data[[surto_flag_col]])) %in% c("1", "true", "sim", "s"), 1, 0)
      } else {
        0
      }
    ) %>%
    mutate(
      ano = dplyr::coalesce(ano_registro, year(data_notificacao)),
      semana = dplyr::coalesce(semana_registro, isoweek(data_notificacao)),
      semana = pmin(pmax(semana, 1), 53),
      populacao = infer_population(municipio),
      surtos = surto_n + surto_flag
    ) %>%
    filter(!is.na(ano), !is.na(semana)) %>%
    group_by(ano, semana, drs, gve, municipio, uvis, populacao) %>%
    summarise(
      casos = sum(casos, na.rm = TRUE),
      surtos = sum(surtos, na.rm = TRUE),
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

generate_mock_data <- function() {
  uvis_pool <- c(
    "UVIS Sé",
    "UVIS Mooca",
    "UVIS Butantã",
    "UVIS Lapa/Pinheiros",
    "UVIS Vila Mariana/Jabaquara",
    "UVIS Freguesia do Ó/Brasilândia",
    "UVIS Penha",
    "UVIS Santana/Tucuruvi/Jaçanã/Tremembé",
    "UVIS Ermelino Matarazzo/São Miguel",
    "UVIS Cidade Tiradentes/Guaianases"
  )

  combinacoes <- lapply(names(drs_gve_mapping), function(drs_atual) {
    gves <- drs_gve_mapping[[drs_atual]]

    bind_rows(lapply(gves, function(gve_atual) {
      tibble::tibble(
        drs = drs_atual,
        gve = gve_atual,
        municipio = gve_municipio_mapping[[gve_atual]]
      )
    }))
  }) %>%
    bind_rows()

  anos <- 2022:year(Sys.Date())
  semanas <- 1:52

  dados <- expand_grid(
    ano = anos,
    semana = semanas
  ) %>%
    crossing(combinacoes) %>%
    mutate(
      ciclo_sazonal = 6 * sin((2 * pi * semana) / 52) + 8 * exp(-((semana - 10)^2) / 70),
      tendencia = (ano - min(anos)) * 1.1,
      ruido = rnorm(n(), mean = 0, sd = 2.5),
      lambda = pmax(1.5, 4 + ciclo_sazonal + tendencia + ruido),
      casos = rpois(n(), lambda = lambda),
      populacao = infer_population(municipio),
      surtos = ifelse(casos > quantile(casos, 0.9, na.rm = TRUE), 1, 0),
      uvis = ifelse(
        normalize_key(municipio) == "sao paulo",
        sample(uvis_pool, n(), replace = TRUE),
        NA_character_
      ),
      incidencia = round((casos / populacao) * 100000, 2)
    ) %>%
    select(ano, semana, drs, gve, municipio, uvis, casos, populacao, incidencia, surtos) %>%
    arrange(ano, semana, drs, gve, municipio, uvis)

  dados
}

load_epidemiological_data <- function(prefer_db = TRUE, allow_mock_fallback = FALSE) {
  db_error <- NULL
  db_empty <- FALSE

  if (prefer_db && exists("load_oftalmo_raw_data", mode = "function")) {
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
  }

  if (!allow_mock_fallback) {
    detailed_reason <- if (!is.null(db_error)) {
      paste("Erro da conexão/consulta:", db_error)
    } else if (db_empty) {
      "A consulta foi executada, mas não retornou dados válidos para o painel."
    } else {
      "A função de carga não retornou dados."
    }

    stop(
      paste(
        "Não foi possível carregar dados do banco.",
        detailed_reason,
        "Como OFTALMO_ALLOW_MOCK está desativado, o app não usará dados simulados."
      )
    )
  }

  mock_data <- generate_mock_data()
  attr(mock_data, "data_source") <- "mock"
  mock_data
}

allow_mock_fallback <- tolower(Sys.getenv("OFTALMO_ALLOW_MOCK", unset = "false")) %in% c("1", "true", "sim", "yes")
dados_epidemiologicos <- load_epidemiological_data(
  prefer_db = TRUE,
  allow_mock_fallback = allow_mock_fallback
)
dados_fonte <- attr(dados_epidemiologicos, "data_source")

get_gve_by_drs <- function(drs) {
  sort(unique(dados_epidemiologicos$gve[dados_epidemiologicos$drs == drs]))
}

get_municipios_by_gve <- function(gve) {
  sort(unique(dados_epidemiologicos$municipio[dados_epidemiologicos$gve == gve]))
}

get_municipio_coords <- function(municipios) {
  coords_known <- data.frame(
    municipio = c(
      "Ribeirão Preto", "Araraquara", "São Carlos", "Campinas", "Piracicaba",
      "Sorocaba", "São Paulo", "Santo André", "São Bernardo", "Bauru", "Jaú",
      "Araçatuba", "Birigui", "Andradina", "Cravinhos", "Limeira", "Valinhos", "Vinhedo"
    ),
    lat = c(
      -21.1775, -21.7947, -22.0174, -22.9056, -22.7253,
      -23.5015, -23.5505, -23.6578, -23.6914, -22.3145, -22.2936,
      -21.2069, -21.2885, -20.8960, -21.3408, -22.5645, -22.9698, -23.0296
    ),
    lng = c(
      -47.8103, -48.1756, -47.8860, -47.0608, -47.6492,
      -47.4526, -46.6333, -46.5333, -46.5655, -49.0587, -48.5592,
      -50.4328, -50.3408, -51.3790, -47.7290, -47.4004, -46.9956, -46.9755
    ),
    stringsAsFactors = FALSE
  )

  base <- data.frame(municipio = unique(as.character(municipios)), stringsAsFactors = FALSE)
  coords <- base %>% left_join(coords_known, by = "municipio")

  missing <- is.na(coords$lat) | is.na(coords$lng)
  if (any(missing)) {
    hash <- vapply(
      coords$municipio[missing],
      function(x) sum(utf8ToInt(enc2utf8(x))),
      numeric(1)
    )

    coords$lat[missing] <- -22.3 + ((hash %% 220) - 110) / 100
    coords$lng[missing] <- -48.2 + (((hash * 7) %% 240) - 120) / 100
  }

  coords
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
