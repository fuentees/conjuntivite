#!/usr/bin/env Rscript

get_script_directory <- function() {
  args_full <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args_full, value = TRUE)
  if (length(file_arg) == 0) {
    return(getwd())
  }
  dirname(normalizePath(sub("^--file=", "", file_arg[1]), winslash = "/", mustWork = TRUE))
}

parse_output_path <- function(args) {
  output_path <- NULL

  for (arg in args) {
    if (grepl("^--output=", arg)) {
      output_path <- sub("^--output=", "", arg)
      break
    }
  }

  if (is.null(output_path) && length(args) > 0) {
    plain_args <- args[!grepl("^--output=", args)]
    if (length(plain_args) > 0) {
      output_path <- plain_args[1]
    }
  }

  if (is.null(output_path) || trimws(output_path) == "") {
    output_path <- Sys.getenv("OFTALMO_JSON_EXPORT_PATH", unset = file.path("data", "oftalmo_snapshot.json"))
  }

  output_path
}

main <- function() {
  script_dir <- get_script_directory()
  project_root <- normalizePath(file.path(script_dir, ".."), winslash = "/", mustWork = TRUE)
  setwd(project_root)

  args <- commandArgs(trailingOnly = TRUE)
  output_path <- parse_output_path(args)

  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Pacote 'jsonlite' não está instalado. Execute install.packages('jsonlite').", call. = FALSE)
  }

  Sys.setenv(
    OFTALMO_DATA_MODE = "db"
  )

  source("R/data.R", local = TRUE)

  if (!exists("dados_epidemiologicos", inherits = TRUE)) {
    stop("Objeto 'dados_epidemiologicos' não encontrado após carregar R/data.R.", call. = FALSE)
  }

  dados <- get("dados_epidemiologicos", inherits = TRUE)
  fonte <- get("dados_fonte", inherits = TRUE)

  if (is.null(dados) || nrow(dados) == 0) {
    stop("Nenhum dado foi carregado para exportação.", call. = FALSE)
  }

  if (!identical(fonte, "database")) {
    stop(
      paste0(
        "Exportação abortada: fonte atual = '", fonte,
        "'. Verifique a conexão e as variáveis OFTALMO_DB_*."
      ),
      call. = FALSE
    )
  }

  output_path <- path.expand(output_path)
  output_dir <- dirname(output_path)
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }

  payload <- list(
    generated_at_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    source = "database",
    n_records = nrow(dados),
    schema_version = 1,
    prepared_dataset = TRUE,
    data = dados
  )

  jsonlite::write_json(
    x = payload,
    path = output_path,
    pretty = FALSE,
    auto_unbox = TRUE,
    na = "null",
    dataframe = "rows"
  )

  message("JSON exportado com sucesso: ", normalizePath(output_path, winslash = "/", mustWork = FALSE))
  message("Registros exportados: ", nrow(dados))
}

main()
