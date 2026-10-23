ensure_db_dependency <- function(pkg) {
  # Os pacotes vêm do renv.lock (renv::restore()); não instala nada em tempo de execução.
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(sprintf("Pacote '%s' não disponível para conexão com banco. Rode renv::restore().", pkg))
  }
}

# Conexão com o MariaDB. Usuário e senha vêm só de variáveis de ambiente
# (arquivo .Renviron na pasta do projeto, fora do Git). Veja .Renviron.example.
ConectDb <- function() {
  ensure_db_dependency("DBI")
  ensure_db_dependency("RMariaDB")

  usuario <- Sys.getenv("OFTALMO_DB_USER")
  senha <- Sys.getenv("OFTALMO_DB_PASSWORD")
  if (usuario == "" || senha == "") {
    stop(
      "Usuário ou senha do banco não configurados. Copie .Renviron.example para .Renviron, ",
      "preencha OFTALMO_DB_USER e OFTALMO_DB_PASSWORD e reinicie o R.",
      call. = FALSE
    )
  }

  DBI::dbConnect(
    RMariaDB::MariaDB(),
    user = usuario,
    password = senha,
    encoding = "UTF-8",
    dbname = Sys.getenv("OFTALMO_DB_NAME", unset = "cevesp_2k17"),
    host = Sys.getenv("OFTALMO_DB_HOST", unset = "192.168.1.204"),
    port = as.integer(Sys.getenv("OFTALMO_DB_PORT", unset = "3306"))
  )
}

fetch_oftalmo_raw_data <- function(con) {
  ensure_db_dependency("DBI")

  my_query <- "SELECT * FROM `OftalmoNotifica9a60` WHERE 1;"
  DBI::dbGetQuery(con, my_query)
}

load_oftalmo_raw_data <- function() {
  con <- ConectDb()
  on.exit(
    try(DBI::dbDisconnect(con), silent = TRUE),
    add = TRUE
  )

  fetch_oftalmo_raw_data(con)
}
