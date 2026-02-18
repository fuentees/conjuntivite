ensure_db_dependency <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
  }

  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(sprintf("Pacote '%s' não disponível para conexão com banco.", pkg))
  }
}

# Função para conectar ao banco de dados MariaDB.
# Permite sobrescrever por variáveis de ambiente sem alterar o código.
ConectDb <- function() {
  ensure_db_dependency("DBI")
  ensure_db_dependency("RMariaDB")

  con <- DBI::dbConnect(
    RMariaDB::MariaDB(),
    user = Sys.getenv("OFTALMO_DB_USER", unset = "vfontes"),
    password = Sys.getenv("OFTALMO_DB_PASSWORD", unset = "Saude@123Oftal"),
    encoding = "UTF-8",
    dbname = Sys.getenv("OFTALMO_DB_NAME", unset = "cevesp_2k17"),
    host = Sys.getenv("OFTALMO_DB_HOST", unset = "192.168.1.204")
  )

  con
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
