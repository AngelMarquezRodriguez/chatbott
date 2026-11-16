# ==============================================================================
# CONEXIÓN A POSTGRESQL EN LA NUBE (SUPABASE)
# Archivo: R_scripts/conexion_bd.R
# Las credenciales ya NO van dentro del código: se leen del archivo .Renviron
# ==============================================================================
library(DBI)
library(RPostgres)

conectar_supabase <- function() {
  password <- Sys.getenv("DB_PASSWORD")
  user     <- Sys.getenv("DB_USER")
  if (!nzchar(password) || !nzchar(user)) {
    stop("Faltan credenciales. Copia '.Renviron.example' a '.Renviron', complétalo y reinicia R.")
  }
  dbConnect(
    RPostgres::Postgres(),
    host     = Sys.getenv("DB_HOST", "aws-1-ca-central-1.pooler.supabase.com"),
    port     = as.integer(Sys.getenv("DB_PORT", "5432")),
    dbname   = Sys.getenv("DB_NAME", "postgres"),
    user     = user,
    password = password,
    sslmode  = "require"
  )
}
