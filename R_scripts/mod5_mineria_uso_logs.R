# ==============================================================================
# MÓDULO 5 (V2): MINERÍA DE USO WEB (LOGS, RANKING EN VIVO Y ESTADÍSTICAS)
# Archivo: R_scripts/mod5_mineria_uso_logs.R
# ==============================================================================
library(DBI)
library(dplyr)
if (file.exists("conexion_bd.R")) source("conexion_bd.R") else source("R_scripts/conexion_bd.R")

texto_o_na <- function(x) if (is.null(x) || length(x) == 0 || is.na(x)) NA_character_ else as.character(x)

registrar_consulta_log <- function(consulta_usuario, destino_recuperado = NA, cluster = NA,
                                   sentimiento = NA, sesion_id = NA, canal = "texto", intencion = NA) {
  con <- conectar_supabase()
  on.exit(dbDisconnect(con))
  
  nuevo_registro <- data.frame(
    fecha_hora         = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    consulta_usuario   = texto_o_na(consulta_usuario),
    destino_recuperado = texto_o_na(destino_recuperado),
    cluster            = texto_o_na(cluster),
    sentimiento        = texto_o_na(sentimiento),
    sesion_id          = texto_o_na(sesion_id),
    canal              = texto_o_na(canal),
    intencion          = texto_o_na(intencion),
    stringsAsFactors   = FALSE
  )
  dbWriteTable(con, "logs_uso_chatbot", nuevo_registro, append = TRUE, row.names = FALSE)
}

obtener_resumen_uso <- function(n = 15) {
  con <- conectar_supabase()
  on.exit(dbDisconnect(con))
  dbGetQuery(con, sprintf("SELECT * FROM logs_uso_chatbot ORDER BY id_log DESC LIMIT %d", as.integer(n)))
}

# Ranking en vivo de los destinos más buscados
obtener_ranking_destinos <- function(n = 10) {
  con <- conectar_supabase()
  on.exit(dbDisconnect(con))
  dbGetQuery(con, sprintf("
    SELECT destino_recuperado AS destino,
           COUNT(*)::int      AS consultas,
           MAX(cluster)       AS cluster
    FROM logs_uso_chatbot
    WHERE destino_recuperado IS NOT NULL AND destino_recuperado <> ''
    GROUP BY destino_recuperado
    ORDER BY consultas DESC, destino
    LIMIT %d", as.integer(n)))
}

# Totales: consultas, voz vs texto, y consultas por categoría temática
obtener_estadisticas_uso <- function() {
  con <- conectar_supabase()
  on.exit(dbDisconnect(con))
  total <- dbGetQuery(con, "SELECT COUNT(*)::int AS n FROM logs_uso_chatbot")$n
  por_canal <- dbGetQuery(con, "SELECT COALESCE(canal,'texto') AS canal, COUNT(*)::int AS n
                                FROM logs_uso_chatbot GROUP BY 1 ORDER BY n DESC")
  por_cluster <- dbGetQuery(con, "SELECT cluster, COUNT(*)::int AS n FROM logs_uso_chatbot
                                  WHERE cluster IS NOT NULL AND cluster <> '' GROUP BY cluster ORDER BY n DESC")
  list(total = total, por_canal = por_canal, por_cluster = por_cluster)
}

if (sys.nframe() == 0) {
  cat("Probando Módulo 5 (Minería de Uso Web en Supabase)...\n")
  if (nrow(obtener_resumen_uso()) == 0) {
    registrar_consulta_log("¿Qué atractivos tiene el Salar de Uyuni?", "Salar de Uyuni", "Atractivos Naturales", "Positivo (+4)", "semilla", "texto", "consulta")
    registrar_consulta_log("¿Qué se come en La Paz?", "Salteña", "Gastronomía", "Positivo (+2)", "semilla", "voz", "consulta")
  }
  print(obtener_resumen_uso())
  print(obtener_ranking_destinos())
}
