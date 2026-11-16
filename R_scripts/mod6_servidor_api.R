# ==============================================================================
# MÓDULO 6 (V2 CORREGIDO): SERVIDOR API DEL CHATBOT (PLUMBER)
# Archivo: R_scripts/mod6_servidor_api.R
# ==============================================================================
library(plumber)
library(stringr)
library(dplyr)
library(jsonlite)
if (file.exists("conexion_bd.R")) {
  source("conexion_bd.R"); source("mod5_mineria_uso_logs.R"); source("motor_chat.R")
} else {
  source("R_scripts/conexion_bd.R"); source("R_scripts/mod5_mineria_uso_logs.R"); source("R_scripts/motor_chat.R")
}

# Asegurar que las columnas de la migración v2 existan en Supabase
asegurar_columnas_v2 <- function() {
  tryCatch({
    con <- conectar_supabase()
    on.exit(dbDisconnect(con))
    dbExecute(con, "ALTER TABLE indice_nodos ADD COLUMN IF NOT EXISTS resumen TEXT;")
    dbExecute(con, "ALTER TABLE indice_nodos ADD COLUMN IF NOT EXISTS imagen_url TEXT;")
    dbExecute(con, "ALTER TABLE indice_nodos ADD COLUMN IF NOT EXISTS fuente TEXT;")
    dbExecute(con, "ALTER TABLE metricas_mineria ADD COLUMN IF NOT EXISTS alertas TEXT;")
    dbExecute(con, "ALTER TABLE logs_uso_chatbot ADD COLUMN IF NOT EXISTS sesion_id TEXT;")
    dbExecute(con, "ALTER TABLE logs_uso_chatbot ADD COLUMN IF NOT EXISTS canal TEXT;")
    dbExecute(con, "ALTER TABLE logs_uso_chatbot ADD COLUMN IF NOT EXISTS intencion TEXT;")
  }, error = function(e) message("Aviso migración BD: ", conditionMessage(e)))
}
asegurar_columnas_v2()

cargar_datos_memoria <- function() {
  con <- conectar_supabase()
  on.exit(dbDisconnect(con))
  nodos    <- dbGetQuery(con, "SELECT * FROM indice_nodos")
  metricas <- dbGetQuery(con, "SELECT * FROM metricas_mineria")
  trends   <- dbGetQuery(con, "SELECT * FROM tendencias_google ORDER BY popularidad_promedio DESC")
  
  cols_nodo <- intersect(c("titulo", "texto_indexado", "resumen", "imagen_url", "fuente"), names(nodos))
  base <- metricas %>% left_join(nodos[, cols_nodo], by = c("nombre_nodo" = "titulo"))
  list(base = preparar_indice(base), trends = trends)
}

memoria <- cargar_datos_memoria()

#* @filter cors
function(req, res) {
  res$setHeader("Access-Control-Allow-Origin", "*")
  res$setHeader("Access-Control-Allow-Methods", "GET, POST, OPTIONS")
  res$setHeader("Access-Control-Allow-Headers", "Content-Type")
  if (req$REQUEST_METHOD == "OPTIONS") {
    res$status <- 200
    return(list())
  }
  plumber::forward()
}

responder_chat <- function(mensaje, sesion_id, canal) {
  if (is.null(sesion_id) || !nzchar(sesion_id)) sesion_id <- paste0("ses_", as.integer(Sys.time()))
  if (!canal %in% c("texto", "voz")) canal <- "texto"
  procesar_mensaje(mensaje, sesion_id, canal, memoria$base, registrar = registrar_consulta_log)
}

#* Conversación principal
#* @serializer unboxedJSON
#* @post /chat
function(req) {
  pb <- req$postBody
  if (is.null(pb)) pb <- ""
  Encoding(pb) <- "UTF-8"
  b <- tryCatch(fromJSON(pb, simplifyVector = FALSE), error = function(e) list())
  sid <- vacio(b$session_id)
  if (!nzchar(sid)) sid <- vacio(b$sesion_id)
  responder_chat(vacio(b$mensaje), sid, vacio(b$canal))
}

#* Versión GET
#* @serializer unboxedJSON
#* @get /chat
function(q = "", sesion_id = "prueba") {
  responder_chat(q, sesion_id, "texto")
}

#* Reiniciar sesión
#* @serializer unboxedJSON
#* @post /reiniciar
function(req) {
  pb <- req$postBody; if (is.null(pb)) pb <- ""
  b <- tryCatch(fromJSON(pb, simplifyVector = FALSE), error = function(e) list())
  sid <- vacio(b$session_id)
  if (!nzchar(sid)) sid <- vacio(b$sesion_id)
  if (nzchar(sid)) reiniciar_sesion(sid)
  list(ok = TRUE)
}

#* Ranking en vivo de destinos y lugares NER (formato exacto para index.html)
#* @serializer unboxedJSON
#* @get /ranking
function() {
  dest <- tryCatch(obtener_ranking_destinos(10), error = function(e) data.frame())
  est  <- tryCatch(obtener_estadisticas_uso(), error = function(e) list(total = 0, por_canal = data.frame()))
  
  voz_n <- 0
  if (!is.null(est$por_canal) && nrow(est$por_canal) > 0) {
    v_row <- est$por_canal[est$por_canal$canal == "voz", ]
    if (nrow(v_row) > 0) voz_n <- sum(v_row$n)
  }
  
  # Calcular los lugares NER más mencionados en los destinos consultados
  todos_ner <- unlist(strsplit(paste(memoria$base$entidades_ner, collapse = ", "), ",\\s*"))
  todos_ner <- todos_ner[nzchar(todos_ner) & todos_ner != "Bolivia"]
  tb_ner <- sort(table(todos_ner), decreasing = TRUE)
  lugares_df <- if (length(tb_ner) > 0) {
    data.frame(lugar = names(head(tb_ner, 8)), menciones = as.integer(head(tb_ner, 8)), stringsAsFactors = FALSE)
  } else data.frame()
  
  list(
    destinos    = dest,
    lugares     = lugares_df,
    stats       = list(total = est$total, voz = voz_n),
    actualizado = format(Sys.time(), "%H:%M:%S")
  )
}

#* Grafo interactivo para la pestaña 'Grafo'
#* @serializer unboxedJSON
#* @get /grafo
function() {
  con <- conectar_supabase()
  on.exit(dbDisconnect(con))
  aristas <- dbGetQuery(con, "SELECT from_nodo AS desde, to_nodo AS hasta FROM enlaces_aristas")
  nodos <- memoria$base %>% select(nombre = nombre_nodo, pagerank_10, cluster)
  list(nodos = nodos, aristas = aristas)
}

#* Datos para las tablas del panel de minería
#* @serializer unboxedJSON
#* @get /dashboard
function() {
  red <- memoria$base %>%
    select(nombre_nodo, id_url, enlaces_entrantes, pagerank_real, pagerank_10, authority, hub,
           cluster, sentimiento_base, alertas, entidades_ner)
  list(
    logs   = tryCatch(obtener_resumen_uso(20), error = function(e) data.frame()),
    red    = red,
    trends = memoria$trends
  )
}

#* Estado compatible con index.html
#* @serializer unboxedJSON
#* @get /estado
function() {
  list(
    estado   = "ok",
    destinos = nrow(memoria$base),
    llm      = ifelse(nzchar(Sys.getenv("ANTHROPIC_API_KEY")), "Claude", "ninguno"),
    modelo   = Sys.getenv("ANTHROPIC_MODEL", "claude-sonnet-4-6"),
    whisper  = FALSE
  )
}