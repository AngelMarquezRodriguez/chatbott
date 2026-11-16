# ==============================================================================
# MÓDULO 3 (V2): MINERÍA DE CONTENIDO (PLN, TF-IDF, K-MEANS, NER-LOC, SENTIMIENTOS)
# Archivo: R_scripts/mod3_contenido_pln_clustering.R
#  - Clustering K-Means con nombres REALES: Atractivos Naturales / Gastronomía /
#    Historia y Museos (antes los nombres se asignaban al azar)
#  - NER de lugares (LOC): spaCy si está instalado; si no, gazetteer + patrones
#  - Sentimiento con negación y alertas de riesgo (frío, altura, lluvias, etc.)
# ==============================================================================
library(tm)
library(stringr)
library(stringi)
library(dplyr)
source("R_scripts/conexion_bd.R")

norm <- function(x) {
  x <- stri_trans_general(tolower(x), "Latin-ASCII")
  gsub("[^a-z0-9 ]", " ", x)
}

# ------------------------------------------------------------------------------
# Léxicos temáticos (para nombrar los clústeres)
# ------------------------------------------------------------------------------
lexicos <- list(
  "Atractivos Naturales" = c("lago", "salar", "montana", "parque", "nacional", "reserva", "fauna",
                             "flora", "selva", "rio", "laguna", "volcan", "nevado", "glaciar", "valle",
                             "desierto", "altiplano", "biodiversidad", "especies", "cordillera", "cerro",
                             "amazonia", "bosque", "cumbre", "altitud", "clima", "trekking", "isla"),
  "Gastronomía" = c("plato", "platos", "comida", "sopa", "carne", "maiz", "papa", "receta", "ingredientes",
                    "salsa", "picante", "bebida", "relleno", "horno", "cocina", "gastronomia", "pollo",
                    "arroz", "chuno", "empanada", "queso", "harina", "desayuno", "elaborada", "fermentada"),
  "Historia y Museos" = c("historia", "museo", "colonial", "siglo", "imperio", "fundada", "iglesia",
                          "templo", "arqueologico", "independencia", "ruinas", "patrimonio", "catedral",
                          "jesuitas", "misiones", "festividad", "carnaval", "fiesta", "danza", "virgen",
                          "espanoles", "precolombina", "civilizacion", "cultura", "unesco", "arte")
)

# ------------------------------------------------------------------------------
# NER de lugares (LOC)
# ------------------------------------------------------------------------------
gazetario <- c("Bolivia", "La Paz", "El Alto", "Sucre", "Potosí", "Cochabamba", "Santa Cruz", "Oruro",
               "Tarija", "Beni", "Pando", "Uyuni", "Copacabana", "Titicaca", "Tiahuanaco", "Tiwanaku",
               "Madidi", "Rurrenabaque", "Coroico", "Yungas", "Chuquisaca", "Altiplano", "Andes", "Perú",
               "Chile", "Argentina", "Brasil", "Paraguay", "Illimani", "Sajama", "Samaipata", "Chiquitos",
               "Torotoro", "Amazonas", "Amazonía", "Chaco", "Lima", "Cusco", "Desaguadero", "Challapata",
               "Sorata", "Achacachi", "Tupiza", "Villazón", "Yacuiba", "Trinidad", "Cobija", "Quillacollo",
               "Punata", "Villa Tunari", "Chapare", "Pampa", "Sud Lípez", "Lípez", "Avaroa")

patron_geo <- paste0(
  "\\b(?:Lago|R[ií]o|Cerro|Parque\\s+Nacional|Valle|Salar|Isla|Laguna|Cordillera|Nevado|Volc[aá]n|",
  "Pen[ií]nsula|Departamento|Provincia|Municipio)\\s+",
  "(?:(?:de|del|de\\s+la|de\\s+los|la|las|los)\\s+)?[A-ZÁÉÍÓÚÑ][a-záéíóúñ]+(?:\\s+[A-ZÁÉÍÓÚÑ][a-záéíóúñ]+)?"
)

usar_spacy <- FALSE
if (requireNamespace("spacyr", quietly = TRUE)) {
  usar_spacy <- tryCatch({
    spacyr::spacy_initialize(model = "es_core_news_md")
    TRUE
  }, error = function(e) FALSE)
}
cat(ifelse(usar_spacy, "   NER: usando spaCy (es_core_news_md)\n",
           "   NER: usando gazetteer + patrones (spaCy no disponible)\n"))

extraer_loc <- function(texto, max_loc = 10) {
  hallazgos <- character(0)
  
  if (usar_spacy) {
    ents <- tryCatch(spacyr::spacy_extract_entity(texto), error = function(e) NULL)
    if (!is.null(ents) && nrow(ents) > 0)
      hallazgos <- c(hallazgos, gsub("_", " ", ents$text[ents$ent_type == "LOC"]))
  }
  
  hallazgos <- c(hallazgos,
                 gazetario[vapply(gazetario, function(g) grepl(g, texto, fixed = TRUE), logical(1))],
                 unlist(str_extract_all(texto, patron_geo)))
  
  hallazgos <- unique(str_squish(hallazgos))
  hallazgos <- hallazgos[nchar(hallazgos) > 3]
  if (length(hallazgos) == 0) return("")
  pos <- vapply(hallazgos, function(h) { p <- regexpr(h, texto, fixed = TRUE)[1]; ifelse(p < 0, 1e6, p) }, numeric(1))
  paste(head(hallazgos[order(pos)], max_loc), collapse = ", ")
}

# ------------------------------------------------------------------------------
# Sentimiento + alertas de riesgo
# ------------------------------------------------------------------------------
positivas <- c("patrimonio", "importante", "mayor", "atractivo", "turistico", "riqueza", "biodiversidad",
               "hermoso", "famoso", "cultura", "tradicional", "historico", "maravilla", "unico", "destaca",
               "principal", "belleza", "reserva", "libertad", "obra", "declarado", "reconocido", "popular",
               "emblematico", "espectacular", "sabroso", "delicioso", "tipico", "visitado", "mejor",
               "ideal", "recomendable", "impresionante", "encanto", "paisaje", "unesco")
negativas <- c("peligro", "riesgo", "extremo", "sequia", "contaminacion", "amenaza", "perdida", "conflicto",
               "crisis", "desastre", "frio", "escasez", "peligroso", "deterioro", "abandono", "inundacion",
               "deslizamiento", "bloqueo", "accidente", "helada", "tormenta", "soroche")
riesgos <- c(
  "peligro" = "peligros", "peligroso" = "zonas peligrosas", "riesgo" = "riesgos", "frio" = "frío intenso",
  "helada" = "heladas", "soroche" = "mal de altura (soroche)", "altura" = "gran altitud",
  "altitud" = "gran altitud", "lluvia" = "temporada de lluvias", "lluvias" = "temporada de lluvias",
  "inundacion" = "inundaciones", "deslizamiento" = "deslizamientos", "sequia" = "sequías",
  "contaminacion" = "contaminación", "bloqueo" = "bloqueos de carretera", "tormenta" = "tormentas",
  "extremo" = "condiciones extremas", "accidente" = "accidentes", "escasez" = "escasez de servicios"
)
negadores <- c("no", "sin", "nunca", "ni")

analizar_sentimiento <- function(texto) {
  tok <- unlist(strsplit(str_squish(norm(texto)), " "))
  tok <- tok[nzchar(tok)]
  score <- 0
  for (i in seq_along(tok)) {
    w <- tok[i]
    signo <- if (w %in% positivas) 1 else if (w %in% negativas) -1 else 0
    if (signo != 0 && i > 1 && tok[i - 1] %in% negadores) signo <- -signo
    score <- score + signo
  }
  encontrados <- unique(riesgos[names(riesgos) %in% tok])
  alertas <- if (length(encontrados) > 0) paste(encontrados, collapse = ", ") else ""
  
  etiqueta <- if (score >= 3) paste0("Muy Positivo (+", score, ")")
  else if (score > 0) paste0("Positivo (+", score, ")")
  else if (score < 0) paste0("Precaución (", score, ")")
  else "Informativo / Neutro (0)"
  
  list(etiqueta = etiqueta, score = score, alertas = alertas)
}

# ------------------------------------------------------------------------------
# Módulo principal
# ------------------------------------------------------------------------------
ejecutar_modulo3 <- function() {
  cat("1. Descargando textos indexados desde Supabase...\n")
  con <- conectar_supabase()
  on.exit(dbDisconnect(con), add = TRUE)
  nodos <- dbGetQuery(con, "SELECT titulo, texto_indexado FROM indice_nodos")
  metricas <- dbGetQuery(con, "SELECT nombre_nodo, id_url, enlaces_entrantes, enlaces_salientes,
                                      pagerank_real, pagerank_10, authority, hub FROM metricas_mineria")
  df <- metricas %>% left_join(nodos, by = c("nombre_nodo" = "titulo"))
  df$texto_indexado[is.na(df$texto_indexado)] <- ""
  
  # A) PLN + TF-IDF -----------------------------------------------------------
  cat("2. Creando Corpus PLN y Matriz TF-IDF...\n")
  corpus <- VCorpus(VectorSource(df$texto_indexado))
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removeWords, c(stopwords("spanish"), "bolivia", "ciudad", "parte", "año", "años"))
  corpus <- tm_map(corpus, stripWhitespace)
  dtm <- DocumentTermMatrix(corpus, control = list(weighting = weightTfIdf))
  matriz_tfidf <- as.matrix(dtm)
  
  # B) Clustering K-Means con nombres reales ----------------------------------
  cat("3. Ejecutando Clustering K-Means (k = 3) con nombres temáticos reales...\n")
  tokens <- lapply(df$texto_indexado, function(t) {
    x <- unlist(strsplit(str_squish(norm(t)), " ")); x[nzchar(x)]
  })
  S <- t(vapply(tokens, function(tk) {
    vapply(lexicos, function(lx) sum(tk %in% lx) / max(length(tk), 1), numeric(1))
  }, numeric(length(lexicos))))
  colnames(S) <- names(lexicos)
  
  escalar <- function(m) apply(m, 2, function(x) {
    s <- sd(x); if (is.na(s) || s == 0) x * 0 else (x - mean(x)) / s
  })
  pca <- prcomp(matriz_tfidf, scale. = FALSE)
  X <- cbind(escalar(S), pca$x[, 1:min(2, ncol(pca$x)), drop = FALSE])
  
  set.seed(123)
  k_res <- kmeans(X, centers = 3, nstart = 25)
  
  # Asignar a cada clúster la categoría que mejor encaja (prueba las 6 permutaciones)
  M <- sapply(1:3, function(k) colMeans(S[k_res$cluster == k, , drop = FALSE]))  # categorías x clústeres
  perms <- list(c(1,2,3), c(1,3,2), c(2,1,3), c(2,3,1), c(3,1,2), c(3,2,1))
  mejor <- perms[[which.max(sapply(perms, function(p) sum(M[cbind(p, 1:3)])))]]
  nombre_por_cluster <- names(lexicos)[mejor]       # posición k -> categoría
  df$cluster <- nombre_por_cluster[k_res$cluster]
  
  cat("\n   Distribución de clústeres:\n"); print(table(df$cluster))
  
  # C) NER (LOC) y D) Sentimientos ---------------------------------------------
  cat("4. Extrayendo Lugares (NER-LOC) y Análisis de Sentimientos...\n")
  df$entidades_ner <- vapply(df$texto_indexado, extraer_loc, character(1), USE.NAMES = FALSE)
  sent <- lapply(df$texto_indexado, analizar_sentimiento)
  df$sentimiento_base <- vapply(sent, function(s) s$etiqueta, character(1))
  df$alertas <- vapply(sent, function(s) s$alertas, character(1))
  
  tabla_final <- df %>% select(
    nombre_nodo, id_url, enlaces_entrantes, enlaces_salientes,
    pagerank_real, pagerank_10, authority, hub,
    cluster, sentimiento_base, entidades_ner, alertas
  )
  
  dbExecute(con, "TRUNCATE TABLE metricas_mineria;")
  dbWriteTable(con, "metricas_mineria", tabla_final, append = TRUE, row.names = FALSE)
  
  cat("\n=== MINERÍA DE CONTENIDO GUARDADA EN SUPABASE ===\n")
  print(tabla_final %>% select(nombre_nodo, cluster, sentimiento_base, entidades_ner))
  if (usar_spacy) spacyr::spacy_finalize()
}

ejecutar_modulo3()
