library(stringr)
library(stringi)
library(httr)
library(jsonlite)

NOMBRE_BOT <- "Wayra"

norm <- function(x) {
  x <- stri_trans_general(tolower(x), "Latin-ASCII")
  gsub("[^a-z0-9 ]", " ", x)
}

vacio <- function(x) {
  if (is.null(x)) return("")
  if (length(x) == 0) return("")
  if (is.na(x[1])) return("")
  as.character(x[1])
}

STOP_ES <- c("que", "como", "donde", "cual", "cuales", "para", "con", "los", "las", "del", "una", "uno",
             "por", "sobre", "hay", "tiene", "tienen", "quiero", "conocer", "visitar", "lugar", "lugares",
             "ver", "hola", "bolivia", "turismo", "turistico", "turisticos", "dime", "cuentame", "puedo",
             "me", "mas", "muy", "este", "esta", "esto", "algo", "sus", "son", "ser", "hacer", "favor",
             "informacion", "dame", "decime", "saber", "tambien", "pero", "porque", "cuando", "quien")

CATEGORIAS <- c("Atractivos Naturales", "Gastronomía", "Historia y Museos")

COORD_BOLIVIA <- data.frame(
  nombre = c("Bolivia", "La Paz", "El Alto", "Sucre", "Potosí", "Cochabamba", "Santa Cruz", "Santa Cruz de la Sierra",
             "Oruro", "Tarija", "Beni", "Trinidad", "Pando", "Cobija", "Uyuni", "Salar de Uyuni", "Copacabana",
             "Lago Titicaca", "Titicaca", "Isla del Sol", "Tiahuanaco", "Tiwanaku", "Madidi", "Parque Nacional Madidi",
             "Rurrenabaque", "Coroico", "Yungas", "Camino de la Muerte", "Sajama", "Parque Nacional Sajama",
             "Laguna Colorada", "Reserva Eduardo Avaroa", "Valle de la Luna", "Torotoro", "Parque Nacional Torotoro",
             "Illimani", "Huayna Potosí", "Chacaltaya", "Cerro Rico", "Cerro Rico de Potosí", "Casa de la Libertad",
             "Chiquitos", "Misiones Jesuíticas de Chiquitos", "Samaipata", "Fuerte de Samaipata", "Carnaval de Oruro",
             "Urkupiña", "Quillacollo", "Sorata", "Tupiza", "Altiplano", "Andes", "Amazonía"),
  lat = c(-16.2902, -16.4897, -16.5000, -19.0196, -19.5723, -17.3895, -17.7833, -17.7833,
          -17.9833, -21.5355, -14.8333, -14.8333, -11.0267, -11.0267, -20.4597, -20.1338, -16.1667,
          -15.9254, -15.9254, -16.0167, -16.5547, -16.5547, -13.6667, -13.6667,
          -14.4414, -16.1889, -16.3000, -16.3333, -18.1081, -18.1081,
          -22.1983, -22.4667, -16.5672, -18.1333, -18.1333,
          -16.6333, -16.2622, -16.3533, -19.6186, -19.6186, -19.0478,
          -16.3742, -16.3742, -18.1797, -18.1797, -17.9647,
          -17.3925, -17.3925, -15.7731, -21.4439, -18.0000, -16.5000, -13.5000),
  lon = c(-63.5887, -68.1193, -68.1500, -65.2620, -65.7550, -66.1568, -63.1821, -63.1821,
          -67.1500, -64.7296, -64.9000, -64.9000, -68.7692, -68.7692, -66.8250, -67.4891, -69.0833,
          -69.3354, -69.3354, -69.1833, -68.6733, -68.6733, -68.2500, -68.2500,
          -67.5278, -67.7272, -67.8000, -67.8167, -68.8831, -68.8831,
          -67.7722, -67.5000, -68.0942, -65.7667, -65.7667,
          -67.7908, -68.1489, -68.1314, -65.7497, -65.7497, -65.2596,
          -60.9606, -60.9606, -63.8756, -63.8756, -67.1144,
          -66.2783, -66.2783, -68.6486, -65.7186, -67.5000, -68.0000, -66.0000),
  stringsAsFactors = FALSE
)
COORD_BOLIVIA$.norm <- norm(COORD_BOLIVIA$nombre)

geocodificar_entidades <- function(docs) {
  if (nrow(docs) == 0) return(list())
  nombres_raw <- unique(c(docs$nombre_nodo, unlist(strsplit(paste(docs$entidades_ner, collapse = ", "), ",\\s*"))))
  nombres_raw <- nombres_raw[nzchar(nombres_raw)]
  res <- list()
  vistos <- character(0)
  for (nom in nombres_raw) {
    nn <- str_squish(norm(nom))
    if (!nzchar(nn)) next
    if (nn %in% vistos) next
    idx <- which(COORD_BOLIVIA$.norm == nn)[1]
    if (is.na(idx)) {
      c1 <- grepl(nn, COORD_BOLIVIA$.norm, fixed = TRUE)
      c2 <- grepl(COORD_BOLIVIA$.norm, nn, fixed = TRUE)
      idx <- which(c1 | c2)[1]
    }
    if (!is.na(idx)) {
      vistos <- c(vistos, nn)
      res[[length(res) + 1]] <- list(
        nombre = COORD_BOLIVIA$nombre[idx],
        lat    = COORD_BOLIVIA$lat[idx],
        lon    = COORD_BOLIVIA$lon[idx],
        tipo   = "Entidad Geográfica (NER-LOC)"
      )
    }
  }
  head(res, 10)
}

preparar_indice <- function(df) {
  for (col in c("resumen", "imagen_url", "fuente", "alertas", "cluster", "sentimiento_base", "entidades_ner")) {
    if (!col %in% names(df)) df[[col]] <- ""
    df[[col]][is.na(df[[col]])] <- ""
  }
  df$texto_indexado[is.na(df$texto_indexado)] <- ""
  df$resumen <- ifelse(nzchar(df$resumen), df$resumen, substr(df$texto_indexado, 1, 600))
  for (col in c("pagerank_real", "pagerank_10", "authority", "hub", "enlaces_entrantes")) {
    df[[col]][is.na(df[[col]])] <- 0
  }
  df$.titulo_n <- norm(df$nombre_nodo)
  df$.texto_n  <- norm(df$texto_indexado)
  df
}

.sesiones <- new.env(parent = emptyenv())

obtener_sesion <- function(id) {
  if (!exists(id, envir = .sesiones, inherits = FALSE)) {
    assign(id, list(historial = list(), ultimo_destino = NULL, turnos = 0), envir = .sesiones)
  }
  get(id, envir = .sesiones)
}

guardar_sesion <- function(id, s) {
  assign(id, s, envir = .sesiones)
}

reiniciar_sesion <- function(id) {
  if (exists(id, envir = .sesiones, inherits = FALSE)) {
    rm(list = id, envir = .sesiones)
  }
}

detectar_intencion <- function(q) {
  n <- str_squish(norm(q))
  if (nchar(n) < 40 && grepl("^(hola|holi|buenas|buenos dias|buen dia|buenas tardes|buenas noches|hey|saludos|que tal|hello)", n)) return("saludo")
  if (grepl("\\b(chau|chao|adios|hasta luego|nos vemos|hasta pronto|bye)\\b", n)) return("despedida")
  if (nchar(n) < 50 && grepl("\\b(gracias|te agradezco|muy amable)\\b", n)) return("gracias")
  if (grepl("(quien eres|quien sos|como te llamas|tu nombre|que eres|que puedes hacer|que haces|para que sirves|^ayuda|como funcionas)", n)) return("identidad")
  if (grepl("(como estas|como te va|como andas|como te sientes)", n)) return("estado")
  if (grepl("(ruta|itinerario|plan de viaje|armame|arma un|organiza.*viaje|recorrido|\\d+ dias)", n)) return("ruta")
  if (grepl("(mas buscad|mas popular|mas visitad|ranking|\\btop\\b|principales destinos|mejores destinos|que me recomiendas|recomiendame|recomienda)", n)) return("ranking")
  if (grepl("(compar|diferencia entre|\\bvs\\b|versus|cual es mejor)", n)) return("comparar")
  if (grepl("(riesgo|peligro|seguro|segura|clima|soroche|altura|\\bfrio|llueve|epoca|cuando ir|mejor epoca|advertencia|precaucion|cuidado)", n)) return("riesgos")
  "consulta"
}

detectar_categoria <- function(q) {
  n <- norm(q)
  if (grepl("(comer|comida|plato|gastronom|tipico|sabor|salten|sopa|bebida|receta|restaurante|cocina|desayun|almorz)", n)) return("Gastronomía")
  if (grepl("(naturaleza|natural|paisaje|lago|montan|parque|trekking|aventura|selva|nevado|laguna|salar|senderismo)", n)) return("Atractivos Naturales")
  if (grepl("(historia|museo|colonial|ruinas|arqueolog|cultura|fiesta|festividad|carnaval|iglesia|patrimonio|tradicion|baile|danza)", n)) return("Historia y Museos")
  NULL
}

extraer_dias <- function(q) {
  m <- str_match(norm(q), "(\\d+) ?dias?")[, 2]
  if (!is.na(m)) return(min(max(as.integer(m), 1), 7))
  if (grepl("fin de semana", norm(q))) return(2L)
  3L
}

buscar_destinos <- function(q, df, n = 3) {
  qn <- str_squish(norm(q))
  palabras <- unlist(strsplit(qn, " "))
  palabras <- unique(palabras[nchar(palabras) > 2 & !(palabras %in% STOP_ES)])
  if (length(palabras) == 0) return(df[0, ])
  
  crudo <- vapply(seq_len(nrow(df)), function(i) {
    t <- df$.titulo_n[i]
    x <- df$.texto_n[i]
    bono <- if (nchar(t) > 2 && grepl(t, qn, fixed = TRUE)) (if (t == "bolivia") 1 else 8) else 0
    tit  <- sum(vapply(palabras, function(p) 3 * as.numeric(grepl(p, t, fixed = TRUE)), numeric(1)))
    txt  <- sum(vapply(palabras, function(p) min(str_count(x, fixed(p)), 5), numeric(1)))
    bono + tit + txt
  }, numeric(1))
  
  if (max(crudo) < 1) return(df[0, ])
  rel <- crudo / max(crudo)
  auth <- df$authority / max(c(df$authority, 1e-9))
  final <- ifelse(crudo > 0, 0.75 * rel + 0.15 * (df$pagerank_10 / 10) + 0.10 * auth, 0)
  
  idx <- order(final, decreasing = TRUE)
  idx <- idx[final[idx] > 0][seq_len(min(n, sum(final > 0)))]
  out <- df[idx, ]
  out$.score <- final[idx]
  out$.crudo <- crudo[idx]
  out
}

top_por_pagerank <- function(df, categoria = NULL, n = 5) {
  d <- df[df$nombre_nodo != "Bolivia", ]
  if (!is.null(categoria)) d <- d[d$cluster == categoria, ]
  d <- d[order(d$pagerank_real, d$enlaces_entrantes, decreasing = TRUE), ]
  head(d, n)
}

construir_contexto <- function(docs) {
  if (nrow(docs) == 0) return("")
  paste(vapply(seq_len(nrow(docs)), function(i) {
    d <- docs[i, ]
    sprintf("### %s\nCategoría (K-Means): %s | PageRank 0-10: %s | Sentimiento: %s | Alertas: %s\nLugares mencionados (NER): %s\nTexto: %s",
            d$nombre_nodo, d$cluster, d$pagerank_10, d$sentimiento_base,
            ifelse(nzchar(d$alertas), d$alertas, "ninguna"), d$entidades_ner,
            substr(d$texto_indexado, 1, 1100))
  }, character(1)), collapse = "\n\n")
}

SYSTEM_PROMPT <- paste0(
  "Eres ", NOMBRE_BOT, ", un guía turístico virtual de Bolivia, cálido, cercano y entusiasta. ",
  "Hablas en español natural, nunca robótico.\n",
  "1. Básate en <contexto_minado>: viene de Wikipedia/Wikivoyage procesado con minería web.\n",
  "2. Si hay 'Alertas', adviértelo con amabilidad.\n",
  "3. Responde corto: máximo 130 palabras."
)

llamar_claude <- function(mensaje, historial, contexto = "") {
  key <- Sys.getenv("ANTHROPIC_API_KEY")
  if (!nzchar(key)) return(NULL)
  ult <- tail(historial, 8)
  contenido <- if (nzchar(contexto)) {
    paste0("<contexto_minado>\n", contexto, "\n</contexto_minado>\n\nMensaje del turista: ", mensaje)
  } else {
    mensaje
  }
  msgs <- c(ult, list(list(role = "user", content = contenido)))
  cuerpo <- list(model = Sys.getenv("ANTHROPIC_MODEL", "claude-sonnet-4-6"), max_tokens = 700, system = SYSTEM_PROMPT, messages = msgs)
  res <- tryCatch(
    POST("https://api.anthropic.com/v1/messages", add_headers(`x-api-key` = key, `anthropic-version` = "2023-06-01"),
         content_type_json(), timeout(45), body = toJSON(cuerpo, auto_unbox = TRUE)),
    error = function(e) NULL
  )
  if (is.null(res)) return(NULL)
  if (status_code(res) != 200) return(NULL)
  js <- fromJSON(content(res, "text", encoding = "UTF-8"), simplifyVector = FALSE)
  bloques <- Filter(function(b) identical(b$type, "text"), js$content)
  if (length(bloques) == 0) return(NULL)
  texto_final <- trimws(paste(vapply(bloques, function(b) b$text, character(1)), collapse = "\n"))
  if (nzchar(texto_final)) texto_final else NULL
}

primera_frase <- function(txt, max = 170) {
  f <- str_extract(txt, "^[^.]{20,}\\.")
  f <- ifelse(is.na(f), substr(txt, 1, max), f)
  if (nchar(f) > max) paste0(substr(f, 1, max - 1), "…") else f
}

elegir <- function(...) {
  v <- c(...)
  v[sample.int(length(v), 1)]
}

respuesta_local <- function(intencion, docs, df, categoria = NULL, dias = 3L) {
  ejemplos <- paste(head(sample(df$nombre_nodo[df$nombre_nodo != "Bolivia"]), 4), collapse = ", ")
  switch(intencion,
    saludo = elegir(
      paste0("¡Hola! Soy ", NOMBRE_BOT, ", tu guía virtual por Bolivia 🇧🇴. Puedo hablarte de lugares, fiestas, platos típicos o armarte una ruta. ¿Qué te gustaría conocer?"),
      paste0("¡Buenas! Qué gusto saludarte. Soy ", NOMBRE_BOT, ". Pregúntame por ", ejemplos, " o pídeme una ruta. ¿Por dónde empezamos?")),
    despedida = "¡Hasta pronto! Que tengas un viaje increíble por Bolivia. Aquí estaré cuando quieras planear otro rincón 🏔️",
    gracias = elegir("¡Con mucho gusto! ¿Quieres que te sugiera algo más para tu viaje?",
                     "¡De nada! Si quieres, te armo una ruta o te cuento qué se come por ahí."),
    estado = "¡Muy bien, gracias por preguntar! Listo para ayudarte a planear tu viaje. ¿Qué lugar de Bolivia te llama la atención?",
    identidad = paste0("Soy ", NOMBRE_BOT, ", un chatbot de turismo, cultura y gastronomía de Bolivia basado en Minería Web en R."),
    ranking = {
      if (nrow(docs) == 0) return("Todavía no tengo datos para armar ese ranking.")
      lista <- paste(sprintf("%d) %s (PageRank %s/10)", seq_len(nrow(docs)), docs$nombre_nodo, docs$pagerank_10), collapse = "; ")
      paste0("Los destinos más enlazados", if (!is.null(categoria)) paste0(" en ", categoria) else "", " son: ", lista, ". ¿Quieres que te cuente de alguno?")
    },
    ruta = {
      if (nrow(docs) == 0) {
        return(paste0("Para armarte una ruta dime qué te gusta o a qué zona vas. Por ejemplo: ", ejemplos, "."))
      }
      n <- min(nrow(docs), dias)
      lineas <- vapply(seq_len(n), function(i) {
        sprintf("Día %d (%s): %s", i, docs$nombre_nodo[i], primera_frase(docs$resumen[i], 130))
      }, character(1))
      paste0("Te propongo esta ruta de ", dias, " días:\n", paste(lineas, collapse = "\n"), "\n¿La ajustamos a tu estilo?")
    },
    comparar = {
      if (nrow(docs) < 2) {
        return("Dime los dos lugares que quieres comparar, por ejemplo: 'Uyuni vs Copacabana'.")
      }
      paste0(
        docs$nombre_nodo[1], " (", docs$cluster[1], ", PageRank ", docs$pagerank_10[1], "): ", primera_frase(docs$resumen[1], 130), "\n",
        docs$nombre_nodo[2], " (", docs$cluster[2], ", PageRank ", docs$pagerank_10[2], "): ", primera_frase(docs$resumen[2], 130), "\n",
        "¿Cuál se acerca más a lo que buscas?"
      )
    },
    {
      if (nrow(docs) == 0) {
        return(paste0("No encontré eso en mi base de conocimiento. Puedo ayudarte con destinos como ", ejemplos, ". ¿Probamos con alguno?"))
      }
      d <- docs[1, ]
      txt <- paste0("Sobre **", d$nombre_nodo, "**: ", substr(d$resumen, 1, 480))
      if (nzchar(d$alertas)) {
        txt <- paste0(txt, "\n⚠️ Ten en cuenta: ", d$alertas, ".")
      }
      paste0(txt, "\n¿Quieres que te arme una ruta o te cuente algo más?")
    }
  )
}

sugerencias_para <- function(intencion, docs) {
  if (nrow(docs) > 0 && intencion %in% c("consulta", "riesgos", "comparar")) {
    d <- docs$nombre_nodo[1]
    return(c(paste0("¿Qué riesgos tiene ", d, "?"), paste0("¿Qué se come cerca de ", d, "?"), "Armame una ruta de 3 días"))
  }
  c("¿Cuáles son los destinos más populares?", "¿Qué platos típicos hay?", "Armame una ruta de 3 días por La Paz")
}

procesar_mensaje <- function(mensaje, sesion_id, canal, df, registrar = NULL) {
  mensaje <- str_squish(mensaje)
  if (!nzchar(mensaje)) mensaje <- "hola"
  if (!nzchar(sesion_id)) sesion_id <- paste0("ses_", as.integer(Sys.time()))
  s <- obtener_sesion(sesion_id)
  
  intencion <- detectar_intencion(mensaje)
  categoria <- detectar_categoria(mensaje)
  n_msg <- str_squish(norm(mensaje))
  
  docs <- df[0, ]
  dias <- 3L
  if (intencion == "ranking") {
    docs <- top_por_pagerank(df, categoria, 5)
  } else if (intencion == "ruta") {
    dias <- extraer_dias(mensaje)
    docs <- buscar_destinos(mensaje, df, n = max(dias, 3))
    if (nrow(docs) < min(dias, 3)) {
      docs <- rbind(docs[, names(df)], top_por_pagerank(df, categoria, 5)[, names(df)])
    }
    docs <- docs[!duplicated(docs$nombre_nodo), ]
  } else if (intencion %in% c("consulta", "riesgos", "comparar")) {
    docs <- buscar_destinos(mensaje, df, n = if (intencion == "comparar") 2 else 3)
    seguimiento <- grepl("\\b(alli|ahi|alla|ese lugar|esa ciudad|ese sitio|el mismo|cerca de ahi|y que|y donde)\\b", n_msg)
    if (length(setdiff(unlist(strsplit(n_msg, " ")), STOP_ES)) <= 2) {
      seguimiento <- TRUE
    }
    usar_previo <- (nrow(docs) == 0)
    if (!usar_previo && seguimiento && docs$.crudo[1] < 8) {
      usar_previo <- TRUE
    }
    if (!is.null(s$ultimo_destino) && usar_previo) {
      previo <- df[df$nombre_nodo == s$ultimo_destino, ]
      if (nrow(previo) > 0) {
        extra <- buscar_destinos(paste(mensaje, s$ultimo_destino), df, n = 2)
        docs <- rbind(previo[, names(df)], extra[!extra$nombre_nodo %in% previo$nombre_nodo, names(df)])
      }
    }
  }
  
  contexto <- construir_contexto(docs)
  texto <- llamar_claude(mensaje, s$historial, contexto)
  modo <- "claude"
  if (is.null(texto)) {
    texto <- respuesta_local(intencion, docs, df, categoria, dias)
    modo <- "local"
  }
  
  s$historial <- c(s$historial, list(list(role = "user", content = mensaje),
                                     list(role = "assistant", content = texto)))
  s$historial <- tail(s$historial, 16)
  if (nrow(docs) > 0 && intencion %in% c("consulta", "riesgos", "comparar", "ruta")) {
    s$ultimo_destino <- docs$nombre_nodo[1]
  }
  s$turnos <- s$turnos + 1
  guardar_sesion(sesion_id, s)
  
  principal <- if (nrow(docs) > 0) docs[1, ] else NULL
  
  if (!is.null(registrar)) {
    tryCatch(registrar(mensaje, if (!is.null(principal)) principal$nombre_nodo else NA,
                       if (!is.null(principal)) principal$cluster else NA,
                       if (!is.null(principal)) principal$sentimiento_base else NA,
                       sesion_id, canal, intencion),
             error = function(e) message("[log] ", conditionMessage(e)))
  }
  
  fuentes_lista <- if (nrow(docs) > 0 && intencion %in% c("consulta", "riesgos", "comparar", "ruta", "ranking")) {
    lapply(seq_len(min(nrow(docs), 3)), function(i) {
      d <- docs[i, ]
      list(nombre = d$nombre_nodo, url = d$id_url, cluster = d$cluster,
           sentimiento = d$sentimiento_base, pagerank_10 = d$pagerank_10,
           authority = d$authority, hub = d$hub, enlaces_entrantes = d$enlaces_entrantes)
    })
  } else list()
  
  lugares_mapa <- if (nrow(docs) > 0) geocodificar_entidades(docs) else list()
  
  alertas_lista <- if (!is.null(principal) && nzchar(principal$alertas)) {
    as.list(unlist(strsplit(principal$alertas, ",\\s*")))
  } else list()
  
  list(
    respuesta    = texto,
    session_id   = sesion_id,
    intencion    = intencion,
    modo         = modo,
    fuentes      = fuentes_lista,
    lugares      = lugares_mapa,
    alertas      = alertas_lista,
    sugerencias  = as.list(sugerencias_para(intencion, docs))
  )
}