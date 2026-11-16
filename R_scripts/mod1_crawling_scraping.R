# ==============================================================================
# MÓDULO 1 (V2): CRAWLING + SCRAPING MULTIFUENTE (Wikipedia + Wikivoyage)
# Archivo: R_scripts/mod1_crawling_scraping.R
#  - ~45 destinos: naturaleza, historia/museos, fiestas y gastronomía
#  - Resuelve títulos automáticamente con la API de Wikipedia si el enlace falla
#  - Detecta enlaces entre destinos por URL real (más preciso que grepl)
#  - Guarda resumen, imagen y fuente para mostrar tarjetas visuales en el chat
# ==============================================================================
library(rvest)
library(httr)
library(xml2)
library(stringr)
library(jsonlite)
source("R_scripts/conexion_bd.R")

UA <- "Mozilla/5.0 (compatible; ChatbotTurismoBolivia/2.0; proyecto academico UMSA)"

# nombre = cómo se llama el nodo | wp = título en Wikipedia | wv = página en Wikivoyage (opcional)
# alias  = otros títulos de Wikipedia que cuentan como enlace hacia este nodo
destinos_info <- list(
  # --- General y ciudades ---
  list(nombre = "Bolivia", wp = "Bolivia", wv = "Bolivia"),
  list(nombre = "La Paz", wp = "La Paz", wv = "La Paz"),
  list(nombre = "Sucre", wp = "Sucre", wv = "Sucre"),
  list(nombre = "Potosí", wp = "Potosí", wv = "Potosí"),
  list(nombre = "Cochabamba", wp = "Cochabamba", wv = "Cochabamba"),
  list(nombre = "Santa Cruz de la Sierra", wp = "Santa Cruz de la Sierra", wv = "Santa Cruz de la Sierra"),
  list(nombre = "Oruro", wp = "Oruro", wv = "Oruro"),
  list(nombre = "Tarija", wp = "Tarija", wv = "Tarija"),
  list(nombre = "Copacabana", wp = "Copacabana (Bolivia)", wv = "Copacabana", alias = c("Copacabana")),
  list(nombre = "Uyuni", wp = "Uyuni", wv = "Uyuni"),
  list(nombre = "Rurrenabaque", wp = "Rurrenabaque", wv = "Rurrenabaque"),
  list(nombre = "Coroico", wp = "Coroico"),
  # --- Atractivos naturales ---
  list(nombre = "Salar de Uyuni", wp = "Salar de Uyuni", alias = c("Salar_de_Uyuni")),
  list(nombre = "Lago Titicaca", wp = "Lago Titicaca"),
  list(nombre = "Isla del Sol", wp = "Isla del Sol"),
  list(nombre = "Parque Nacional Madidi", wp = "Parque nacional Madidi"),
  list(nombre = "Parque Nacional Sajama", wp = "Parque nacional Sajama"),
  list(nombre = "Laguna Colorada", wp = "Laguna Colorada"),
  list(nombre = "Reserva Eduardo Avaroa", wp = "Reserva Nacional de Fauna Andina Eduardo Avaroa"),
  list(nombre = "Valle de la Luna", wp = "Valle de la Luna (Bolivia)"),
  list(nombre = "Parque Nacional Torotoro", wp = "Parque nacional Torotoro"),
  list(nombre = "Parque Noel Kempff Mercado", wp = "Parque nacional Noel Kempff Mercado"),
  list(nombre = "Yungas", wp = "Yungas"),
  list(nombre = "Camino de la Muerte", wp = "Camino de la Muerte"),
  list(nombre = "Illimani", wp = "Illimani"),
  list(nombre = "Huayna Potosí", wp = "Huayna Potosí"),
  list(nombre = "Chacaltaya", wp = "Chacaltaya"),
  # --- Historia, museos y patrimonio ---
  list(nombre = "Tiahuanaco", wp = "Tiahuanaco", alias = c("Tiwanaku")),
  list(nombre = "Cerro Rico de Potosí", wp = "Cerro Rico"),
  list(nombre = "Casa de la Libertad", wp = "Casa de la Libertad"),
  list(nombre = "Misiones Jesuíticas de Chiquitos", wp = "Misiones jesuíticas de Chiquitos"),
  list(nombre = "Fuerte de Samaipata", wp = "Fuerte de Samaipata"),
  list(nombre = "Museo Nacional de Arte", wp = "Museo Nacional de Arte (Bolivia)"),
  list(nombre = "Mercado de las Brujas", wp = "Mercado de las Brujas"),
  # --- Fiestas y tradiciones ---
  list(nombre = "Carnaval de Oruro", wp = "Carnaval de Oruro"),
  list(nombre = "Gran Poder", wp = "Festividad del Señor del Gran Poder"),
  list(nombre = "Alasita", wp = "Alasita"),
  list(nombre = "Urkupiña", wp = "Virgen de Urkupiña"),
  # --- Gastronomía ---
  list(nombre = "Gastronomía de Bolivia", wp = "Gastronomía de Bolivia"),
  list(nombre = "Salteña", wp = "Salteña"),
  list(nombre = "Silpancho", wp = "Silpancho"),
  list(nombre = "Pique macho", wp = "Pique macho"),
  list(nombre = "Sopa de maní", wp = "Sopa de maní"),
  list(nombre = "Api", wp = "Api (bebida)"),
  list(nombre = "Chuño", wp = "Chuño"),
  list(nombre = "Chicha", wp = "Chicha")
)

# ------------------------------------------------------------------------------
# Utilidades HTTP
# ------------------------------------------------------------------------------
http_get <- function(url, ...) {
  Sys.sleep(0.3)  # cortesía con el servidor
  tryCatch(GET(url, user_agent(UA), timeout(25), ...), error = function(e) NULL)
}

url_titulo <- function(base, titulo) {
  paste0(base, URLencode(gsub(" ", "_", titulo), reserved = FALSE))
}

ok <- function(res) !is.null(res) && status_code(res) == 200

# Intenta el título exacto; si falla, usa el buscador de Wikipedia para resolverlo
resolver_wikipedia <- function(d) {
  res <- http_get(url_titulo("https://es.wikipedia.org/wiki/", d$wp))
  if (ok(res)) return(res)
  
  api <- http_get("https://es.wikipedia.org/w/api.php",
                  query = list(action = "query", list = "search", srsearch = d$nombre,
                               format = "json", srlimit = 1))
  if (!ok(api)) return(NULL)
  js <- tryCatch(fromJSON(content(api, "text", encoding = "UTF-8")), error = function(e) NULL)
  hits <- js$query$search
  if (is.null(hits) || nrow(hits) == 0) return(NULL)
  cat("   [~] '", d$nombre, "' resuelto vía búsqueda como: ", hits$title[1], "\n", sep = "")
  res2 <- http_get(url_titulo("https://es.wikipedia.org/wiki/", hits$title[1]))
  if (ok(res2)) res2 else NULL
}

limpiar_texto <- function(x) {
  x <- gsub("\\[[^\\]]*\\]", "", x, perl = TRUE)
  str_squish(x)
}

# ------------------------------------------------------------------------------
# Extracción
# ------------------------------------------------------------------------------
extraer_wikipedia <- function(res, max_parrafos = 12) {
  html <- read_html(content(res, "text", encoding = "UTF-8"))
  
  parrafos <- html %>% html_nodes("#mw-content-text p") %>% html_text(trim = TRUE)
  parrafos <- limpiar_texto(parrafos)
  parrafos <- parrafos[nchar(parrafos) > 70]
  
  imagen <- tryCatch(html %>% html_node("meta[property='og:image']") %>% html_attr("content"),
                     error = function(e) NA_character_)
  hrefs <- na.omit(html %>% html_nodes("#mw-content-text a") %>% html_attr("href"))
  
  list(parrafos = head(parrafos, max_parrafos), imagen = imagen, hrefs = hrefs)
}

extraer_wikivoyage <- function(titulo, max_items = 10) {
  res <- http_get(url_titulo("https://es.wikivoyage.org/wiki/", titulo))
  if (!ok(res)) return(character(0))
  html <- read_html(content(res, "text", encoding = "UTF-8"))
  items <- html %>% html_nodes(".mw-parser-output p, .mw-parser-output li") %>% html_text(trim = TRUE)
  items <- limpiar_texto(items)
  items <- items[nchar(items) > 60 & nchar(items) < 600]
  head(unique(items), max_items)
}

# ------------------------------------------------------------------------------
# Módulo principal
# ------------------------------------------------------------------------------
ejecutar_modulo1 <- function(destinos) {
  cat("1. Iniciando Spider multifuente (Wikipedia + Wikivoyage)...\n")
  
  paginas <- list()   # nombre -> datos de la página
  
  for (d in destinos) {
    tryCatch({
      res <- resolver_wikipedia(d)
      if (is.null(res)) {
        cat("   [SALTADO] No se pudo obtener:", d$nombre, "\n")
        next
      }
      wp <- extraer_wikipedia(res)
      if (length(wp$parrafos) == 0) {
        cat("   [SALTADO] Sin texto útil:", d$nombre, "\n")
        next
      }
      wv <- if (!is.null(d$wv)) extraer_wikivoyage(d$wv) else character(0)
      
      texto <- paste(c(wp$parrafos, wv), collapse = " ")
      paginas[[d$nombre]] <- list(
        info    = d,
        url     = res$url,
        resumen = substr(paste(head(wp$parrafos, 2), collapse = " "), 1, 650),
        texto   = texto,
        imagen  = ifelse(is.na(wp$imagen), "", wp$imagen),
        fuente  = ifelse(length(wv) > 0, "Wikipedia + Wikivoyage", "Wikipedia"),
        hrefs   = wp$hrefs
      )
      cat("   [OK]", d$nombre, "(", ifelse(length(wv) > 0, "WP+WV", "WP"), ")\n")
    }, error = function(e) cat("   [ERROR]", d$nombre, ":", conditionMessage(e), "\n"))
  }
  
  if (length(paginas) < 5) stop("Se obtuvieron muy pocas páginas; revisa tu conexión a Internet.")
  
  # --- Nodos -----------------------------------------------------------------
  indice_nodos <- do.call(rbind, lapply(names(paginas), function(n) {
    p <- paginas[[n]]
    data.frame(id_url = p$url, titulo = n, texto_indexado = p$texto,
               estado_enlace = "200 OK (Válido)", resumen = p$resumen,
               imagen_url = p$imagen, fuente = p$fuente, stringsAsFactors = FALSE)
  }))
  indice_nodos <- indice_nodos[!duplicated(indice_nodos$id_url), ]
  
  # --- Aristas: enlaces reales entre destinos --------------------------------
    # --- Aristas: enlaces reales entre destinos --------------------------------
  norm_ruta <- function(h) {
    h <- sub("^https?://es\\.wikipedia\\.org", "", h)
    if (startsWith(h, "./")) h <- paste0("/wiki/", substring(h, 3))
    if (!startsWith(h, "/wiki/")) return(NA_character_)
    h <- sub("[#?].*$", "", h)
    x <- tryCatch(URLdecode(h), error = function(e) h)
    Encoding(x) <- "UTF-8"
    tolower(gsub(" ", "_", x))
  }
  
  mapa <- c()  # ruta normalizada -> nombre del nodo
  for (n in indice_nodos$titulo) {
    p <- paginas[[n]]
    mapa[norm_ruta(sub("^https?://[^/]+", "", p$url))] <- n
    for (a in p$info$alias) mapa[norm_ruta(paste0("/wiki/", a))] <- n
  }
  
  filas <- list()
  for (n in indice_nodos$titulo) {
    rutas <- unique(na.omit(vapply(paginas[[n]]$hrefs, norm_ruta, character(1), USE.NAMES = FALSE)))
    enlazados <- setdiff(unique(unname(mapa[intersect(rutas, names(mapa))])), n)
    cat("   ", n, ": ", length(paginas[[n]]$hrefs), " hrefs, ", length(enlazados), " enlaces a otros destinos\n", sep = "")
    if (length(enlazados) > 0)
      filas[[length(filas) + 1]] <- data.frame(from_nodo = n, to_nodo = enlazados, stringsAsFactors = FALSE)
  }
  if (length(filas) == 0) stop("No se detectaron enlaces entre destinos; revisa el selector de enlaces.")
  enlaces_aristas <- unique(do.call(rbind, filas))
  
  cat("2. Subiendo", nrow(indice_nodos), "nodos y", nrow(enlaces_aristas), "enlaces a Supabase...\n")
  con <- conectar_supabase()
  on.exit(dbDisconnect(con), add = TRUE)
  
  dbExecute(con, "TRUNCATE TABLE indice_nodos, enlaces_aristas RESTART IDENTITY;")
  dbWriteTable(con, "indice_nodos", indice_nodos, append = TRUE, row.names = FALSE)
  dbWriteTable(con, "enlaces_aristas", enlaces_aristas, append = TRUE, row.names = FALSE)
  
  cat("¡ÉXITO! Se subieron", nrow(indice_nodos), "nodos y", nrow(enlaces_aristas), "enlaces reales.\n")
}

ejecutar_modulo1(destinos_info)
