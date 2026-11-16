# Ejecutar UNA sola vez:  source("R_scripts/librerias.R")
options(repos = c(CRAN = "https://cloud.r-project.org"))

install.packages(c(
  # Base de datos
  "DBI", "RPostgres",
  # Crawling / scraping
  "rvest", "httr", "xml2", "stringr", "stringi", "jsonlite",
  # Estructura (grafos)
  "igraph", "tidygraph", "dplyr", "ggraph", "ggplot2",
  # Contenido (PLN)
  "tm",
  # Tendencias
  "gtrendsR",
  # API del chatbot
  "plumber"
))

# --- OPCIONAL: NER real con spaCy (requiere Python) ---
# install.packages("spacyr")
# spacyr::spacy_install()                       # instala spaCy
# spacyr::spacy_download_langmodel("es_core_news_md")
# Si no lo instalas, el módulo 3 usa un NER propio (gazetteer + patrones) automáticamente.

# Si usas renv, después de instalar ejecuta:  renv::snapshot()
