library(plumber)

# Si quieres volver a correr todo el scraping desde cero, cambia FALSE por TRUE:
RECALCULAR_MINERIA <- FALSE

if (RECALCULAR_MINERIA) {
  source("R_scripts/mod1_crawling_scraping.R", encoding = "UTF-8")
  source("R_scripts/mod2_estructura_enlaces.R", encoding = "UTF-8")
  source("R_scripts/mod3_contenido_pln_clustering.R", encoding = "UTF-8")
  source("R_scripts/mod4_google_trends.R", encoding = "UTF-8")
  source("R_scripts/mod5_mineria_uso_logs.R", encoding = "UTF-8")
}

cat("\n======================================================\n")
cat("  CHATBOT WAYRA ACTIVO EN: http://127.0.0.1:8000\n")
cat("  Abre ahora 'web/index.html' en tu navegador (Chrome o Edge para usar el micrófono)\n")
cat("======================================================\n")

pr <- plumb("R_scripts/mod6_servidor_api.R")
pr <- pr_static(pr, "/app", "web")
pr$run(host = "127.0.0.1", port = 8000)
