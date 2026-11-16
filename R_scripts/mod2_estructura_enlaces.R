# ==============================================================================
# MÓDULO 2: MINERÍA DE ESTRUCTURA WEB (tidygraph + PageRank + HITS Authority/Hub)
# Archivo: R_scripts/mod2_estructura_enlaces.R
# ==============================================================================
library(igraph)
library(tidygraph)
library(dplyr)
library(ggraph)
library(ggplot2)
source("R_scripts/conexion_bd.R")

if (!dir.exists("web/img_grafos")) dir.create("web/img_grafos", recursive = TRUE)

ejecutar_modulo2 <- function() {
  cat("1. Descargando nodos y aristas desde Supabase...\n")
  con <- conectar_supabase()
  nodos_df <- dbGetQuery(con, "SELECT titulo AS name, id_url FROM indice_nodos")
  aristas_df <- dbGetQuery(con, "SELECT from_nodo AS from, to_nodo AS to FROM enlaces_aristas")
  
  # Crear grafo dirigido y convertir a tidygraph (Igual que DMCap5_4.R y DMCap_5_5.R)
  grafo_igraph <- graph_from_data_frame(d = aristas_df, vertices = nodos_df, directed = TRUE)
  
  grafo_tidy <- as_tbl_graph(grafo_igraph) %>%
    activate(nodes) %>%
    mutate(
      nombre_nodo = as.character(name),
      enlaces_entrantes = as.integer(centrality_degree(mode = "in")),
      enlaces_salientes = as.integer(centrality_degree(mode = "out")),
      pagerank_real = round(centrality_pagerank(damping = 0.85), 4),
      pagerank_10 = round((pagerank_real / max(pagerank_real)) * 10, 2),
      authority = round(centrality_authority(), 4),
      hub = round(centrality_hub(), 4)
    )
  
  tabla_estructura <- grafo_tidy %>%
    activate(nodes) %>%
    as_tibble() %>%
    select(nombre_nodo, id_url, enlaces_entrantes, enlaces_salientes, pagerank_real, pagerank_10, authority, hub) %>%
    arrange(desc(pagerank_real))
  
  # Guardar en la tabla 'metricas_mineria' de Supabase
  dbExecute(con, "TRUNCATE TABLE metricas_mineria;")
  dbWriteTable(con, "metricas_mineria", tabla_estructura, append = TRUE, row.names = FALSE)
  dbDisconnect(con)
  
  cat("\n=== TABLA GUARDADA EN SUPABASE (metricas_mineria) ===\n")
  print(tabla_estructura %>% select(nombre_nodo, enlaces_entrantes, pagerank_real, pagerank_10, authority, hub))
  
  # Generar las 3 gráficas de red (PageRank, Authority y Hub)
  set.seed(42)
  
  p_pr <- ggraph(grafo_tidy, layout = "fr") +
    geom_edge_link(arrow = arrow(length = unit(3, "mm"), type = "closed"), end_cap = circle(4, "mm"), edge_colour = "#94a3b8") +
    geom_node_point(aes(size = pagerank_real, color = pagerank_real)) +
    geom_node_text(aes(label = nombre_nodo), repel = TRUE, size = 3.8, fontface = "bold") +
    scale_size_continuous(range = c(4, 14)) +
    labs(title = "Grafo Dirigido - Algoritmo PageRank", subtitle = "Tamaño del nodo según su PageRank") +
    theme_graph(base_family = "sans")
  ggsave("web/img_grafos/grafo_pagerank.png", p_pr, width = 8, height = 5.5, dpi = 150)
  
  p_auth <- ggraph(grafo_tidy, layout = "fr") +
    geom_edge_link(arrow = arrow(length = unit(3, "mm"), type = "closed"), end_cap = circle(4, "mm"), edge_colour = "#94a3b8") +
    geom_node_point(aes(size = authority), color = "#dc2626") +
    geom_node_text(aes(label = nombre_nodo), repel = TRUE, size = 3.8, fontface = "bold") +
    scale_size_continuous(range = c(4, 14)) +
    labs(title = "Algoritmo HITS - Authority (Autoridad)", subtitle = "Fuentes principales de información") +
    theme_graph(base_family = "sans")
  ggsave("web/img_grafos/grafo_authority.png", p_auth, width = 8, height = 5.5, dpi = 150)
  
  p_hub <- ggraph(grafo_tidy, layout = "fr") +
    geom_edge_link(arrow = arrow(length = unit(3, "mm"), type = "closed"), end_cap = circle(4, "mm"), edge_colour = "#94a3b8") +
    geom_node_point(aes(size = hub), color = "#16a34a") +
    geom_node_text(aes(label = nombre_nodo), repel = TRUE, size = 3.8, fontface = "bold") +
    scale_size_continuous(range = c(4, 14)) +
    labs(title = "Algoritmo HITS - Hub (Concentrador)", subtitle = "Páginas que apuntan a muchas autoridades") +
    theme_graph(base_family = "sans")
  ggsave("web/img_grafos/grafo_hub.png", p_hub, width = 8, height = 5.5, dpi = 150)
  
  cat("\n2. ¡Gráficas exportadas en 'web/img_grafos/' y métricas subidas a Supabase!\n")
}

ejecutar_modulo2()