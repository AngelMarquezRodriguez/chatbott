-- ==============================================================================
-- MIGRACIÓN V2 - Ejecutar UNA vez en Supabase > SQL Editor
-- Es seguro ejecutarla varias veces (IF NOT EXISTS).
-- ==============================================================================

-- Nuevas columnas del índice (imagen, resumen, fuente)
ALTER TABLE indice_nodos ADD COLUMN IF NOT EXISTS resumen     TEXT;
ALTER TABLE indice_nodos ADD COLUMN IF NOT EXISTS imagen_url  TEXT;
ALTER TABLE indice_nodos ADD COLUMN IF NOT EXISTS fuente      TEXT;

-- Alertas de riesgo detectadas por análisis de sentimiento
ALTER TABLE metricas_mineria ADD COLUMN IF NOT EXISTS alertas TEXT;

-- Logs: sesión, canal (texto / voz) e intención detectada
ALTER TABLE logs_uso_chatbot ADD COLUMN IF NOT EXISTS sesion_id  TEXT;
ALTER TABLE logs_uso_chatbot ADD COLUMN IF NOT EXISTS canal      TEXT;
ALTER TABLE logs_uso_chatbot ADD COLUMN IF NOT EXISTS intencion  TEXT;

-- Índices para acelerar el ranking en vivo
CREATE INDEX IF NOT EXISTS idx_logs_destino ON logs_uso_chatbot (destino_recuperado);
