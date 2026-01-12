# =============================================================================
# 02_demo_proximity_buffers.R
# Análisis Geoespacial en R - Demo 2: Análisis de Proximidad con Buffers
# =============================================================================
# Objetivo: Usar buffers para analizar proximidad entre infraestructura
# (hospitales) y eventos, evaluando exposición y sensibilidad a la distancia
# =============================================================================

# -----------------------------------------------------------------------------
# 0. WORKDIR + LIBRERÍAS + OPCIONES + CARPETAS
# -----------------------------------------------------------------------------
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(ggplot2)
  library(tidyr)
  library(scales)
  library(readr)
})

options(scipen = 999)
sf_use_s2(FALSE)

dir.create("data", showWarnings = FALSE)
dir.create("outputs", showWarnings = FALSE)

cat("✓ Directorio configurado, librerías cargadas y carpetas listas\n")

# -----------------------------------------------------------------------------
# 1. CARGAR OBJETOS (ESTILO DEMO 1 + FALLBACK SI FALTAN _UTM)
# -----------------------------------------------------------------------------
cat("\n", strrep("=", 70), "\n", sep = "")
cat("PREPARACIÓN: CARGA DE OBJETOS + CRS PROYECTADO PARA BUFFERS\n")
cat(strrep("=", 70), "\n\n", sep = "")

cat("→ Cargando objetos del setup...\n")

rds_paths <- c(
  poligonos_utm   = "data/poligonos_utm.rds",
  hospitales_utm  = "data/hospitales_utm.rds",
  eventos_utm     = "data/eventos_utm.rds",
  poligonos       = "data/poligonos_clean.rds",
  hospitales      = "data/hospitales_clean.rds",
  eventos         = "data/eventos_clean.rds"
)

estado_archivos <- data.frame(
  archivo = names(rds_paths),
  path = unname(rds_paths),
  existe = file.exists(rds_paths),
  stringsAsFactors = FALSE
)
print(estado_archivos)

# Los WGS84 deben existir sí o sí
if (!all(file.exists(rds_paths[c("poligonos","hospitales","eventos")]))) {
  faltan <- names(rds_paths[c("poligonos","hospitales","eventos")])[
    !file.exists(rds_paths[c("poligonos","hospitales","eventos")])
  ]
  stop(
    "❌ Faltan RDS base (WGS84): ", paste(faltan, collapse = ", "),
    "\n   Corre primero 00_setup_and_sanity_checks.R o revisa setwd()."
  )
}

# Cargar WGS84
poligonos  <- readRDS(rds_paths["poligonos"])
hospitales <- readRDS(rds_paths["hospitales"])
eventos    <- readRDS(rds_paths["eventos"])

# Si faltan los UTM, crearlos y guardarlos
if (!all(file.exists(rds_paths[c("poligonos_utm","hospitales_utm","eventos_utm")]))) {
  cat("\n⚠ No encontré uno o más *_utm.rds. Los crearé desde WGS84...\n")
  crs_proyectado <- 32719  # UTM 19S
  
  poligonos_utm   <- st_transform(poligonos, crs_proyectado)
  hospitales_utm  <- st_transform(hospitales, crs_proyectado)
  eventos_utm     <- st_transform(eventos, crs_proyectado)
  
  saveRDS(poligonos_utm,  rds_paths["poligonos_utm"])
  saveRDS(hospitales_utm, rds_paths["hospitales_utm"])
  saveRDS(eventos_utm,    rds_paths["eventos_utm"])
  
  cat("✓ UTM creados y guardados en data/\n")
} else {
  poligonos_utm  <- readRDS(rds_paths["poligonos_utm"])
  hospitales_utm <- readRDS(rds_paths["hospitales_utm"])
  eventos_utm    <- readRDS(rds_paths["eventos_utm"])
  cat("✓ UTM cargados desde RDS\n")
}

cat("\n✓ Datos cargados:\n")
cat("  - Polígonos:", nrow(poligonos), "zonas\n")
cat("  - Hospitales:", nrow(hospitales), "puntos\n")
cat("  - Eventos:", nrow(eventos), "puntos\n")

# Checks de columnas mínimas
req_pol_cols <- c("zona_id", "zona_nombre", "poblacion")
miss_pol <- setdiff(req_pol_cols, names(poligonos))
if (length(miss_pol) > 0) stop("❌ 'poligonos' sin columnas requeridas: ", paste(miss_pol, collapse = ", "))

req_hosp_cols <- c("hospital_id", "nombre", "tipo")
miss_hosp <- setdiff(req_hosp_cols, names(hospitales))
if (length(miss_hosp) > 0) stop("❌ 'hospitales' sin columnas requeridas: ", paste(miss_hosp, collapse = ", "))

req_evt_cols <- c("evento_id", "tipo", "severidad")
miss_evt <- setdiff(req_evt_cols, names(eventos))
if (length(miss_evt) > 0) stop("❌ 'eventos' sin columnas requeridas: ", paste(miss_evt, collapse = ", "))

# -----------------------------------------------------------------------------
# 2. VERIFICAR CRS PROYECTADO (METROS)
# -----------------------------------------------------------------------------
cat("\n→ Verificando CRS (UTM, metros)...\n")
cat("  - Polígonos UTM:", st_crs(poligonos_utm)$input, "\n")
cat("  - Hospitales UTM:", st_crs(hospitales_utm)$input, "\n")
cat("  - Eventos UTM:", st_crs(eventos_utm)$input, "\n")

cat("\n⚠ CRÍTICO: Buffers usan unidades del CRS (debe ser metros)\n\n")

# Check simple: EPSG 32719 esperado
if (st_crs(hospitales_utm)$epsg != 32719) {
  stop("❌ CRS inesperado. Esperaba EPSG:32719 en hospitales_utm.")
}

cat("✓ CRS confirmado (UTM 19S)\n")

# -----------------------------------------------------------------------------
# 3. DEFINIR HIPÓTESIS DE PROXIMIDAD
# -----------------------------------------------------------------------------
cat("\n", strrep("=", 70), "\n", sep = "")
cat("PASO 1: DEFINIR HIPÓTESIS DE PROXIMIDAD\n")
cat(strrep("=", 70), "\n\n", sep = "")

cat("🎯 HIPÓTESIS DE TRABAJO:\n")
cat("  'Los eventos de emergencia cercanos a hospitales tienen mejor\n")
cat("   respuesta. Analizaremos proximidad a 300m, 500m y 1000m'\n\n")

distancias <- c(300, 500, 1000)

cat("→ Distancias de análisis:\n")
for (d in distancias) cat("  - ", d, " m (", d/1000, " km)\n", sep = "")
cat("\n⚠ Estas distancias son supuestos para demo.\n\n")

# -----------------------------------------------------------------------------
# 4. GENERAR BUFFERS
# -----------------------------------------------------------------------------
cat(strrep("=", 70), "\n", sep = "")
cat("PASO 2: GENERAR BUFFERS ALREDEDOR DE HOSPITALES\n")
cat(strrep("=", 70), "\n\n", sep = "")

cat("→ Generando buffers (por hospital y por distancia)...\n")

buffers_lista <- list()

for (dist in distancias) {
  cat("  Procesando buffer de ", dist, " m...\n", sep = "")
  buffer_temp <- hospitales_utm %>%
    st_buffer(dist = dist) %>%
    mutate(distancia = dist, distancia_km = dist / 1000)
  
  buffers_lista[[as.character(dist)]] <- buffer_temp
  cat("    ✓ ", nrow(buffer_temp), " buffers creados\n", sep = "")
}

cat("\n✓ Buffers generados\n")

cat("\n→ Vista previa (500m):\n")
print(
  head(
    buffers_lista[["500"]] %>% select(hospital_id, nombre, tipo, distancia_km),
    3
  )
)

cat("\n→ Disolviendo buffers superpuestos (para análisis agregado)...\n")
buffers_disueltos <- list()

for (dist in distancias) {
  buffer_disuelto <- buffers_lista[[as.character(dist)]] %>%
    st_union() %>%
    st_sf(distancia = dist, geometry = .)
  
  buffers_disueltos[[as.character(dist)]] <- buffer_disuelto
  cat("  ✓ Buffer ", dist, "m disuelto\n", sep = "")
}

cat("\n💡 Buffers individuales = análisis por hospital\n")
cat("   Buffers disueltos   = cobertura agregada (evita doble conteo)\n")

# -----------------------------------------------------------------------------
# 5. CLASIFICAR EVENTOS SEGÚN PROXIMIDAD
# -----------------------------------------------------------------------------
cat("\n", strrep("=", 70), "\n", sep = "")
cat("PASO 3: CLASIFICAR EVENTOS SEGÚN PROXIMIDAD\n")
cat(strrep("=", 70), "\n\n", sep = "")

cat("→ Identificando eventos dentro de cada buffer disuelto...\n")

eventos_clasificados <- eventos_utm %>%
  mutate(
    en_buffer_300m  = FALSE,
    en_buffer_500m  = FALSE,
    en_buffer_1000m = FALSE
  )

for (dist in distancias) {
  buffer_disuelto <- buffers_disueltos[[as.character(dist)]]
  
  interseccion <- st_intersects(
    eventos_clasificados,
    buffer_disuelto,
    sparse = FALSE
  )
  
  col_name <- paste0("en_buffer_", dist, "m")
  eventos_clasificados[[col_name]] <- as.vector(interseccion)
  
  n_dentro <- sum(interseccion)
  pct_dentro <- (n_dentro / nrow(eventos_clasificados)) * 100
  
  cat("  ✓ Buffer ", dist, "m: ", n_dentro, " eventos (", round(pct_dentro, 1), "%)\n", sep = "")
}

cat("\n→ Vista previa:\n")
print(
  eventos_clasificados %>%
    select(evento_id, tipo, severidad, en_buffer_300m, en_buffer_500m, en_buffer_1000m) %>%
    st_drop_geometry() %>%
    head(5)
)

cat("\n→ Creando categoría definitiva de proximidad...\n")
eventos_clasificados <- eventos_clasificados %>%
  mutate(
    categoria_proximidad = case_when(
      en_buffer_300m  ~ "Muy cerca (< 300m)",
      en_buffer_500m  ~ "Cerca (300-500m)",
      en_buffer_1000m ~ "Medianamente cerca (500-1000m)",
      TRUE ~ "Lejos (> 1000m)"
    ),
    categoria_proximidad = factor(
      categoria_proximidad,
      levels = c(
        "Muy cerca (< 300m)",
        "Cerca (300-500m)",
        "Medianamente cerca (500-1000m)",
        "Lejos (> 1000m)"
      )
    )
  )

tabla_proximidad <- eventos_clasificados %>%
  st_drop_geometry() %>%
  count(categoria_proximidad) %>%
  mutate(porcentaje = (n / sum(n)) * 100)

cat("✓ Clasificación completada\n\n")
print(tabla_proximidad)

# -----------------------------------------------------------------------------
# 6. MÉTRICAS DE EXPOSICIÓN
# -----------------------------------------------------------------------------
cat("\n", strrep("=", 70), "\n", sep = "")
cat("PASO 4: CALCULAR MÉTRICAS DE EXPOSICIÓN\n")
cat(strrep("=", 70), "\n\n", sep = "")

cat("→ Resumen de exposición por distancia:\n\n")

exposicion_resumen <- data.frame(
  distancia_m = distancias,
  distancia_km = distancias / 1000,
  n_eventos_dentro = c(
    sum(eventos_clasificados$en_buffer_300m),
    sum(eventos_clasificados$en_buffer_500m),
    sum(eventos_clasificados$en_buffer_1000m)
  ),
  n_eventos_total = nrow(eventos_clasificados)
) %>%
  mutate(
    pct_dentro = (n_eventos_dentro / n_eventos_total) * 100,
    n_eventos_fuera = n_eventos_total - n_eventos_dentro,
    pct_fuera = 100 - pct_dentro
  )

print(exposicion_resumen)

cat("\n→ Ranking de hospitales con más eventos cercanos (500m)...\n\n")

# IMPORTANTE: evitar choque entre eventos$tipo (tipo de evento)
# y hospitales$tipo (tipo de hospital) renombrando antes del join
buffers_500_rank <- buffers_lista[["500"]] %>%
  select(hospital_id, nombre, tipo) %>%
  rename(
    hospital_nombre = nombre,
    hospital_tipo   = tipo
  )

eventos_por_hospital <- eventos_utm %>%
  st_join(buffers_500_rank, join = st_within, left = TRUE) %>%
  filter(!is.na(hospital_id)) %>%
  st_drop_geometry() %>%
  count(hospital_id, hospital_nombre, hospital_tipo, name = "eventos_cercanos") %>%
  arrange(desc(eventos_cercanos))

print(head(eventos_por_hospital, 10))

cat("\n→ Análisis de sensibilidad...\n\n")
sensibilidad <- exposicion_resumen %>%
  mutate(
    incremento_pct = pct_dentro - lag(pct_dentro, default = 0),
    eventos_adicionales = n_eventos_dentro - lag(n_eventos_dentro, default = 0)
  )

print(sensibilidad)

cat("\n💡 Interpretación rápida:\n")
cat("  - 0 a 300m: ", sensibilidad$n_eventos_dentro[1], " eventos\n", sep = "")
cat("  - 300 a 500m: +", sensibilidad$eventos_adicionales[2], " eventos\n", sep = "")
cat("  - 500 a 1000m: +", sensibilidad$eventos_adicionales[3], " eventos\n", sep = "")

# -----------------------------------------------------------------------------
# 7. VISUALIZACIÓN (MAPAS EN WGS84 + GRÁFICO)
# -----------------------------------------------------------------------------
cat("\n", strrep("=", 70), "\n", sep = "")
cat("PASO 5: VISUALIZACIÓN\n")
cat(strrep("=", 70), "\n\n", sep = "")

cat("→ Generando mapa de cobertura por buffers...\n")

buffers_500m_wgs  <- st_transform(buffers_lista[["500"]], 4326)
buffers_1000m_wgs <- st_transform(buffers_lista[["1000"]], 4326)
eventos_clasificados_wgs <- st_transform(eventos_clasificados, 4326)

mapa_buffers <- ggplot() +
  geom_sf(data = poligonos, fill = "gray95", color = "gray70", linewidth = 0.3) +
  geom_sf(data = buffers_1000m_wgs,
          fill = "#FEE5D9", alpha = 0.3, color = "#FC9272", linewidth = 0.3) +
  geom_sf(data = buffers_500m_wgs,
          fill = "#FCAE91", alpha = 0.4, color = "#FB6A4A", linewidth = 0.3) +
  geom_sf(data = eventos_clasificados_wgs,
          aes(color = categoria_proximidad),
          size = 1.5, alpha = 0.6) +
  scale_color_manual(
    values = c(
      "Muy cerca (< 300m)" = "#00441B",
      "Cerca (300-500m)" = "#41AB5D",
      "Medianamente cerca (500-1000m)" = "#FED976",
      "Lejos (> 1000m)" = "#CB181D"
    ),
    name = "Proximidad\na Hospital"
  ) +
  geom_sf(data = hospitales, shape = 3, size = 3, color = "black", stroke = 1.5) +
  labs(
    title = "Análisis de Proximidad: Eventos y Hospitales",
    subtitle = "Buffers de 500m (oscuro) y 1000m (claro)",
    caption = "Cruz negra = hospital | Puntos = eventos"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "right",
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

ggsave("outputs/02_mapa_buffers_proximidad.png", mapa_buffers,
       width = 12, height = 9, dpi = 300)

cat("✓ Guardado: outputs/02_mapa_buffers_proximidad.png\n")

cat("\n→ Generando mapa de buffers individuales (500m)...\n")

mapa_buffers_individual <- ggplot() +
  geom_sf(data = poligonos, fill = "gray95", color = "gray70") +
  geom_sf(data = buffers_500m_wgs,
          aes(fill = tipo), alpha = 0.4, color = "black", linewidth = 0.2) +
  scale_fill_manual(
    values = c("Público" = "#41B6C4", "Privado" = "#FD8D3C"),
    name = "Tipo Hospital"
  ) +
  geom_sf(data = hospitales, shape = 3, size = 2, stroke = 1) +
  labs(
    title = "Buffers Individuales (500m)",
    subtitle = "Se observan traslapos entre hospitales cercanos"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    legend.position = "bottom"
  )

ggsave("outputs/02_mapa_buffers_individual.png", mapa_buffers_individual,
       width = 8, height = 7, dpi = 300)

cat("✓ Guardado: outputs/02_mapa_buffers_individual.png\n")

cat("\n→ Generando gráfico de sensibilidad...\n")

grafico_sensibilidad <- ggplot(exposicion_resumen, aes(x = distancia_m, y = pct_dentro)) +
  geom_line(color = "#2171B5", linewidth = 1.2) +
  geom_point(color = "#2171B5", size = 4) +
  geom_text(aes(label = paste0(round(pct_dentro, 1), "%")),
            vjust = -1, size = 4, fontface = "bold") +
  scale_x_continuous(breaks = distancias, labels = paste0(distancias, "m")) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20),
                     labels = paste0(seq(0, 100, 20), "%")) +
  labs(
    title = "Sensibilidad: % de Eventos Cubiertos según Distancia",
    subtitle = "¿Cómo cambia la cobertura al expandir el radio?",
    x = "Distancia (m)",
    y = "% de eventos cubiertos",
    caption = paste0("Total eventos: ", nrow(eventos_clasificados))
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.title = element_text(size = 11),
    panel.grid.minor = element_blank()
  )

ggsave("outputs/02_grafico_sensibilidad.png", grafico_sensibilidad,
       width = 10, height = 6, dpi = 300)

cat("✓ Guardado: outputs/02_grafico_sensibilidad.png\n")

# -----------------------------------------------------------------------------
# 8. OUTPUTS: TABLAS
# -----------------------------------------------------------------------------
cat("\n", strrep("=", 70), "\n", sep = "")
cat("PASO 6: EXPORTAR RESULTADOS\n")
cat(strrep("=", 70), "\n\n", sep = "")

cat("→ Guardando tabla de exposición...\n")
write_csv(exposicion_resumen, "outputs/02_exposicion_por_distancia.csv")
cat("✓ Guardado: outputs/02_exposicion_por_distancia.csv\n")

cat("\n→ Guardando eventos clasificados...\n")
eventos_exportar <- eventos_clasificados %>%
  st_drop_geometry() %>%
  select(
    evento_id, tipo, severidad, categoria_proximidad,
    en_buffer_300m, en_buffer_500m, en_buffer_1000m
  )

write_csv(eventos_exportar, "outputs/02_eventos_clasificados_proximidad.csv")
cat("✓ Guardado: outputs/02_eventos_clasificados_proximidad.csv\n")

cat("\n→ Guardando ranking de hospitales (500m)...\n")
write_csv(eventos_por_hospital, "outputs/02_ranking_hospitales_500m.csv")
cat("✓ Guardado: outputs/02_ranking_hospitales_500m.csv\n")

# -----------------------------------------------------------------------------
# RESUMEN FINAL
# -----------------------------------------------------------------------------
cat("\n", strrep("=", 70), "\n", sep = "")
cat("✓ DEMO 2 COMPLETADA\n")
cat(strrep("=", 70), "\n\n", sep = "")

cat("📊 Resultados clave:\n")
cat("  - Eventos analizados:", nrow(eventos_clasificados), "\n")
cat("  - Cobertura 300m:", round(exposicion_resumen$pct_dentro[1], 1), "%\n")
cat("  - Cobertura 500m:", round(exposicion_resumen$pct_dentro[2], 1), "%\n")
cat("  - Cobertura 1000m:", round(exposicion_resumen$pct_dentro[3], 1), "%\n")

if (nrow(eventos_por_hospital) > 0) {
  cat("  - Hospital con más eventos cercanos (500m):",
      eventos_por_hospital$hospital_nombre[1],
      "(", eventos_por_hospital$eventos_cercanos[1], ")\n")
}

cat("\n📁 Outputs generados:\n")
cat("  - outputs/02_mapa_buffers_proximidad.png\n")
cat("  - outputs/02_mapa_buffers_individual.png\n")
cat("  - outputs/02_grafico_sensibilidad.png\n")
cat("  - outputs/02_exposicion_por_distancia.csv\n")
cat("  - outputs/02_eventos_clasificados_proximidad.csv\n")
cat("  - outputs/02_ranking_hospitales_500m.csv\n\n")
