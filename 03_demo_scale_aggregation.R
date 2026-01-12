# =============================================================================
# 03_demo_scale_aggregation.R
# Análisis Geoespacial en R - Demo 3: Escala y Agregación Espacial (MAUP)
# =============================================================================
# Objetivo: Demostrar el problema de la unidad de análisis modificable (MAUP)
# comparando agregación administrativa vs grilla hexagonal
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
  library(readr)
  library(patchwork)  # combinar gráficos
})

options(scipen = 999)
sf_use_s2(FALSE)

dir.create("data", showWarnings = FALSE)
dir.create("outputs", showWarnings = FALSE)

cat("✓ Directorio configurado, librerías cargadas y carpetas listas\n")

# -----------------------------------------------------------------------------
# 1. CARGAR OBJETOS (ESTILO DEMO 1)
# -----------------------------------------------------------------------------
cat("\n", strrep("=", 70), "\n", sep = "")
cat("PREPARACIÓN: CARGAR DATOS ENRIQUECIDOS\n")
cat(strrep("=", 70), "\n\n", sep = "")

cat("→ Cargando objetos del setup...\n")

rds_paths <- c(
  poligonos = "data/poligonos_clean.rds",
  eventos   = "data/eventos_clean.rds"
)

estado_archivos <- data.frame(
  archivo = names(rds_paths),
  path = unname(rds_paths),
  existe = file.exists(rds_paths),
  stringsAsFactors = FALSE
)
print(estado_archivos)

if (!all(file.exists(rds_paths))) {
  faltan <- names(rds_paths)[!file.exists(rds_paths)]
  stop(
    "❌ Faltan RDS base: ", paste(faltan, collapse = ", "),
    "\n   Corre primero 00_setup_and_sanity_checks.R o revisa setwd()."
  )
}

poligonos <- readRDS(rds_paths["poligonos"])
eventos   <- readRDS(rds_paths["eventos"])

cat("\n✓ Datos cargados:\n")
cat("  - Polígonos:", nrow(poligonos), "zonas administrativas\n")
cat("  - Eventos:", nrow(eventos), "puntos\n")

# Checks mínimos
req_pol_cols <- c("zona_id", "zona_nombre", "poblacion")
miss_pol <- setdiff(req_pol_cols, names(poligonos))
if (length(miss_pol) > 0) stop("❌ 'poligonos' sin columnas requeridas: ", paste(miss_pol, collapse = ", "))

req_evt_cols <- c("evento_id", "tipo", "severidad")
miss_evt <- setdiff(req_evt_cols, names(eventos))
if (length(miss_evt) > 0) stop("❌ 'eventos' sin columnas requeridas: ", paste(miss_evt, collapse = ", "))

cat("\n→ Variables disponibles para agregación:\n")
cat("  - Conteo de eventos\n")
cat("  - Severidad promedio (1-5)\n")
cat("  - Tasa por 10k habitantes (con población)\n\n")

# -----------------------------------------------------------------------------
# 2. DATASET BASE: EVENTOS CON ZONA (JOIN ESPACIAL)
# -----------------------------------------------------------------------------
cat(strrep("=", 70), "\n", sep = "")
cat("PASO 1: EVENTOS + ZONA ADMINISTRATIVA (JOIN ESPACIAL)\n")
cat(strrep("=", 70), "\n\n", sep = "")

cat("→ Asignando zona a cada evento (st_join + st_within)...\n")

eventos_con_zona <- eventos %>%
  st_join(poligonos %>% select(zona_id, zona_nombre, poblacion), join = st_within, left = TRUE) %>%
  filter(!is.na(zona_id))

cat("✓ Eventos con zona asignada:", nrow(eventos_con_zona), "\n")

# -----------------------------------------------------------------------------
# 3. AGREGACIÓN A: UNIDAD ADMINISTRATIVA
# -----------------------------------------------------------------------------
cat("\n", strrep("=", 70), "\n", sep = "")
cat("PASO 2A: AGREGACIÓN ADMINISTRATIVA (ZONAS)\n")
cat(strrep("=", 70), "\n\n", sep = "")

cat("→ Resumiendo por zona...\n")

resumen_admin <- eventos_con_zona %>%
  st_drop_geometry() %>%
  group_by(zona_id, zona_nombre, poblacion) %>%
  summarise(
    n_eventos = n(),
    severidad_promedio = mean(severidad, na.rm = TRUE),
    severidad_max = max(severidad, na.rm = TRUE),
    pct_tipo_accidente = mean(tipo == "Accidente", na.rm = TRUE) * 100,
    .groups = "drop"
  ) %>%
  mutate(
    tasa_por_10k = (n_eventos / poblacion) * 10000
  )

print(resumen_admin)

cat("\n→ Uniendo resumen con geometrías administrativas...\n")

poligonos_con_metricas <- poligonos %>%
  left_join(resumen_admin, by = c("zona_id", "zona_nombre", "poblacion")) %>%
  tidyr::replace_na(list(
    n_eventos = 0,
    severidad_promedio = 0,
    severidad_max = 0,
    pct_tipo_accidente = 0,
    tasa_por_10k = 0
  ))

cat("✓ Capa administrativa lista\n")
cat("  - Rango tasa_por_10k:",
    round(min(poligonos_con_metricas$tasa_por_10k, na.rm = TRUE), 2), "-",
    round(max(poligonos_con_metricas$tasa_por_10k, na.rm = TRUE), 2), "\n")

# -----------------------------------------------------------------------------
# 4. AGREGACIÓN B: GRILLA HEXAGONAL (EN CRS PROYECTADO PARA TAMAÑO REAL)
# -----------------------------------------------------------------------------
cat("\n", strrep("=", 70), "\n", sep = "")
cat("PASO 2B: AGREGACIÓN HEXAGONAL (GRILLA)\n")
cat(strrep("=", 70), "\n\n", sep = "")

cat("→ Creando grilla hexagonal en CRS proyectado (metros)...\n")

# Pasar a UTM para que "cellsize" sea real (metros)
crs_proyectado <- 32719
poligonos_utm <- st_transform(poligonos, crs_proyectado)
eventos_utm   <- st_transform(eventos, crs_proyectado)

# Tamaño de celda: ~1.1 km (aprox similar a tu 0.01° anterior, pero correcto en metros)
cellsize_m <- 1100

grilla_hex <- st_make_grid(
  poligonos_utm,
  cellsize = cellsize_m,
  square = FALSE,
  what = "polygons"
) %>%
  st_sf(hex_id = seq_along(.), geometry = .)

cat("✓ Grilla creada:", nrow(grilla_hex), "hexágonos\n")
cat("  - Cellsize:", cellsize_m, "m (~", round(cellsize_m / 1000, 2), "km)\n")

cat("\n→ Recortando grilla al área...\n")

grilla_hex_recortada <- grilla_hex %>%
  st_filter(poligonos_utm, .predicate = st_intersects)

cat("✓ Grilla recortada:", nrow(grilla_hex_recortada), "hexágonos\n")

cat("\n→ Asignando eventos a hexágonos...\n")

eventos_con_hex <- eventos_utm %>%
  st_join(grilla_hex_recortada, join = st_within, left = TRUE) %>%
  filter(!is.na(hex_id))

cat("✓ Eventos asignados:", nrow(eventos_con_hex), "/", nrow(eventos_utm), "\n")

cat("\n→ Calculando métricas por hexágono...\n")

resumen_hex <- eventos_con_hex %>%
  st_drop_geometry() %>%
  group_by(hex_id) %>%
  summarise(
    n_eventos = n(),
    severidad_promedio = mean(severidad, na.rm = TRUE),
    severidad_max = max(severidad, na.rm = TRUE),
    pct_tipo_accidente = mean(tipo == "Accidente", na.rm = TRUE) * 100,
    .groups = "drop"
  )

cat("\n→ Preparando capa hexagonal final...\n")

grilla_con_metricas <- grilla_hex_recortada %>%
  left_join(resumen_hex, by = "hex_id") %>%
  filter(!is.na(n_eventos), n_eventos > 0)

cat("✓ Hexágonos con eventos:", nrow(grilla_con_metricas), "\n")
cat("  - Rango n_eventos:", min(grilla_con_metricas$n_eventos), "-",
    max(grilla_con_metricas$n_eventos), "\n")

cat("\n💡 Nota: se excluyeron hexágonos vacíos (solo para visualización)\n")

# Pasar grilla a WGS84 para mapas comparables con la capa administrativa
grilla_con_metricas_wgs <- st_transform(grilla_con_metricas, 4326)

# -----------------------------------------------------------------------------
# 5. COMPARACIÓN ENTRE AGREGACIONES (DESCRIPTIVAS + TOPS)
# -----------------------------------------------------------------------------
cat("\n", strrep("=", 70), "\n", sep = "")
cat("PASO 3: COMPARACIÓN ENTRE AGREGACIONES (MAUP)\n")
cat(strrep("=", 70), "\n\n", sep = "")

cat("→ Estadísticas comparativas:\n\n")

cat("ADMINISTRATIVA (zonas):\n")
cat("  - Unidades:", nrow(poligonos_con_metricas), "\n")
cat("  - Promedio eventos/unidad:", round(mean(poligonos_con_metricas$n_eventos, na.rm = TRUE), 1), "\n")
cat("  - Rango:", min(poligonos_con_metricas$n_eventos, na.rm = TRUE), "-",
    max(poligonos_con_metricas$n_eventos, na.rm = TRUE), "\n")
cat("  - Desv. estándar:", round(sd(poligonos_con_metricas$n_eventos, na.rm = TRUE), 1), "\n")

cat("\nHEXAGONAL (grilla):\n")
cat("  - Unidades:", nrow(grilla_con_metricas), "\n")
cat("  - Promedio eventos/unidad:", round(mean(grilla_con_metricas$n_eventos, na.rm = TRUE), 1), "\n")
cat("  - Rango:", min(grilla_con_metricas$n_eventos, na.rm = TRUE), "-",
    max(grilla_con_metricas$n_eventos, na.rm = TRUE), "\n")
cat("  - Desv. estándar:", round(sd(grilla_con_metricas$n_eventos, na.rm = TRUE), 1), "\n")

cat("\n→ Top 3 unidades con más eventos:\n\n")

cat("ADMINISTRATIVA:\n")
top_admin <- poligonos_con_metricas %>%
  st_drop_geometry() %>%
  arrange(desc(n_eventos)) %>%
  slice(1:3) %>%
  select(zona_nombre, n_eventos, tasa_por_10k)
print(top_admin)

cat("\nHEXAGONAL:\n")
top_hex <- grilla_con_metricas %>%
  st_drop_geometry() %>%
  arrange(desc(n_eventos)) %>%
  slice(1:3) %>%
  select(hex_id, n_eventos, severidad_promedio)
print(top_hex)

cat("\n", strrep("=", 70), "\n", sep = "")
cat("🎯 MAUP: PROBLEMA DE LA UNIDAD MODIFICABLE\n")
cat(strrep("=", 70), "\n\n", sep = "")

cat("Al cambiar la unidad de agregación:\n")
cat("  ✓ Cambian métricas\n")
cat("  ✓ Cambian patrones espaciales\n")
cat("  ✓ Cambian hotspots\n")
cat("  ✓ Puede cambiar la conclusión\n\n")

cat("⚠ Implicaciones:\n")
cat("  1) No existe una unidad 'correcta' universal\n")
cat("  2) Justificar según el problema\n")
cat("  3) Probar múltiples escalas\n")
cat("  4) Reportar sensibilidad\n\n")

# -----------------------------------------------------------------------------
# 6. VISUALIZACIÓN COMPARATIVA (PATCHWORK)
# -----------------------------------------------------------------------------
cat(strrep("=", 70), "\n", sep = "")
cat("PASO 4: VISUALIZACIÓN COMPARATIVA\n")
cat(strrep("=", 70), "\n\n", sep = "")

cat("→ Generando mapa administrativo...\n")

mapa_admin <- ggplot() +
  geom_sf(
    data = poligonos_con_metricas,
    aes(fill = tasa_por_10k),
    color = "white",
    linewidth = 0.5
  ) +
  scale_fill_gradient(
    low = "#F0F0F0",
    high = "#CB181D",
    name = "Tasa por\n10k hab.",
    na.value = "gray90"
  ) +
  geom_sf_text(
    data = poligonos_con_metricas,
    aes(label = n_eventos),
    size = 3,
    fontface = "bold"
  ) +
  labs(
    title = "A. Agregación Administrativa",
    subtitle = paste(nrow(poligonos_con_metricas), "zonas | Normalizado por población")
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    legend.position = "right",
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

cat("→ Generando mapa hexagonal...\n")

mapa_hex <- ggplot() +
  geom_sf(
    data = poligonos,
    fill = NA,
    color = "gray50",
    linewidth = 0.3,
    linetype = "dashed"
  ) +
  geom_sf(
    data = grilla_con_metricas_wgs,
    aes(fill = n_eventos),
    color = "white",
    linewidth = 0.2
  ) +
  scale_fill_gradient(
    low = "#F0F0F0",
    high = "#2171B5",
    name = "N° Eventos",
    na.value = "transparent"
  ) +
  labs(
    title = "B. Agregación Hexagonal",
    subtitle = paste(nrow(grilla_con_metricas_wgs), "hexágonos con eventos | Celda ~", round(cellsize_m/1000, 1), "km")
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    legend.position = "right",
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

cat("→ Combinando visualizaciones...\n")

mapa_comparativo <- mapa_admin + mapa_hex +
  plot_annotation(
    title = "Comparación: Mismos Datos, Diferente Agregación",
    subtitle = "¿Cómo cambia la historia según la unidad de análisis?",
    caption = "Líneas punteadas en B = límites administrativos originales",
    theme = theme(
      plot.title = element_text(face = "bold", size = 16),
      plot.subtitle = element_text(size = 12)
    )
  )

ggsave("outputs/03_comparacion_agregaciones.png", mapa_comparativo,
       width = 16, height = 8, dpi = 300)

cat("✓ Guardado: outputs/03_comparacion_agregaciones.png\n")

cat("\n→ Generando mapa de contexto con puntos originales...\n")

mapa_puntos <- ggplot() +
  geom_sf(data = poligonos, fill = "gray95", color = "gray60", linewidth = 0.4) +
  geom_sf(data = eventos, aes(color = tipo), size = 1, alpha = 0.6) +
  scale_color_manual(
    values = c(
      "Accidente" = "#E31A1C",
      "Incendio" = "#FF7F00",
      "Médico" = "#6A3D9A",
      "Otro" = "#33A02C"
    ),
    name = "Tipo Evento"
  ) +
  labs(
    title = "C. Datos Originales (Puntos)",
    subtitle = "Antes de cualquier agregación"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    legend.position = "right",
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

secuencia_agregacion <- mapa_puntos + mapa_admin + mapa_hex +
  plot_annotation(
    title = "Evolución: Del Dato Original a la Agregación Espacial",
    caption = "Izquierda → Centro → Derecha: aumenta el nivel de agregación",
    theme = theme(plot.title = element_text(face = "bold", size = 16))
  )

ggsave("outputs/03_secuencia_agregacion.png", secuencia_agregacion,
       width = 18, height = 6, dpi = 300)

cat("✓ Guardado: outputs/03_secuencia_agregacion.png\n")

cat("\n→ Generando histograma comparativo...\n")

datos_comparacion <- bind_rows(
  poligonos_con_metricas %>%
    st_drop_geometry() %>%
    select(n_eventos) %>%
    mutate(agregacion = "Administrativa"),
  grilla_con_metricas %>%
    st_drop_geometry() %>%
    select(n_eventos) %>%
    mutate(agregacion = "Hexagonal")
)

histograma_comp <- ggplot(datos_comparacion, aes(x = n_eventos, fill = agregacion)) +
  geom_histogram(alpha = 0.7, position = "identity", bins = 15) +
  scale_fill_manual(
    values = c("Administrativa" = "#CB181D", "Hexagonal" = "#2171B5"),
    name = "Tipo de\nAgregación"
  ) +
  labs(
    title = "Distribución de Eventos por Unidad de Análisis",
    subtitle = "Comparación según tipo de agregación",
    x = "N° eventos por unidad",
    y = "Frecuencia (n° unidades)",
    caption = "Nota: Distribuciones distintas por geometrías distintas (MAUP)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "right"
  )

ggsave("outputs/03_histograma_comparativo.png", histograma_comp,
       width = 10, height = 6, dpi = 300)

cat("✓ Guardado: outputs/03_histograma_comparativo.png\n")

# -----------------------------------------------------------------------------
# 7. OUTPUTS: TABLAS (ESTILO DEMO 1/2)
# -----------------------------------------------------------------------------
cat("\n", strrep("=", 70), "\n", sep = "")
cat("PASO 5: EXPORTAR RESULTADOS\n")
cat(strrep("=", 70), "\n\n", sep = "")

cat("→ Guardando tablas...\n")

tabla_admin <- poligonos_con_metricas %>%
  st_drop_geometry() %>%
  select(zona_id, zona_nombre, poblacion, n_eventos, tasa_por_10k, severidad_promedio) %>%
  arrange(desc(n_eventos))

write_csv(tabla_admin, "outputs/03_resumen_administrativo.csv")
cat("✓ Guardado: outputs/03_resumen_administrativo.csv\n")

tabla_hex <- grilla_con_metricas %>%
  st_drop_geometry() %>%
  select(hex_id, n_eventos, severidad_promedio, severidad_max) %>%
  arrange(desc(n_eventos))

write_csv(tabla_hex, "outputs/03_resumen_hexagonal.csv")
cat("✓ Guardado: outputs/03_resumen_hexagonal.csv\n")

tabla_comparativa <- data.frame(
  Metrica = c(
    "N° unidades",
    "Eventos por unidad (promedio)",
    "Eventos por unidad (mediana)",
    "Eventos por unidad (máximo)",
    "Desviación estándar"
  ),
  Administrativa = c(
    nrow(poligonos_con_metricas),
    round(mean(poligonos_con_metricas$n_eventos, na.rm = TRUE), 2),
    median(poligonos_con_metricas$n_eventos, na.rm = TRUE),
    max(poligonos_con_metricas$n_eventos, na.rm = TRUE),
    round(sd(poligonos_con_metricas$n_eventos, na.rm = TRUE), 2)
  ),
  Hexagonal = c(
    nrow(grilla_con_metricas),
    round(mean(grilla_con_metricas$n_eventos, na.rm = TRUE), 2),
    median(grilla_con_metricas$n_eventos, na.rm = TRUE),
    max(grilla_con_metricas$n_eventos, na.rm = TRUE),
    round(sd(grilla_con_metricas$n_evento_))
    )
  )