# =============================================================================
# 01_demo_table_to_sf_join.R
# Análisis Geoespacial en R - Demo 1: CSV → Puntos → Join Espacial
# =============================================================================
# Objetivo: Convertir una tabla CSV con coordenadas a puntos espaciales,
# hacer join espacial con polígonos y generar resúmenes por zona.
# =============================================================================

# -----------------------------------------------------------------------------
# 0. WORKDIR + LIBRERÍAS + OPCIONES
# -----------------------------------------------------------------------------
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(ggplot2)
  library(readr)
  library(scales)
  library(tidyr)
})

options(scipen = 999)
sf_use_s2(FALSE)

dir.create("outputs", showWarnings = FALSE)

# -----------------------------------------------------------------------------
# 1. CARGAR OBJETOS DEL SETUP
# -----------------------------------------------------------------------------
cat("→ Cargando objetos limpios del setup...\n")

poligonos <- readRDS("data/poligonos_clean.rds")
eventos   <- readRDS("data/eventos_clean.rds")

cat("✓ Datos cargados:\n")
cat("  - Polígonos:", nrow(poligonos), "zonas\n")
cat("  - Eventos (sf):", nrow(eventos), "puntos\n\n")

# Checks mínimos de columnas esperadas
req_pol_cols <- c("zona_id", "zona_nombre", "poblacion")
miss_pol <- setdiff(req_pol_cols, names(poligonos))
if (length(miss_pol) > 0) {
  stop("❌ 'poligonos' no tiene columnas requeridas: ", paste(miss_pol, collapse = ", "))
}

# -----------------------------------------------------------------------------
# 2. DEMO: CONSTRUIR “CSV RAW” DESDE EVENTOS sf (para mostrar el flujo completo)
# -----------------------------------------------------------------------------
eventos_csv <- data.frame(
  evento_id  = eventos$evento_id,
  fecha      = eventos$fecha,
  lat        = st_coordinates(eventos)[, 2],
  lon        = st_coordinates(eventos)[, 1],
  severidad  = eventos$severidad,
  tipo       = eventos$tipo
)

cat("  - Eventos CSV:", nrow(eventos_csv), "filas\n\n")

# -----------------------------------------------------------------------------
# 3. PASO 1: CSV → PUNTOS sf (con validación previa)
# -----------------------------------------------------------------------------
cat(strrep("=", 70), "\n", sep = "")
cat("PASO 1: CONVERTIR CSV A PUNTOS ESPACIALES\n")
cat(strrep("=", 70), "\n\n", sep = "")

cat("→ Vista previa del CSV:\n")
print(head(eventos_csv, 3))

cat("\n→ Verificando calidad de coordenadas...\n")

n_na_lat <- sum(is.na(eventos_csv$lat))
n_na_lon <- sum(is.na(eventos_csv$lon))
cat("  - NAs en latitud:", n_na_lat, "\n")
cat("  - NAs en longitud:", n_na_lon, "\n")

cat("  - Rango latitud: [", min(eventos_csv$lat, na.rm = TRUE), ",",
    max(eventos_csv$lat, na.rm = TRUE), "]\n", sep = "")
cat("  - Rango longitud: [", min(eventos_csv$lon, na.rm = TRUE), ",",
    max(eventos_csv$lon, na.rm = TRUE), "]\n", sep = "")

# Filtro básico Chile continental (demo)
coordenadas_validas <- eventos_csv %>%
  filter(
    !is.na(lat), !is.na(lon),
    lat >= -56, lat <= -17,
    lon >= -76, lon <= -66
  )

n_excluidos <- nrow(eventos_csv) - nrow(coordenadas_validas)
if (n_excluidos > 0) {
  cat("  ⚠ ", n_excluidos, " eventos excluidos por coordenadas inválidas\n", sep = "")
}

cat("\n→ Convirtiendo a objeto sf...\n")
eventos_sf <- coordenadas_validas %>%
  st_as_sf(
    coords = c("lon", "lat"),  # orden correcto: lon, lat
    crs = 4326,
    remove = FALSE
  )

cat("✓ Conversión exitosa:\n")
cat("  - Geometría:", st_geometry_type(eventos_sf, by_geometry = FALSE), "\n")
cat("  - CRS:", st_crs(eventos_sf)$input, "\n")
cat("  - Registros finales:", nrow(eventos_sf), "\n")

cat("\n→ Vista previa del objeto espacial:\n")
print(head(eventos_sf %>% select(evento_id, tipo, severidad), 3))

# -----------------------------------------------------------------------------
# 4. PASO 2: JOIN ESPACIAL (Puntos → Polígonos)
# -----------------------------------------------------------------------------
cat("\n", strrep("=", 70), "\n", sep = "")
cat("PASO 2: JOIN ESPACIAL - ASIGNAR ZONA A CADA EVENTO\n")
cat(strrep("=", 70), "\n\n", sep = "")

eventos_con_zona <- eventos_sf %>%
  st_join(poligonos, join = st_within, left = TRUE)

cat("✓ Join completado:\n")
cat("  - Eventos con zona asignada:", sum(!is.na(eventos_con_zona$zona_id)), "\n")
cat("  - Eventos sin zona:", sum(is.na(eventos_con_zona$zona_id)), "\n")

cat("\n→ Vista previa del resultado:\n")
print(
  eventos_con_zona %>%
    select(evento_id, tipo, severidad, zona_id, zona_nombre) %>%
    st_drop_geometry() %>%
    head(5)
)

# Limpiar (quedarnos con los que caen dentro)
eventos_con_zona_clean <- eventos_con_zona %>%
  filter(!is.na(zona_id))

cat("\n→ Dataset limpio para análisis:", nrow(eventos_con_zona_clean), "eventos\n")

# -----------------------------------------------------------------------------
# 5. PASO 3: RESÚMENES POR ZONA
# -----------------------------------------------------------------------------
cat("\n", strrep("=", 70), "\n", sep = "")
cat("PASO 3: ANÁLISIS Y RESÚMENES POR ZONA\n")
cat(strrep("=", 70), "\n\n", sep = "")

# 5.1 Conteo por zona (solo por ID, para no duplicar nombres luego)
cat("→ Calculando conteo de eventos por zona...\n")
resumen_conteo <- eventos_con_zona_clean %>%
  st_drop_geometry() %>%
  group_by(zona_id) %>%
  summarise(n_eventos = n(), .groups = "drop") %>%
  arrange(desc(n_eventos)
)

print(resumen_conteo)

# 5.2 Severidad por zona (solo por ID)
cat("\n→ Calculando severidad promedio por zona...\n")
resumen_severidad <- eventos_con_zona_clean %>%
  st_drop_geometry() %>%
  group_by(zona_id) %>%
  summarise(
    n_eventos = n(),
    severidad_promedio = mean(severidad, na.rm = TRUE),
    severidad_mediana = median(severidad, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(severidad_promedio))

print(resumen_severidad)

# 5.3 Distribución por tipo de evento y zona (usa nombre solo para lectura)
cat("\n→ Distribución de tipos de evento por zona...\n")
resumen_tipo <- eventos_con_zona_clean %>%
  st_drop_geometry() %>%
  count(zona_nombre, tipo) %>%
  pivot_wider(names_from = tipo, values_from = n, values_fill = 0)

print(resumen_tipo)

# 5.4 Tasas por población (unimos conteo con poligonos, que trae población)
cat("\n→ Calculando tasas por 10,000 habitantes...\n")
resumen_con_tasa <- resumen_conteo %>%
  left_join(poligonos %>% st_drop_geometry() %>% select(zona_id, poblacion), by = "zona_id") %>%
  mutate(tasa_por_10k = (n_eventos / poblacion) * 10000) %>%
  arrange(desc(tasa_por_10k))

print(resumen_con_tasa)

# 5.5 Unir métricas con geometrías para mapear (evita duplicar zona_nombre/poblacion)
cat("\n→ Preparando datos para visualización...\n")
poligonos_con_eventos <- poligonos %>%
  left_join(
    resumen_con_tasa %>% select(zona_id, n_eventos, tasa_por_10k),
    by = "zona_id"
  ) %>%
  mutate(
    categoria_eventos = cut(
      n_eventos,
      breaks = c(0, 50, 80, 110, Inf),
      labels = c("0-50", "51-80", "81-110", "110+"),
      include.lowest = TRUE
    )
  )

cat("✓ Datos preparados para mapeo\n")

# -----------------------------------------------------------------------------
# 6. PASO 4: VISUALIZACIÓN
# -----------------------------------------------------------------------------
cat("\n", strrep("=", 70), "\n", sep = "")
cat("PASO 4: VISUALIZACIÓN\n")
cat(strrep("=", 70), "\n\n", sep = "")

# (Opcional) si quieres evitar warnings de st_point_on_surface, descomenta:
# poligonos_plot <- st_transform(poligonos_con_eventos, 32719)
# eventos_plot   <- st_transform(eventos_con_zona_clean, 32719)
poligonos_plot <- poligonos_con_eventos
eventos_plot   <- eventos_con_zona_clean

# 6.1 Mapa coroplético por conteo
cat("→ Generando mapa coroplético (conteo de eventos)...\n")
mapa_conteo <- ggplot() +
  geom_sf(
    data = poligonos_plot,
    aes(fill = n_eventos),
    color = "white",
    linewidth = 0.5
  ) +
  scale_fill_gradient(
    low = "#FFF5EB",
    high = "#D94701",
    name = "N° Eventos",
    na.value = "gray90"
  ) +
  geom_sf_text(
    data = poligonos_plot,
    aes(label = n_eventos),
    size = 4,
    fontface = "bold"
  ) +
  labs(
    title = "Distribución de Eventos de Emergencia por Zona",
    subtitle = paste("Total:", sum(resumen_conteo$n_eventos), "eventos"),
    caption = "Fuente: Análisis geoespacial con sf"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "right",
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

ggsave("outputs/01_mapa_conteo_eventos.png", mapa_conteo, width = 10, height = 8, dpi = 300)
cat("✓ Guardado: outputs/01_mapa_conteo_eventos.png\n")

# 6.2 Mapa coroplético por tasa
cat("\n→ Generando mapa coroplético (tasa por población)...\n")
mapa_tasa <- ggplot() +
  geom_sf(
    data = poligonos_plot,
    aes(fill = tasa_por_10k),
    color = "white",
    linewidth = 0.5
  ) +
  scale_fill_gradient(
    low = "#E0ECF4",
    high = "#2B8CBE",
    name = "Tasa por\n10k hab.",
    na.value = "gray90"
  ) +
  geom_sf_text(
    data = poligonos_plot,
    aes(label = round(tasa_por_10k, 1)),
    size = 3.5,
    fontface = "bold"
  ) +
  labs(
    title = "Tasa de Eventos por 10,000 Habitantes",
    subtitle = "Normalizado por población de la zona",
    caption = "Nota: La tasa permite comparar zonas de distinto tamaño poblacional"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "right",
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

ggsave("outputs/01_mapa_tasa_eventos.png", mapa_tasa, width = 10, height = 8, dpi = 300)
cat("✓ Guardado: outputs/01_mapa_tasa_eventos.png\n")

# 6.3 Mapa con puntos sobre polígonos (validación visual)
cat("\n→ Generando mapa de validación con puntos...\n")
mapa_validacion <- ggplot() +
  geom_sf(
    data = poligonos_plot,
    aes(fill = categoria_eventos),
    color = "gray40",
    linewidth = 0.3,
    alpha = 0.6
  ) +
  scale_fill_brewer(
    palette = "YlOrRd",
    name = "Categoría\nEventos",
    na.value = "gray90"
  ) +
  geom_sf(
    data = eventos_plot,
    aes(color = tipo),
    size = 1,
    alpha = 0.5
  ) +
  scale_color_manual(
    values = c(
      "Accidente" = "#E31A1C",
      "Incendio"  = "#FF7F00",
      "Médico"    = "#6A3D9A",
      "Otro"      = "#33A02C"
    ),
    name = "Tipo Evento"
  ) +
  labs(
    title = "Validación: Puntos de Eventos por Zona",
    subtitle = "Verificación visual del join espacial",
    caption = "Cada punto representa un evento, coloreado por tipo"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "bottom",
    legend.box = "vertical"
  )

ggsave("outputs/01_mapa_validacion_puntos.png", mapa_validacion, width = 10, height = 8, dpi = 300)
cat("✓ Guardado: outputs/01_mapa_validacion_puntos.png\n")

# -----------------------------------------------------------------------------
# 7. PASO 5: EXPORTAR RESULTADOS
# -----------------------------------------------------------------------------
cat("\n", strrep("=", 70), "\n", sep = "")
cat("PASO 5: EXPORTAR RESULTADOS\n")
cat(strrep("=", 70), "\n\n", sep = "")

# 7.1 Tabla resumen por zona (ya NO debería fallar zona_nombre)
cat("→ Guardando tabla resumen...\n")
tabla_final <- poligonos_con_eventos %>%
  st_drop_geometry() %>%
  select(zona_id, zona_nombre, poblacion, n_eventos, tasa_por_10k) %>%
  left_join(
    resumen_severidad %>% select(zona_id, severidad_promedio),
    by = "zona_id"
  ) %>%
  arrange(desc(n_eventos))

write_csv(tabla_final, "outputs/01_resumen_por_zona.csv")
print(tabla_final)
cat("\n✓ Guardado: outputs/01_resumen_por_zona.csv\n")

# 7.2 Tabla de eventos enriquecidos (con zona)
cat("\n→ Guardando tabla de eventos enriquecidos...\n")
eventos_enriquecidos <- eventos_con_zona_clean %>%
  st_drop_geometry() %>%
  select(evento_id, fecha, tipo, severidad, zona_id, zona_nombre)

write_csv(eventos_enriquecidos, "outputs/01_eventos_con_zona.csv")
cat("✓ Guardado: outputs/01_eventos_con_zona.csv\n")
cat("  - Primeras filas:\n")
print(head(eventos_enriquecidos))

# -----------------------------------------------------------------------------
# RESUMEN FINAL Y LECCIONES CLAVE
# -----------------------------------------------------------------------------
cat("\n", strrep("=", 70), "\n", sep = "")
cat("✓ DEMO 1 COMPLETADA\n")
cat(rep("=", 70), "\n\n", sep = "")

cat("📊 Resultados clave:\n")
cat("  - Eventos procesados:", nrow(eventos_con_zona_clean), "\n")
cat("  - Zona con más eventos:",
    tabla_final$zona_nombre[1], "(", tabla_final$n_eventos[1], ")\n")
cat("  - Zona con mayor tasa:",
    tabla_final$zona_nombre[which.max(tabla_final$tasa_por_10k)],
    "(", round(max(tabla_final$tasa_por_10k, na.rm = TRUE), 1), "por 10k hab.)\n")

cat("\n📁 Outputs generados:\n")
cat("  - outputs/01_mapa_conteo_eventos.png\n")
cat("  - outputs/01_mapa_tasa_eventos.png\n")
cat("  - outputs/01_mapa_validacion_puntos.png\n")
cat("  - outputs/01_resumen_por_zona.csv\n")
cat("  - outputs/01_eventos_con_zona.csv\n")

cat("\n🎓 Lecciones clave:\n")
cat("  1. Siempre verificar coordenadas antes de convertir a sf\n")
cat("  2. En st_as_sf, coords = c('lon','lat') (orden importa)\n")
cat("  3. st_join + st_within asigna atributos del polígono a los puntos\n")
cat("  4. Tasas por población permiten comparaciones justas entre zonas\n")
cat("  5. Validar visualmente el join con un mapa\n\n")
