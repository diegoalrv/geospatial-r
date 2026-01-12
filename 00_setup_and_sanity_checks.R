# =============================================================================
# 00_setup_and_sanity_checks.R
# Análisis Geoespacial en R - Setup, Verificaciones y Lecturas de Archivos
# =============================================================================
# Objetivo: Cargar/crear datos, validar calidad espacial y dejar objetos limpios
#          + plantillas para leer Parquet (WKT) y GeoJSON.
# =============================================================================

# -----------------------------------------------------------------------------
# 0. WORKDIR + ESTRUCTURA DE CARPETAS
# -----------------------------------------------------------------------------
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

dir.create("data", showWarnings = FALSE)
dir.create("outputs", showWarnings = FALSE)
dir.create("datos", showWarnings = FALSE)  # si estás usando /datos/

# -----------------------------------------------------------------------------
# 1. LIBRERÍAS Y CONFIGURACIÓN MÍNIMA
# -----------------------------------------------------------------------------
suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(ggplot2)
  library(units)
  library(readr)
})

options(scipen = 999)   # evitar notación científica
sf_use_s2(FALSE)        # simplificar geometría esférica en demos

cat("✓ Librerías cargadas, opciones seteadas y directorios listos\n")
    
# -----------------------------------------------------------------------------
# 2. GENERACIÓN DE DATOS (SIMULADOS PARA DEMO REPRODUCIBLE)
# -----------------------------------------------------------------------------

# 2.1 Polígonos administrativos (zonas/comunas)
cat("\n→ Generando polígonos administrativos...\n")

zonas_coords <- list(
  data.frame(
    lon = c(-70.68, -70.64, -70.64, -70.68, -70.68),
    lat = c(-33.42, -33.42, -33.38, -33.38, -33.42),
    zona_id = 1, zona_nombre = "Centro"
  ),
  data.frame(
    lon = c(-70.64, -70.60, -70.60, -70.64, -70.64),
    lat = c(-33.42, -33.42, -33.38, -33.38, -33.42),
    zona_id = 2, zona_nombre = "Providencia"
  ),
  data.frame(
    lon = c(-70.68, -70.64, -70.64, -70.68, -70.68),
    lat = c(-33.46, -33.46, -33.42, -33.42, -33.46),
    zona_id = 3, zona_nombre = "Estación Central"
  ),
  data.frame(
    lon = c(-70.64, -70.60, -70.60, -70.64, -70.64),
    lat = c(-33.46, -33.46, -33.42, -33.42, -33.46),
    zona_id = 4, zona_nombre = "Ñuñoa"
  ),
  data.frame(
    lon = c(-70.68, -70.64, -70.64, -70.68, -70.68),
    lat = c(-33.50, -33.50, -33.46, -33.46, -33.50),
    zona_id = 5, zona_nombre = "Maipú"
  ),
  data.frame(
    lon = c(-70.64, -70.60, -70.60, -70.64, -70.64),
    lat = c(-33.50, -33.50, -33.46, -33.46, -33.50),
    zona_id = 6, zona_nombre = "La Reina"
  )
)

# Construcción robusta: primero geometrías, luego tabla
poligonos_geom <- lapply(zonas_coords, function(df) {
  st_polygon(list(as.matrix(df[, c("lon", "lat")])))
}) |> st_sfc(crs = 4326)

poligonos <- st_sf(
  zona_id = sapply(zonas_coords, \(x) unique(x$zona_id)),
  zona_nombre = sapply(zonas_coords, \(x) unique(x$zona_nombre)),
  poblacion = c(142800, 136500, 153300, 208900, 578600, 105800),
  geometry = poligonos_geom
)

cat("✓ Polígonos creados:", nrow(poligonos), "zonas\n")

# 2.2 Puntos (Hospitales)
cat("\n→ Generando puntos de hospitales...\n")

set.seed(123)
n_hospitales <- 15

hospitales <- data.frame(
  hospital_id = 1:n_hospitales,
  nombre = paste("Hospital", LETTERS[1:n_hospitales]),
  lon = runif(n_hospitales, -70.68, -70.60),
  lat = runif(n_hospitales, -33.50, -33.38),
  capacidad = sample(50:500, n_hospitales, replace = TRUE),
  tipo = sample(c("Público", "Privado"), n_hospitales, replace = TRUE)
) |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

cat("✓ Hospitales creados:", nrow(hospitales), "puntos\n")

# 2.3 Eventos CSV (simulado)
cat("\n→ Generando eventos de emergencia...\n")

set.seed(456)
n_eventos <- 500

eventos_raw <- data.frame(
  evento_id = 1:n_eventos,
  fecha = sample(seq(as.Date("2024-01-01"), as.Date("2024-12-31"), by = "day"),
                 n_eventos, replace = TRUE),
  lat = runif(n_eventos, -33.50, -33.38),
  lon = runif(n_eventos, -70.68, -70.60),
  severidad = sample(1:5, n_eventos, replace = TRUE),
  tipo = sample(c("Accidente", "Incendio", "Médico", "Otro"),
                n_eventos, replace = TRUE, prob = c(0.4, 0.2, 0.3, 0.1))
)

cat("✓ Eventos creados:", nrow(eventos_raw), "registros\n")

# -----------------------------------------------------------------------------
# 3. SANITY CHECKS ESPACIALES
# -----------------------------------------------------------------------------
cat("\n", strrep("=", 70), "\n", sep = "")
cat("SANITY CHECKS ESPACIALES\n")
cat(strrep("=", 70), "\n\n", sep = "")

# 3.1 Estructura y tipos geométricos
cat("→ Verificando estructura de datos...\n")

cat("\nPolígonos:\n")
cat("  - Columnas:", paste(names(poligonos), collapse = ", "), "\n")
cat("  - Tipo de geometría:", st_geometry_type(poligonos, by_geometry = FALSE), "\n")

cat("\nHospitales:\n")
cat("  - Columnas:", paste(names(hospitales), collapse = ", "), "\n")
cat("  - Tipo de geometría:", st_geometry_type(hospitales, by_geometry = FALSE), "\n")

cat("\nEventos CSV:\n")
cat("  - Columnas:", paste(names(eventos_raw), collapse = ", "), "\n")
cat("  - Tiene lat/lon:", all(c("lat", "lon") %in% names(eventos_raw)), "\n")

# 3.2 CRS
cat("\n→ Verificando CRS...\n")
cat("  - Polígonos CRS:", st_crs(poligonos)$input, "\n")
cat("  - Hospitales CRS:", st_crs(hospitales)$input, "\n")
cat("  ⚠ Eventos: No tiene CRS hasta convertir a sf\n")

# 3.3 Geometrías inválidas
cat("\n→ Validando geometrías...\n")
n_invalidos_poly <- sum(!st_is_valid(poligonos))
n_invalidos_hosp <- sum(!st_is_valid(hospitales))

cat("  - Polígonos inválidos:", n_invalidos_poly, "\n")
cat("  - Hospitales inválidos:", n_invalidos_hosp, "\n")

if (n_invalidos_poly > 0) {
  cat("  ⚠ Reparando polígonos inválidos...\n")
  poligonos <- st_make_valid(poligonos)
}

# 3.4 Conteos, rangos y NAs
cat("\n→ Resumen de registros...\n")
cat("  - Total polígonos:", nrow(poligonos), "\n")
cat("  - Total hospitales:", nrow(hospitales), "\n")
cat("  - Total eventos:", nrow(eventos_raw), "\n")

cat("\n→ Rangos de coordenadas en eventos CSV...\n")
cat("  - Latitud: [", round(min(eventos_raw$lat), 4), ",", round(max(eventos_raw$lat), 4), "]\n")
cat("  - Longitud: [", round(min(eventos_raw$lon), 4), ",", round(max(eventos_raw$lon), 4), "]\n")

n_na_coords <- sum(is.na(eventos_raw$lat) | is.na(eventos_raw$lon))
cat("  - Eventos con coordenadas NA:", n_na_coords, "\n")

# 3.5 Convertir eventos a sf
cat("\n→ Convirtiendo eventos CSV a sf...\n")
eventos <- eventos_raw |>
  filter(!is.na(lat), !is.na(lon)) |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

cat("✓ Eventos convertidos a sf:", nrow(eventos), "puntos\n")

# 3.6 Cobertura espacial (puntos dentro del área)
cat("\n→ Verificando cobertura espacial...\n")
bbox_poligonos <- st_bbox(poligonos)

cat("  - BBox polígonos:",
    paste(round(bbox_poligonos, 4), collapse = ", "), "\n")

hospitales_dentro <- hospitales |> st_filter(poligonos)
eventos_dentro <- eventos |> st_filter(poligonos)

cat("  - Hospitales dentro del área:", nrow(hospitales_dentro), "/", nrow(hospitales), "\n")
cat("  - Eventos dentro del área:", nrow(eventos_dentro), "/", nrow(eventos), "\n")

# -----------------------------------------------------------------------------
# 4. NORMALIZACIÓN (CRS + OBJETOS PARA ANÁLISIS)
# -----------------------------------------------------------------------------
cat("\n", strrep("=", 70), "\n", sep = "")
cat("NORMALIZACIÓN DE DATOS\n")
cat(strrep("=", 70), "\n\n", sep = "")

crs_geografico <- 4326   # WGS84
crs_proyectado <- 32719  # UTM 19S (Santiago)

cat("→ CRS definidos:\n")
cat("  - Geográfico:", crs_geografico, "\n")
cat("  - Proyectado:", crs_proyectado, "\n")

poligonos_utm <- st_transform(poligonos, crs_proyectado)
hospitales_utm <- st_transform(hospitales, crs_proyectado)
eventos_utm <- st_transform(eventos, crs_proyectado)

cat("✓ Objetos transformados a UTM\n")

cat("\n→ Columnas clave (referencia):\n")
cat("  - Polígonos: zona_id, zona_nombre\n")
cat("  - Eventos: evento_id, tipo, severidad\n")
cat("  - Hospitales: hospital_id, nombre, capacidad\n")

# -----------------------------------------------------------------------------
# 5. PERSISTENCIA: GUARDAR OBJETOS LIMPIOS
# -----------------------------------------------------------------------------
cat("\n", strrep("=", 70), "\n", sep = "")
cat("PREPARACIÓN FINAL\n")
cat(strrep("=", 70), "\n\n", sep = "")

saveRDS(poligonos, "data/poligonos_clean.rds")
saveRDS(hospitales, "data/hospitales_clean.rds")
saveRDS(eventos, "data/eventos_clean.rds")

saveRDS(poligonos_utm, "data/poligonos_utm.rds")
saveRDS(hospitales_utm, "data/hospitales_utm.rds")
saveRDS(eventos_utm, "data/eventos_utm.rds")

cat("✓ Objetos guardados en data/\n")

# -----------------------------------------------------------------------------
# 6. OUTPUTS: MAPA DE VALIDACIÓN + TABLA DE ESTADO
# -----------------------------------------------------------------------------
cat("\n→ Generando outputs...\n")

mapa_validacion <- ggplot() +
  geom_sf(data = poligonos, fill = "lightblue", alpha = 0.3, color = "gray30") +
  geom_sf(data = hospitales, aes(color = tipo), size = 3, shape = 17) +
  geom_sf(data = eventos, alpha = 0.3, size = 0.5, color = "red") +
  scale_color_manual(values = c("Público" = "#2C7BB6", "Privado" = "#D7191C")) +
  labs(
    title = "Validación: Cobertura Espacial de Datos",
    subtitle = paste("Santiago:", nrow(poligonos), "zonas,",
                     nrow(hospitales), "hospitales,",
                     nrow(eventos), "eventos"),
    color = "Tipo Hospital",
    caption = "Puntos rojos: eventos de emergencia"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "bottom"
  )

ggsave(
  filename = "outputs/00_validacion_cobertura.png",
  plot = mapa_validacion,
  width = 10, height = 8, dpi = 300
)

cat("✓ Mapa guardado: outputs/00_validacion_cobertura.png\n")

estado_datasets <- data.frame(
  Dataset = c("Polígonos", "Hospitales", "Eventos"),
  N_Registros = c(nrow(poligonos), nrow(hospitales), nrow(eventos)),
  CRS = c(st_crs(poligonos)$input, st_crs(hospitales)$input, st_crs(eventos)$input),
  Tipo_Geometria = c(
    st_geometry_type(poligonos, by_geometry = FALSE),
    st_geometry_type(hospitales, by_geometry = FALSE),
    st_geometry_type(eventos, by_geometry = FALSE)
  ),
  BBox_xmin = c(st_bbox(poligonos)[1], st_bbox(hospitales)[1], st_bbox(eventos)[1]),
  BBox_ymin = c(st_bbox(poligonos)[2], st_bbox(hospitales)[2], st_bbox(eventos)[2]),
  BBox_xmax = c(st_bbox(poligonos)[3], st_bbox(hospitales)[3], st_bbox(eventos)[3]),
  BBox_ymax = c(st_bbox(poligonos)[4], st_bbox(hospitales)[4], st_bbox(eventos)[4])
)

print(estado_datasets)
write_csv(estado_datasets, "outputs/00_estado_datasets.csv")

cat("✓ Tabla guardada: outputs/00_estado_datasets.csv\n")

# -----------------------------------------------------------------------------
# 7. RESUMEN FINAL
# -----------------------------------------------------------------------------
cat("\n", strrep("=", 70), "\n", sep = "")
cat("✓ SETUP COMPLETADO\n")
cat(strrep("=", 70), "\n\n", sep = "")

cat("Objetos disponibles en memoria:\n")
cat("  - poligonos (WGS84) /", nrow(poligonos), "zonas\n")
cat("  - hospitales (WGS84) /", nrow(hospitales), "puntos\n")
cat("  - eventos (WGS84) /", nrow(eventos), "puntos\n")
cat("  - poligonos_utm (UTM 19S) /", nrow(poligonos_utm), "zonas\n")
cat("  - hospitales_utm (UTM 19S) /", nrow(hospitales_utm), "puntos\n")
cat("  - eventos_utm (UTM 19S) /", nrow(eventos_utm), "puntos\n")

cat("\nOutputs generados:\n")
cat("  - outputs/00_validacion_cobertura.png\n")
cat("  - outputs/00_estado_datasets.csv\n")
cat("\n🎯 Todo listo para las demos siguientes\n\n")

# =============================================================================
# 99_snippets_lectura_archivos.R (SNIPPETS REUSABLES)
# =============================================================================
# Nota: dejo estos bloques al final, separados del setup principal.

# -----------------------------------------------------------------------------
# A) LEER PARQUET CON GEOMETRÍA EN WKT → sf
# -----------------------------------------------------------------------------
suppressPackageStartupMessages({
  library(arrow)
  library(sf)
})

parquet_path <- "C:/Users/dalex/Documents/Clase R/datos/h3_temuco.parquet"

df_parquet <- read_parquet(parquet_path)

gdf_parquet <- st_sf(
  df_parquet,
  geometry = st_as_sfc(df_parquet$geometry, crs = 4326)
)

# Checks rápidos
print(st_geometry_type(gdf_parquet))
print(st_crs(gdf_parquet))
plot(st_geometry(gdf_parquet), main = "Chequeo rápido de geometría (Parquet/WKT)")

# -----------------------------------------------------------------------------
# B) LEER GEOJSON DIRECTO → sf
# -----------------------------------------------------------------------------
suppressPackageStartupMessages({
  library(sf)
})

geojson_path <- "C:/Users/dalex/Documents/Clase R/datos/hospitales_clinicas_poligonos.geojson"

gdf_geojson <- st_read(geojson_path, quiet = TRUE)

# Checks rápidos
print(st_geometry_type(gdf_geojson))
print(st_crs(gdf_geojson))
plot(st_geometry(gdf_geojson), main = "Chequeo rápido de geometría (GeoJSON)")
