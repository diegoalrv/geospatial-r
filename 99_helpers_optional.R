# =============================================================================
# 99_helpers_optional.R
# Análisis Geoespacial en R - Funciones Auxiliares
# =============================================================================
# Objetivo: Funciones de utilidad para simplificar tareas comunes en análisis
# geoespacial y mantener código limpio en los demos principales
# =============================================================================

library(sf)
library(dplyr)
library(units)

# -----------------------------------------------------------------------------
# 1. FUNCIÓN: REPORTE RÁPIDO DE OBJETO SF
# -----------------------------------------------------------------------------

#' Genera un reporte rápido de un objeto sf
#' 
#' @param sf_obj Objeto sf a reportar
#' @param name Nombre del objeto (para el título del reporte)
#' @return Imprime información en consola, devuelve invisible(NULL)
#' @examples
#' reporte_sf(poligonos, "Polígonos administrativos")
reporte_sf <- function(sf_obj, name = "Objeto sf") {
  
  # Validar que es un objeto sf
  if (!inherits(sf_obj, "sf")) {
    stop("El objeto no es de clase 'sf'")
  }
  
  cat("\n", rep("=", 60), "\n", sep = "")
  cat("REPORTE:", name, "\n")
  cat(rep("=", 60), "\n\n", sep = "")
  
  # Información básica
  cat("📊 ESTRUCTURA:\n")
  cat("  - Clase:", paste(class(sf_obj), collapse = ", "), "\n")
  cat("  - N° registros:", nrow(sf_obj), "\n")
  cat("  - N° columnas:", ncol(sf_obj), "\n")
  cat("  - Tipo geometría:", st_geometry_type(sf_obj, by_geometry = FALSE), "\n")
  
  # CRS
  cat("\n🌍 SISTEMA DE COORDENADAS:\n")
  cat("  - EPSG:", st_crs(sf_obj)$epsg, "\n")
  cat("  - Proj4string:", st_crs(sf_obj)$proj4string, "\n")
  cat("  - Unidades:", st_crs(sf_obj)$units, "\n")
  cat("  - Es geográfico:", st_is_longlat(sf_obj), "\n")
  
  # BBox
  bbox <- st_bbox(sf_obj)
  cat("\n📍 BOUNDING BOX:\n")
  cat("  - xmin:", round(bbox[1], 4), "\n")
  cat("  - ymin:", round(bbox[2], 4), "\n")
  cat("  - xmax:", round(bbox[3], 4), "\n")
  cat("  - ymax:", round(bbox[4], 4), "\n")
  
  # Validez geométrica
  cat("\n✓ VALIDACIÓN:\n")
  n_invalidos <- sum(!st_is_valid(sf_obj))
  cat("  - Geometrías válidas:", nrow(sf_obj) - n_invalidos, "/", nrow(sf_obj), "\n")
  if (n_invalidos > 0) {
    cat("  ⚠ Geometrías inválidas:", n_invalidos, "\n")
  }
  
  # Columnas
  cat("\n📋 COLUMNAS:\n")
  cat("  ", paste(names(sf_obj), collapse = ", "), "\n")
  
  # Área (solo para polígonos)
  if (st_geometry_type(sf_obj, by_geometry = FALSE) %in% c("POLYGON", "MULTIPOLYGON")) {
    areas <- st_area(sf_obj)
    cat("\n📐 ÁREA (para polígonos):\n")
    cat("  - Total:", round(sum(areas), 2), units(areas), "\n")
    cat("  - Promedio:", round(mean(areas), 2), units(areas), "\n")
    cat("  - Rango: [", round(min(areas), 2), ",", round(max(areas), 2), "]", 
        units(areas), "\n")
  }
  
  cat("\n", rep("=", 60), "\n\n", sep = "")
  
  invisible(NULL)
}

# -----------------------------------------------------------------------------
# 2. FUNCIÓN: ESTANDARIZAR CRS
# -----------------------------------------------------------------------------

#' Estandariza CRS de un objeto sf
#' 
#' @param sf_obj Objeto sf a transformar
#' @param crs_target CRS objetivo (EPSG code o proj4string)
#' @param verbose Mostrar mensajes (default TRUE)
#' @return Objeto sf transformado
#' @examples
#' poligonos_utm <- estandarizar_crs(poligonos, 32719)
estandarizar_crs <- function(sf_obj, crs_target, verbose = TRUE) {
  
  # Validar
  if (!inherits(sf_obj, "sf")) {
    stop("El objeto no es de clase 'sf'")
  }
  
  crs_original <- st_crs(sf_obj)
  
  # Si ya está en el CRS objetivo, no hacer nada
  if (crs_original == st_crs(crs_target)) {
    if (verbose) {
      cat("✓ Objeto ya está en CRS", crs_target, "\n")
    }
    return(sf_obj)
  }
  
  # Transformar
  if (verbose) {
    cat("→ Transformando CRS:\n")
    cat("  - Origen:", crs_original$input, "\n")
    cat("  - Destino:", crs_target, "\n")
  }
  
  sf_transformado <- st_transform(sf_obj, crs_target)
  
  if (verbose) {
    cat("✓ Transformación completada\n")
  }
  
  return(sf_transformado)
}

# -----------------------------------------------------------------------------
# 3. FUNCIÓN: GUARDAR FIGURAS CON NOMBRES CONSISTENTES
# -----------------------------------------------------------------------------

#' Guarda figuras con nombres y configuraciones consistentes
#' 
#' @param plot Objeto ggplot o plot base
#' @param nombre Nombre base del archivo (sin extensión)
#' @param directorio Directorio de salida (default "outputs")
#' @param width Ancho en pulgadas (default 10)
#' @param height Alto en pulgadas (default 8)
#' @param dpi Resolución (default 300)
#' @param formato Formato de salida (default "png", opciones: "png", "pdf", "jpg")
#' @return Path completo del archivo guardado
#' @examples
#' guardar_figura(mi_mapa, "mapa_eventos", width = 12, height = 9)
guardar_figura <- function(plot, nombre, directorio = "outputs", 
                          width = 10, height = 8, dpi = 300, formato = "png") {
  
  # Crear directorio si no existe
  if (!dir.exists(directorio)) {
    dir.create(directorio, recursive = TRUE)
  }
  
  # Construir path
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  filename <- paste0(nombre, "_", timestamp, ".", formato)
  filepath <- file.path(directorio, filename)
  
  # Guardar según formato
  if (formato == "png") {
    ggplot2::ggsave(filepath, plot, width = width, height = height, dpi = dpi)
  } else if (formato == "pdf") {
    ggplot2::ggsave(filepath, plot, width = width, height = height)
  } else if (formato == "jpg") {
    ggplot2::ggsave(filepath, plot, width = width, height = height, dpi = dpi)
  } else {
    stop("Formato no soportado. Usar 'png', 'pdf' o 'jpg'")
  }
  
  cat("✓ Figura guardada:", filepath, "\n")
  
  return(invisible(filepath))
}

# -----------------------------------------------------------------------------
# 4. FUNCIÓN: PLANTILLA DE TEMA GRÁFICO CONSISTENTE
# -----------------------------------------------------------------------------

#' Tema ggplot2 consistente para mapas
#' 
#' @param base_size Tamaño base de la fuente (default 12)
#' @param base_family Familia de fuente (default "")
#' @return Tema ggplot2
#' @examples
#' ggplot() + geom_sf(data = poligonos) + tema_mapa()
tema_mapa <- function(base_size = 12, base_family = "") {
  ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = base_size * 1.2),
      plot.subtitle = ggplot2::element_text(size = base_size * 0.9, 
                                            color = "gray30"),
      plot.caption = ggplot2::element_text(size = base_size * 0.8, 
                                          color = "gray50", hjust = 0),
      legend.position = "right",
      legend.title = ggplot2::element_text(face = "bold", size = base_size * 0.9),
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = "white", color = NA)
    )
}

# -----------------------------------------------------------------------------
# 5. FUNCIÓN: VALIDAR Y LIMPIAR GEOMETRÍAS
# -----------------------------------------------------------------------------

#' Valida y repara geometrías inválidas
#' 
#' @param sf_obj Objeto sf a validar
#' @param reparar Si TRUE, intenta reparar geometrías inválidas (default TRUE)
#' @param verbose Mostrar mensajes (default TRUE)
#' @return Objeto sf con geometrías validadas/reparadas
#' @examples
#' poligonos_limpios <- validar_geometrias(poligonos)
validar_geometrias <- function(sf_obj, reparar = TRUE, verbose = TRUE) {
  
  if (!inherits(sf_obj, "sf")) {
    stop("El objeto no es de clase 'sf'")
  }
  
  # Verificar validez
  validas <- st_is_valid(sf_obj)
  n_invalidas <- sum(!validas)
  
  if (verbose) {
    cat("→ Validando geometrías...\n")
    cat("  - Geometrías válidas:", sum(validas), "/", length(validas), "\n")
    cat("  - Geometrías inválidas:", n_invalidas, "\n")
  }
  
  # Si hay inválidas y se solicita reparar
  if (n_invalidas > 0 && reparar) {
    if (verbose) {
      cat("  → Reparando geometrías inválidas...\n")
    }
    
    sf_obj <- st_make_valid(sf_obj)
    
    # Verificar de nuevo
    validas_post <- st_is_valid(sf_obj)
    n_invalidas_post <- sum(!validas_post)
    
    if (verbose) {
      cat("  ✓ Reparación completada\n")
      cat("  - Geometrías inválidas restantes:", n_invalidas_post, "\n")
    }
    
    if (n_invalidas_post > 0) {
      warning("No se pudieron reparar todas las geometrías")
    }
  }
  
  return(sf_obj)
}

# -----------------------------------------------------------------------------
# 6. FUNCIÓN: CALCULAR CENTROIDE DE POLÍGONOS
# -----------------------------------------------------------------------------

#' Calcula centroides de polígonos
#' 
#' @param sf_obj Objeto sf de polígonos
#' @param point_on_surface Si TRUE, garantiza punto dentro del polígono (default FALSE)
#' @return Objeto sf con geometría POINT
#' @examples
#' centroides <- calcular_centroides(poligonos, point_on_surface = TRUE)
calcular_centroides <- function(sf_obj, point_on_surface = FALSE) {
  
  if (!inherits(sf_obj, "sf")) {
    stop("El objeto no es de clase 'sf'")
  }
  
  tipo_geom <- st_geometry_type(sf_obj, by_geometry = FALSE)
  if (!tipo_geom %in% c("POLYGON", "MULTIPOLYGON")) {
    stop("El objeto debe ser de tipo POLYGON o MULTIPOLYGON")
  }
  
  if (point_on_surface) {
    # Garantiza que el punto está dentro del polígono
    centroides <- st_point_on_surface(sf_obj)
  } else {
    # Centroide geométrico (puede estar fuera en polígonos cóncavos)
    centroides <- st_centroid(sf_obj)
  }
  
  return(centroides)
}

# -----------------------------------------------------------------------------
# 7. FUNCIÓN: CREAR BUFFER CON VALIDACIÓN
# -----------------------------------------------------------------------------

#' Crea buffers con validación de CRS
#' 
#' @param sf_obj Objeto sf
#' @param distancia Distancia del buffer
#' @param unidad Unidad de distancia ("m", "km", etc.) - solo para verificación
#' @param disolver Disolver buffers superpuestos (default FALSE)
#' @return Objeto sf con buffers
#' @examples
#' buffers <- crear_buffer(hospitales_utm, 500, "m")
crear_buffer <- function(sf_obj, distancia, unidad = "m", disolver = FALSE) {
  
  if (!inherits(sf_obj, "sf")) {
    stop("El objeto no es de clase 'sf'")
  }
  
  # Verificar que el CRS sea proyectado
  if (st_is_longlat(sf_obj)) {
    stop("El CRS debe ser proyectado (en metros) para crear buffers. 
         Usar st_transform() primero.")
  }
  
  # Verificar unidades del CRS
  crs_units <- st_crs(sf_obj)$units
  if (!is.null(crs_units) && crs_units != unidad) {
    warning(paste0("Las unidades del CRS (", crs_units, 
                  ") no coinciden con las especificadas (", unidad, ")"))
  }
  
  # Crear buffers
  cat("→ Creando buffers de", distancia, unidad, "...\n")
  buffers <- st_buffer(sf_obj, dist = distancia)
  
  # Disolver si se solicita
  if (disolver) {
    cat("  → Disolviendo buffers superpuestos...\n")
    buffers <- st_union(buffers) %>%
      st_sf(geometry = .)
  }
  
  cat("✓ Buffers creados:", nrow(buffers), "geometrías\n")
  
  return(buffers)
}

# -----------------------------------------------------------------------------
# 8. FUNCIÓN: COMPARAR DOS OBJETOS SF
# -----------------------------------------------------------------------------

#' Compara dos objetos sf para verificar compatibilidad
#' 
#' @param sf_obj1 Primer objeto sf
#' @param sf_obj2 Segundo objeto sf
#' @param nombre1 Nombre del primer objeto (para reportes)
#' @param nombre2 Nombre del segundo objeto (para reportes)
#' @return Lista con resultados de comparación
#' @examples
#' comparar_sf(poligonos, hospitales, "Polígonos", "Hospitales")
comparar_sf <- function(sf_obj1, sf_obj2, nombre1 = "Objeto 1", nombre2 = "Objeto 2") {
  
  cat("\n", rep("=", 60), "\n", sep = "")
  cat("COMPARACIÓN:", nombre1, "vs", nombre2, "\n")
  cat(rep("=", 60), "\n\n", sep = "")
  
  # CRS
  crs1 <- st_crs(sf_obj1)
  crs2 <- st_crs(sf_obj2)
  mismo_crs <- crs1 == crs2
  
  cat("🌍 SISTEMA DE COORDENADAS:\n")
  cat("  -", nombre1, ":", crs1$input, "\n")
  cat("  -", nombre2, ":", crs2$input, "\n")
  cat("  - ¿Mismo CRS?:", ifelse(mismo_crs, "✓ Sí", "✗ No"), "\n")
  
  # Bounding boxes
  bbox1 <- st_bbox(sf_obj1)
  bbox2 <- st_bbox(sf_obj2)
  
  cat("\n📍 COBERTURA ESPACIAL:\n")
  cat("  -", nombre1, "BBox: [", 
      paste(round(bbox1, 2), collapse = ", "), "]\n")
  cat("  -", nombre2, "BBox: [", 
      paste(round(bbox2, 2), collapse = ", "), "]\n")
  
  # Intersección de bboxes
  if (mismo_crs) {
    intersecta <- !(bbox1["xmax"] < bbox2["xmin"] || 
                   bbox1["xmin"] > bbox2["xmax"] ||
                   bbox1["ymax"] < bbox2["ymin"] || 
                   bbox1["ymin"] > bbox2["ymax"])
    cat("  - ¿BBoxes se intersectan?:", ifelse(intersecta, "✓ Sí", "✗ No"), "\n")
  } else {
    cat("  - No se puede verificar intersección (CRS diferentes)\n")
  }
  
  # Tipos de geometría
  tipo1 <- st_geometry_type(sf_obj1, by_geometry = FALSE)
  tipo2 <- st_geometry_type(sf_obj2, by_geometry = FALSE)
  
  cat("\n📐 GEOMETRÍA:\n")
  cat("  -", nombre1, ":", tipo1, "\n")
  cat("  -", nombre2, ":", tipo2, "\n")
  
  cat("\n", rep("=", 60), "\n\n", sep = "")
  
  # Devolver resultados como lista
  resultado <- list(
    mismo_crs = mismo_crs,
    crs1 = crs1$input,
    crs2 = crs2$input,
    bbox_intersecta = if(mismo_crs) intersecta else NA,
    tipo1 = tipo1,
    tipo2 = tipo2
  )
  
  return(invisible(resultado))
}

# -----------------------------------------------------------------------------
# 9. FUNCIÓN: EXPORTAR MÚLTIPLES FORMATOS
# -----------------------------------------------------------------------------

#' Exporta objeto sf a múltiples formatos
#' 
#' @param sf_obj Objeto sf a exportar
#' @param nombre Nombre base del archivo
#' @param directorio Directorio de salida (default "data")
#' @param formatos Vector de formatos (default c("shp", "geojson"))
#' @return Vector de paths de archivos creados
#' @examples
#' exportar_sf(poligonos_limpios, "poligonos_final", formatos = c("shp", "geojson", "gpkg"))
exportar_sf <- function(sf_obj, nombre, directorio = "data", 
                        formatos = c("shp", "geojson")) {
  
  if (!inherits(sf_obj, "sf")) {
    stop("El objeto no es de clase 'sf'")
  }
  
  # Crear directorio si no existe
  if (!dir.exists(directorio)) {
    dir.create(directorio, recursive = TRUE)
  }
  
  paths_creados <- c()
  
  for (formato in formatos) {
    
    if (formato == "shp") {
      path <- file.path(directorio, paste0(nombre, ".shp"))
      st_write(sf_obj, path, delete_dsn = TRUE, quiet = TRUE)
      
    } else if (formato == "geojson") {
      path <- file.path(directorio, paste0(nombre, ".geojson"))
      st_write(sf_obj, path, delete_dsn = TRUE, quiet = TRUE)
      
    } else if (formato == "gpkg") {
      path <- file.path(directorio, paste0(nombre, ".gpkg"))
      st_write(sf_obj, path, delete_dsn = TRUE, quiet = TRUE)
      
    } else if (formato == "csv") {
      # Exportar como CSV (sin geometría, solo atributos)
      path <- file.path(directorio, paste0(nombre, ".csv"))
      df <- sf_obj %>% st_drop_geometry()
      write.csv(df, path, row.names = FALSE)
      
    } else {
      warning(paste("Formato no soportado:", formato))
      next
    }
    
    cat("✓ Exportado:", path, "\n")
    paths_creados <- c(paths_creados, path)
  }
  
  return(invisible(paths_creados))
}

# -----------------------------------------------------------------------------
# 10. FUNCIÓN: RESUMEN ESTADÍSTICO ESPACIAL
# -----------------------------------------------------------------------------

#' Genera resumen estadístico de una variable por unidad espacial
#' 
#' @param sf_obj Objeto sf
#' @param variable Nombre de la variable a resumir
#' @param agrupar_por Columna por la cual agrupar (opcional)
#' @return Dataframe con estadísticas
#' @examples
#' resumen_estadistico(eventos_con_zona, "severidad", "zona_nombre")
resumen_estadistico <- function(sf_obj, variable, agrupar_por = NULL) {
  
  if (!inherits(sf_obj, "sf")) {
    stop("El objeto no es de clase 'sf'")
  }
  
  if (!variable %in% names(sf_obj)) {
    stop(paste("La variable", variable, "no existe en el objeto"))
  }
  
  df <- sf_obj %>% st_drop_geometry()
  
  if (is.null(agrupar_por)) {
    # Resumen global
    resumen <- df %>%
      summarise(
        n = n(),
        media = mean(.data[[variable]], na.rm = TRUE),
        mediana = median(.data[[variable]], na.rm = TRUE),
        desv_std = sd(.data[[variable]], na.rm = TRUE),
        minimo = min(.data[[variable]], na.rm = TRUE),
        maximo = max(.data[[variable]], na.rm = TRUE),
        q25 = quantile(.data[[variable]], 0.25, na.rm = TRUE),
        q75 = quantile(.data[[variable]], 0.75, na.rm = TRUE),
        n_na = sum(is.na(.data[[variable]]))
      )
  } else {
    # Resumen por grupo
    if (!agrupar_por %in% names(df)) {
      stop(paste("La columna", agrupar_por, "no existe en el objeto"))
    }
    
    resumen <- df %>%
      group_by(.data[[agrupar_por]]) %>%
      summarise(
        n = n(),
        media = mean(.data[[variable]], na.rm = TRUE),
        mediana = median(.data[[variable]], na.rm = TRUE),
        desv_std = sd(.data[[variable]], na.rm = TRUE),
        minimo = min(.data[[variable]], na.rm = TRUE),
        maximo = max(.data[[variable]], na.rm = TRUE),
        n_na = sum(is.na(.data[[variable]])),
        .groups = "drop"
      )
  }
  
  return(resumen)
}

# -----------------------------------------------------------------------------
# MENSAJE DE CARGA
# -----------------------------------------------------------------------------

cat("\n✓ Funciones auxiliares cargadas\n\n")
cat("Funciones disponibles:\n")
cat("  1. reporte_sf() - Reporte rápido de objeto sf\n")
cat("  2. estandarizar_crs() - Transformar CRS\n")
cat("  3. guardar_figura() - Guardar plots con nombres consistentes\n")
cat("  4. tema_mapa() - Tema ggplot2 para mapas\n")
cat("  5. validar_geometrias() - Validar y reparar geometrías\n")
cat("  6. calcular_centroides() - Centroides de polígonos\n")
cat("  7. crear_buffer() - Buffers con validación\n")
cat("  8. comparar_sf() - Comparar dos objetos sf\n")
cat("  9. exportar_sf() - Exportar a múltiples formatos\n")
cat("  10. resumen_estadistico() - Estadísticas por grupo\n")
cat("\nUso: Cargar con source('scripts/99_helpers_optional.R')\n\n")
