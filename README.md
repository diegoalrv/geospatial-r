# Análisis Geoespacial en R - Material de Clase

Este repositorio contiene scripts completos y reproducibles para una clase de análisis geoespacial en R, cubriendo desde configuración básica hasta análisis avanzados de proximidad y agregación espacial.

## 📋 Contenido

### Scripts Principales

1. **00_setup_and_sanity_checks.R** - Configuración y validación inicial
2. **01_demo_table_to_sf_join.R** - De CSV a puntos espaciales + join
3. **02_demo_proximity_buffers.R** - Análisis de proximidad con buffers
4. **03_demo_scale_aggregation.R** - Agregación espacial y MAUP
5. **99_helpers_optional.R** - Funciones auxiliares (opcional)

## 🎯 Objetivos Pedagógicos

### Demo 00: Setup y Sanity Checks
**Objetivo**: Establecer buenas prácticas desde el minuto 1

**Aprenderás**:
- Cargar y configurar librerías espaciales
- Validar calidad de datos geoespaciales
- Verificar sistemas de coordenadas (CRS)
- Detectar y reparar geometrías inválidas
- Crear reportes de estado de datasets

**Conceptos clave**:
- CRS geográfico vs proyectado
- Geometrías válidas/inválidas
- Bounding boxes
- Normalización de datos

### Demo 01: CSV → Puntos → Join Espacial
**Objetivo**: Convertir tablas con coordenadas en datos espaciales y enriquecerlos

**Aprenderás**:
- Convertir CSV con lat/lon a objeto `sf`
- Realizar join espacial (puntos con polígonos)
- Calcular métricas agregadas por zona
- Normalizar con tasas poblacionales
- Visualizar resultados en mapas coropléticos

**Conceptos clave**:
- `st_as_sf()` - conversión a espacial
- `st_join()` - joins espaciales
- Tasas vs conteos absolutos
- Validación visual

**Outputs**:
- `01_mapa_conteo_eventos.png` - Mapa coroplético por conteo
- `01_mapa_tasa_eventos.png` - Mapa coroplético por tasa
- `01_mapa_validacion_puntos.png` - Validación visual
- `01_resumen_por_zona.csv` - Tabla resumen
- `01_eventos_con_zona.csv` - Eventos enriquecidos

### Demo 02: Análisis de Proximidad
**Objetivo**: Usar buffers para analizar accesibilidad e exposición

**Aprenderás**:
- Crear buffers (áreas de proximidad)
- Importancia del CRS proyectado
- Disolver buffers superpuestos
- Clasificar eventos por distancia
- Análisis de sensibilidad
- Calcular métricas de exposición

**Conceptos clave**:
- `st_buffer()` - áreas de proximidad
- CRS en metros (UTM) para buffers
- Distancia euclidiana vs accesibilidad real
- Análisis de sensibilidad a parámetros

**Outputs**:
- `02_mapa_buffers_proximidad.png` - Mapa con buffers
- `02_mapa_buffers_individual.png` - Comparación individual vs disuelto
- `02_grafico_sensibilidad.png` - Sensibilidad a distancia
- `02_exposicion_por_distancia.csv` - Métricas de exposición
- `02_eventos_clasificados_proximidad.csv` - Eventos clasificados
- `02_ranking_hospitales_500m.csv` - Ranking por exposición

### Demo 03: Escala y Agregación
**Objetivo**: Demostrar el problema de la unidad de análisis modificable (MAUP)

**Aprenderás**:
- Crear grillas hexagonales
- Agregar datos por diferentes unidades
- Comparar resultados entre agregaciones
- Interpretar el MAUP
- Justificar elección de unidad espacial

**Conceptos clave**:
- MAUP (Modifiable Areal Unit Problem)
- `st_make_grid()` - grillas hexagonales
- Unidades administrativas vs uniformes
- Sensibilidad de resultados a la escala
- Trade-offs en elección de unidad

**Outputs**:
- `03_comparacion_agregaciones.png` - Comparación lado a lado
- `03_secuencia_agregacion.png` - Evolución del análisis
- `03_histograma_comparativo.png` - Distribuciones comparadas
- `03_resumen_administrativo.csv` - Resumen por zona
- `03_resumen_hexagonal.csv` - Resumen por hexágono
- `03_comparacion_estadisticas.csv` - Estadísticas comparativas

## 🚀 Cómo Usar

### Requisitos Previos

```r
# Instalar paquetes necesarios
install.packages(c(
  "sf",           # Manejo de datos espaciales
  "dplyr",        # Manipulación de datos
  "ggplot2",      # Visualización
  "units",        # Unidades
  "readr",        # Lectura de CSV
  "tidyr",        # Limpieza de datos
  "scales",       # Escalas para gráficos
  "patchwork"     # Combinar gráficos
))
```

### Ejecución Secuencial

**Opción 1: Ejecutar todo**
```r
# En R o RStudio
source("scripts/00_setup_and_sanity_checks.R")
source("scripts/01_demo_table_to_sf_join.R")
source("scripts/02_demo_proximity_buffers.R")
source("scripts/03_demo_scale_aggregation.R")
```

**Opción 2: Ejecutar paso a paso** (recomendado para clase)
```r
# Paso 1: Setup
source("scripts/00_setup_and_sanity_checks.R")
# Revisar outputs en outputs/ antes de continuar

# Paso 2: Join espacial
source("scripts/01_demo_table_to_sf_join.R")
# Discutir mapas y conceptos

# Paso 3: Proximidad
source("scripts/02_demo_proximity_buffers.R")
# Analizar buffers y métricas

# Paso 4: Agregación
source("scripts/03_demo_scale_aggregation.R")
# Comparar resultados según escala
```

### Funciones Auxiliares (Opcional)

```r
# Cargar funciones de utilidad
source("scripts/99_helpers_optional.R")

# Ejemplos de uso:
reporte_sf(poligonos, "Polígonos administrativos")
poligonos_utm <- estandarizar_crs(poligonos, 32719)
comparar_sf(poligonos, hospitales, "Zonas", "Hospitales")
```

## 📊 Datos Simulados

Los scripts generan datos simulados pero realistas de Santiago de Chile:

- **6 zonas administrativas** (polígonos)
- **15 hospitales** (puntos)
- **500 eventos de emergencia** (puntos)

Esto permite ejecutar los ejemplos sin archivos externos. Para usar tus propios datos:

```r
# Reemplazar en 00_setup_and_sanity_checks.R
poligonos <- st_read("ruta/a/tus/poligonos.shp")
hospitales <- st_read("ruta/a/tus/puntos.geojson")
eventos_raw <- read_csv("ruta/a/tus/eventos.csv")
```

## 🎓 Conceptos Clave por Demo

### Conceptos Fundamentales

1. **Sistema de Coordenadas (CRS)**
   - Geográfico (lat/lon, EPSG 4326): para mapas web
   - Proyectado (UTM, metros): para análisis métricos

2. **Geometrías Espaciales**
   - POINT: eventos, hospitales
   - POLYGON: zonas administrativas, buffers
   - Validez geométrica

3. **Operaciones Espaciales**
   - Join espacial: asignar atributos por ubicación
   - Buffer: áreas de proximidad
   - Intersección: clasificar por pertenencia

### Lecciones Metodológicas

**Demo 01**:
- ✓ Siempre verificar coordenadas antes de convertir
- ✓ Orden en `st_as_sf()` es `coords = c('lon', 'lat')`
- ✓ Usar tasas poblacionales para comparar zonas
- ✓ Validar visualmente los joins

**Demo 02**:
- ⚠️ Buffers requieren CRS proyectado (metros)
- ⚠️ Distancia es un supuesto que debe justificarse
- ⚠️ Proximidad ≠ accesibilidad real
- ✓ Disolver buffers para análisis agregados
- ✓ Analizar sensibilidad a parámetros

**Demo 03**:
- 🎯 La unidad de análisis NO es neutral
- 🎯 MAUP es inevitable, no evitable
- 🎯 Justificar la elección según objetivo
- 🎯 Probar múltiples escalas
- ✓ Hexágonos = comparables; Admin = interpretables

## 📚 Recursos Adicionales

### Librerías Principales

- [sf](https://r-spatial.github.io/sf/) - Simple Features for R
- [ggplot2](https://ggplot2.tidyverse.org/) - Visualización
- [dplyr](https://dplyr.tidyverse.org/) - Manipulación de datos

### Lecturas Recomendadas

**Sobre MAUP**:
- Openshaw, S. (1984). The Modifiable Areal Unit Problem
- Dark, S. J., & Bram, D. (2007). The modifiable areal unit problem (MAUP) in physical geography
- Fotheringham, A. S., & Wong, D. W. (1991). The modifiable areal unit problem in multivariate statistical analysis

**Análisis Espacial en R**:
- Lovelace, R., Nowosad, J., & Muenchow, J. (2019). Geocomputation with R
- Bivand, R. S., Pebesma, E., & Gomez-Rubio, V. (2013). Applied spatial data analysis with R

### Tutoriales Online

- [Geocomputation with R](https://geocompr.robinlovelace.net/)
- [sf cheatsheet](https://github.com/rstudio/cheatsheets/blob/main/sf.pdf)
- [Spatial Data Science](https://r-spatial.org/book/)

## ⚠️ Advertencias Metodológicas

### Limitaciones del Análisis de Proximidad (Demo 02)

- **Distancia euclidiana ≠ distancia real**: No considera red vial
- **Sin barreras físicas**: Ignora ríos, autopistas, cerros
- **Sin capacidad**: No considera disponibilidad de recursos
- **Proximidad ≠ accesibilidad**: Accesibilidad es multidimensional

### Consideraciones del MAUP (Demo 03)

- No existe una "unidad correcta" universal
- Resultados son sensibles a la agregación elegida
- Reportar múltiples escalas cuando sea posible
- Justificar elección según el problema específico

## 🔧 Troubleshooting

### Problema: "CRS must be projected"
```r
# Solución: Transformar a UTM antes de buffers
objeto_utm <- st_transform(objeto, 32719)  # UTM 19S para Santiago
```

### Problema: "Geometrías inválidas"
```r
# Solución: Reparar con st_make_valid()
objeto_limpio <- st_make_valid(objeto)
```

### Problema: "No se ven los mapas"
```r
# Solución: Revisar directorio outputs/
list.files("outputs/", pattern = ".png")
```
## 🌍 Datos de Fuentes Abiertas (Open Data)

Además de los datos simulados incluidos en este repositorio, puedes ejecutar estos scripts con datasets reales descargables gratuitamente (Chile + global). Abajo tienes fuentes confiables y comunes en investigación aplicada.

**Recomendación**: descarga datos en formatos GeoJSON / Shapefile / CSV, y siempre verifica su CRS.

### 🇨🇱 Datos Abiertos Chile

#### 1) IDE Chile (Infraestructura de Datos Espaciales)

Portal oficial con capas GIS (administrativas, ambientales, hidrografía, etc.).

🔗 https://www.ide.cl/

**Útil para**:
- Límites comunales/regionales
- Cartografía base
- Capas institucionales (según disponibilidad)

#### 2) Datos Abiertos Gobierno de Chile

Portal general de datos abiertos (tabulares + espaciales).

🔗 https://www.datos.gob.cl/

**Útil para**:
- Indicadores comunales
- Catastros, equipamientos, estadísticas
- Datasets CSV para joins espaciales

#### 3) BCNE (Biblioteca del Congreso Nacional)

Muy usado para divisiones político-administrativas + cartografía.

🔗 https://www.bcn.cl/siit/mapas_vectoriales

**Útil para**:
- Comunas / regiones / provincias en vectores

#### 4) INE (Instituto Nacional de Estadísticas)

Principalmente datos tabulares (ideal para joins por comuna/zona).

🔗 https://www.ine.gob.cl/

**Útil para**:
- Población por comuna
- Series temporales

### 🌐 Datos Abiertos Globales

#### 1) OpenStreetMap (OSM)

La fuente más importante del mundo para datos geográficos abiertos.

🔗 https://www.openstreetmap.org/  
🔗 Extractos (recomendado): https://download.geofabrik.de/

**Útil para**:
- Calles, edificios, puntos de interés (POIs)
- Hospitales, escuelas, paraderos
- Redes de transporte

#### 2) Overpass Turbo (consulta OSM sin descargar todo)

Permite descargar puntos/POIs directamente desde OSM con query.

🔗 https://overpass-turbo.eu/

**Ejemplos de uso**:
- Hospitales de una ciudad
- Estaciones de metro
- Colegios

#### 3) Natural Earth (vectores listos para usar)

Muy usado en clases y demos por su calidad.

🔗 https://www.naturalearthdata.com/

**Útil para**:
- Países, regiones, costas
- Datasets globales livianos

#### 4) GADM (límites administrativos globales)

Límites administrativos por país (nivel 0/1/2/3…).

🔗 https://gadm.org/

**Útil para**:
- Límites administrativos en cualquier país

#### 5) Open Data de WorldPop (población gridded)

Raster/población espacial.

🔗 https://www.worldpop.org/

**Útil para**:
- Tasas por población
- Exposición y densidad poblacional

#### 6) Copernicus (Sentinel)

Satélite e imágenes para análisis ambiental/territorial.

🔗 https://dataspace.copernicus.eu/

**Útil para**:
- NDVI, cobertura vegetacional
- Cambios de suelo

### 🚍 Transporte (GTFS)

**GTFS Static**: Datos de transporte público en formato estándar.

🔗 https://gtfs.org/  
🔗 Catálogo global: https://transitfeeds.com/  
🔗 Catálogo alternativo: https://mobilitydatabase.org/

**Útil para**:
- Paraderos, rutas, horarios
- Análisis de accesibilidad

### 🧪 Recomendación de Datasets por Demo

#### Demo 01 (CSV → sf + join espacial)
- **Dataset CSV**: eventos con lat/lon (accidentes, delitos, reclamos, etc.)
- **Polígonos**: comunas/barrios/zonas censales (BCNE, IDE)

#### Demo 02 (Buffers)
- **Puntos de equipamiento**: hospitales/colegios/centros de salud (OSM via Overpass)
- **Eventos**: puntos (CSV)
- Buffers y ranking por exposición

#### Demo 03 (MAUP)
Usar eventos georreferenciados y comparar:
- Agregación administrativa (comunas/zonas)
- Grillas uniformes (hexágonos)

### Extensiones Posibles

- Agregar análisis de red (routing)
- Incluir análisis temporal
- Incorporar datos de población raster
- Análisis de clustering espacial
- Regresiones espaciales

## 📄 Licencia

Material educativo de libre uso. Atribución apreciada pero no requerida.

## 📧 Contacto

Para preguntas, sugerencias o reportar errores en los scripts.
d.alexis.ramirez at gmail dot com

---

**Última actualización**: Enero 2026
**Versión**: 1.0
**Lenguaje**: R (≥ 4.0.0)
