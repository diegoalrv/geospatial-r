# Guía Rápida de Ejecución

Esta guía muestra la salida esperada de cada script y cómo interpretarla.

## 🚀 Ejecución Rápida (Todo de una vez)

```r
# Ejecutar en este orden
source("scripts/00_setup_and_sanity_checks.R")
source("scripts/01_demo_table_to_sf_join.R")
source("scripts/02_demo_proximity_buffers.R")
source("scripts/03_demo_scale_aggregation.R")
```

## 📋 Demo 00: Setup y Validación

### Ejecución
```r
source("scripts/00_setup_and_sanity_checks.R")
```

### Salida Esperada en Consola
```
✓ Librerías cargadas y directorios creados

→ Generando polígonos administrativos...
✓ Polígonos creados: 6 zonas

→ Generando puntos de hospitales...
✓ Hospitales creados: 15 puntos

→ Generando eventos de emergencia...
✓ Eventos creados: 500 registros

======================================================================
SANITY CHECKS ESPACIALES
======================================================================

→ Verificando estructura de datos...
...
✓ Eventos convertidos a puntos espaciales: 500 puntos

======================================================================
NORMALIZACIÓN DE DATOS
======================================================================

→ Definiendo sistemas de coordenadas...
  - Geográfico (mapas): 4326 (WGS84)
  - Proyectado (análisis): 32719 (UTM 19S)
✓ Objetos transformados a UTM

======================================================================
✓ SETUP COMPLETADO
======================================================================

Objetos disponibles en memoria:
  - poligonos (WGS84) / 6 zonas
  - hospitales (WGS84) / 15 puntos
  - eventos (WGS84) / 500 puntos
  - poligonos_utm (UTM 19S) / 6 zonas
  - hospitales_utm (UTM 19S) / 15 puntos
  - eventos_utm (UTM 19S) / 500 puntos

🎯 Todo listo para las demos siguientes
```

### Archivos Creados
```
data/
  ├── poligonos_clean.rds
  ├── hospitales_clean.rds
  ├── eventos_clean.rds
  ├── poligonos_utm.rds
  ├── hospitales_utm.rds
  └── eventos_utm.rds

outputs/
  ├── 00_validacion_cobertura.png
  └── 00_estado_datasets.csv
```

### Qué Revisar
- ✓ Mapa `00_validacion_cobertura.png` muestra puntos rojos (eventos) dentro de polígonos
- ✓ CSV muestra que todos tienen el mismo CRS y BBoxes compatibles

---

## 📋 Demo 01: CSV a Puntos + Join Espacial

### Ejecución
```r
source("scripts/01_demo_table_to_sf_join.R")
```

### Salida Esperada en Consola
```
======================================================================
PASO 1: CONVERTIR CSV A PUNTOS ESPACIALES
======================================================================

→ Verificando calidad de coordenadas...
  - NAs en latitud: 0
  - NAs en longitud: 0

→ Convirtiendo a objeto sf...
✓ Conversión exitosa:
  - Tipo de objeto: sf
  - Tipo de geometría: POINT
  - CRS: EPSG:4326

======================================================================
PASO 2: JOIN ESPACIAL - ASIGNAR ZONA A CADA EVENTO
======================================================================

✓ Join completado:
  - Eventos con zona asignada: 485
  - Eventos sin zona (fuera del área): 15

======================================================================
PASO 3: ANÁLISIS Y RESÚMENES POR ZONA
======================================================================

→ Calculando conteo de eventos por zona...
# A tibble: 6 × 3
  zona_id zona_nombre       n_eventos
    <dbl> <chr>                 <int>
1       5 Maipú                   112
2       4 Ñuñoa                   105
3       6 La Reina                 73
...

→ Calculando tasas por 10,000 habitantes...
# A tibble: 6 × 5
  zona_id zona_nombre       n_eventos poblacion tasa_por_10k
    <dbl> <chr>                 <int>     <dbl>        <dbl>
1       1 Centro                   71    142800         4.97
2       6 La Reina                 73    105800         6.90
...

======================================================================
✓ DEMO 1 COMPLETADA
======================================================================

📊 Resultados clave:
  - Eventos procesados: 485
  - Zona con más eventos: Maipú ( 112 )
  - Zona con mayor tasa: La Reina ( 6.9 por 10k hab.)

🎓 Lecciones clave:
  1. Siempre verificar coordenadas antes de convertir a sf
  2. Orden en st_as_sf es coords = c('lon', 'lat'), no al revés
  3. El join espacial (st_join) asigna atributos por relación geométrica
  4. Usar tasas poblacionales para comparaciones justas entre zonas
  5. Validar visualmente el resultado del join con un mapa
```

### Archivos Creados
```
outputs/
  ├── 01_mapa_conteo_eventos.png      # Mapa coroplético con números
  ├── 01_mapa_tasa_eventos.png        # Mapa coroplético normalizado
  ├── 01_mapa_validacion_puntos.png   # Puntos sobre polígonos
  ├── 01_resumen_por_zona.csv         # Tabla con métricas
  └── 01_eventos_con_zona.csv         # Eventos enriquecidos
```

### Qué Analizar
1. Comparar los dos mapas coropléticos: ¿Cambia la historia con tasa vs conteo?
2. En el mapa de validación, verificar que los puntos caen dentro de sus zonas
3. Revisar tabla CSV: ¿Qué zona tiene más eventos? ¿Y la mayor tasa?

---

## 📋 Demo 02: Análisis de Proximidad

### Ejecución
```r
source("scripts/02_demo_proximity_buffers.R")
```

### Salida Esperada en Consola
```
======================================================================
PREPARACIÓN: CRS PROYECTADO PARA BUFFERS
======================================================================

→ Verificando sistemas de coordenadas...
  - Polígonos: EPSG:32719
  - Hospitales: EPSG:32719
  - Eventos: EPSG:32719

  ⚠ CRÍTICO: Todos los objetos DEBEN estar en CRS proyectado (metros)
  para que los buffers tengan sentido (ej: buffer de 500m)

✓ CRS confirmado en metros (UTM Zone 19S)

======================================================================
PASO 1: DEFINIR HIPÓTESIS DE PROXIMIDAD
======================================================================

🎯 HIPÓTESIS DE TRABAJO:
  'Los eventos de emergencia cercanos a hospitales tienen mejor
   respuesta. Analizaremos proximidad a 300m, 500m y 1000m'

→ Distancias de análisis seleccionadas:
  - 300 metros ( 0.3 km)
  - 500 metros ( 0.5 km)
  - 1000 metros ( 1 km)

⚠ IMPORTANTE: Estas distancias son SUPUESTOS, no verdades absolutas.

======================================================================
PASO 2: GENERAR BUFFERS ALREDEDOR DE HOSPITALES
======================================================================

→ Generando buffers...
  Procesando buffer de 300 metros...
    ✓ Creados 15 buffers circulares
  Procesando buffer de 500 metros...
    ✓ Creados 15 buffers circulares
  Procesando buffer de 1000 metros...
    ✓ Creados 15 buffers circulares

======================================================================
PASO 3: CLASIFICAR EVENTOS SEGÚN PROXIMIDAD
======================================================================

  ✓ Buffer 300 m: 128 eventos dentro (25.6%)
  ✓ Buffer 500 m: 212 eventos dentro (42.4%)
  ✓ Buffer 1000 m: 356 eventos dentro (71.2%)

→ Creando clasificación definitiva de proximidad...
✓ Clasificación completada
# A tibble: 4 × 3
  categoria_proximidad                  n porcentaje
  <fct>                             <int>      <dbl>
1 Muy cerca (< 300m)                  128       25.6
2 Cerca (300-500m)                     84       16.8
3 Medianamente cerca (500-1000m)      144       28.8
4 Lejos (> 1000m)                     144       28.8

======================================================================
PASO 4: CALCULAR MÉTRICAS DE EXPOSICIÓN
======================================================================

  distancia_m distancia_km n_eventos_dentro pct_dentro
1         300          0.3              128       25.6
2         500          0.5              212       42.4
3        1000          1.0              356       71.2

💡 INTERPRETACIÓN:
  - De 0 a 300m se capturan 128 eventos
  - De 300m a 500m se agregan 84 eventos más
  - De 500m a 1000m se agregan 144 eventos más
  Esto sugiere aumento sostenido al expandir cobertura

======================================================================
✓ DEMO 2 COMPLETADA
======================================================================

⚠️  ADVERTENCIAS METODOLÓGICAS:
  - Distancia euclidiana ≠ distancia de red (rutas reales)
  - No considera barreras físicas (ríos, autopistas, cerros)
  - No considera capacidad o disponibilidad del hospital
  - Proximidad es solo uno de muchos factores de accesibilidad
```

### Archivos Creados
```
outputs/
  ├── 02_mapa_buffers_proximidad.png         # Mapa principal con buffers
  ├── 02_mapa_buffers_individual.png         # Comparación individual/disuelto
  ├── 02_grafico_sensibilidad.png            # Gráfico de línea
  ├── 02_exposicion_por_distancia.csv        # Métricas de cobertura
  ├── 02_eventos_clasificados_proximidad.csv # Eventos con categoría
  └── 02_ranking_hospitales_500m.csv         # Hospitales por exposición
```

### Qué Analizar
1. En el mapa principal: ¿Dónde están las áreas sin cobertura?
2. En el gráfico de sensibilidad: ¿La cobertura crece linealmente o con rendimientos decrecientes?
3. En el ranking: ¿Qué hospitales están más expuestos a eventos?

---

## 📋 Demo 03: Agregación Espacial y MAUP

### Ejecución
```r
source("scripts/03_demo_scale_aggregation.R")
```

### Salida Esperada en Consola
```
======================================================================
AGREGACIÓN A: UNIDADES ADMINISTRATIVAS (ZONAS)
======================================================================

# A tibble: 6 × 6
  zona_id zona_nombre    n_eventos severidad_promedio   tasa_por_10k
    <dbl> <chr>              <int>              <dbl>          <dbl>
1       5 Maipú                112               2.98           1.94
2       4 Ñuñoa                105               2.96           5.03
...

======================================================================
AGREGACIÓN B: GRILLA HEXAGONAL
======================================================================

→ Generando grilla hexagonal...
✓ Grilla creada: 56 hexágonos
  - Tamaño de celda: ~1.11 km

→ Recortando grilla al área de estudio...
✓ Grilla recortada: 42 hexágonos dentro del área

✓ Hexágonos con eventos: 39
  - Métrica principal: Conteo de eventos
  - Rango: 1 - 28

======================================================================
PASO 3: COMPARACIÓN ENTRE AGREGACIONES
======================================================================

→ Estadísticas comparativas:

ADMINISTRATIVA (zonas):
  - Unidades de análisis: 6
  - Eventos por unidad (promedio): 80.8
  - Eventos por unidad (rango): 62 - 112
  - Desv. estándar: 19.7

HEXAGONAL (grilla):
  - Unidades de análisis: 39
  - Eventos por unidad (promedio): 12.4
  - Eventos por unidad (rango): 1 - 28
  - Desv. estándar: 7.3

======================================================================
🎯 PROBLEMA DE LA UNIDAD MODIFICABLE (MAUP)
======================================================================

Al cambiar la unidad de agregación:
  ✓ Cambian los valores de las métricas
  ✓ Cambian los patrones espaciales observados
  ✓ Cambian las áreas identificadas como 'hotspots'
  ✓ Pueden cambiar las conclusiones del análisis

⚠️  IMPLICACIONES:
  1. No existe una 'unidad correcta' universal
  2. La elección debe justificarse según el problema
  3. Es recomendable probar múltiples escalas
  4. Reportar sensibilidad de resultados a la escala

======================================================================
PASO 5: DISCUSIÓN Y RECOMENDACIONES
======================================================================

🤔 PREGUNTAS PARA REFLEXIÓN:

1. ¿Qué unidad usarías para una intervención/política y por qué?

   ADMINISTRATIVA:
   ✓ Alineada con jurisdicciones de gestión
   ✓ Permite calcular tasas poblacionales
   ✓ Facilita comunicación con autoridades
   ✗ Puede ocultar heterogeneidad interna
   ✗ Límites arbitrarios (no siempre naturales)

   HEXAGONAL:
   ✓ Unidades uniformes (comparables entre sí)
   ✓ Revela patrones espaciales más finos
   ✓ No sesgada por límites administrativos
   ✗ No alineada con gestión administrativa
   ✗ Difícil asociar con población o denominador

======================================================================
✓ DEMO 3 COMPLETADA
======================================================================

🎓 Lecciones clave:
  1. La unidad de análisis NO es neutral - cambia los resultados
  2. MAUP (Modifiable Areal Unit Problem) es inevitable
  3. No hay una unidad 'correcta', solo más o menos apropiada
  4. Justificar la elección según objetivo y contexto
  5. Probar sensibilidad con múltiples escalas
  6. Hexágonos = comparables; Admin = interpretables
```

### Archivos Creados
```
outputs/
  ├── 03_comparacion_agregaciones.png       # Lado a lado
  ├── 03_secuencia_agregacion.png           # Puntos → Admin → Hex
  ├── 03_histograma_comparativo.png         # Distribuciones
  ├── 03_resumen_administrativo.csv         # Tabla admin
  ├── 03_resumen_hexagonal.csv              # Tabla hex
  └── 03_comparacion_estadisticas.csv       # Comparación numérica
```

### Qué Analizar
1. En la comparación lado a lado: ¿Los hotspots son los mismos?
2. En la secuencia: ¿Cómo cambia la visualización del dato original al agregado?
3. En los histogramas: ¿Cómo afecta la agregación a la distribución de valores?

---

## 🔍 Verificación Rápida

### Después de ejecutar todo, deberías tener:

```
proyecto/
├── data/
│   ├── poligonos_clean.rds
│   ├── hospitales_clean.rds
│   ├── eventos_clean.rds
│   ├── poligonos_utm.rds
│   ├── hospitales_utm.rds
│   └── eventos_utm.rds
│
└── outputs/
    ├── 00_validacion_cobertura.png
    ├── 00_estado_datasets.csv
    ├── 01_mapa_conteo_eventos.png
    ├── 01_mapa_tasa_eventos.png
    ├── 01_mapa_validacion_puntos.png
    ├── 01_resumen_por_zona.csv
    ├── 01_eventos_con_zona.csv
    ├── 02_mapa_buffers_proximidad.png
    ├── 02_mapa_buffers_individual.png
    ├── 02_grafico_sensibilidad.png
    ├── 02_exposicion_por_distancia.csv
    ├── 02_eventos_clasificados_proximidad.csv
    ├── 02_ranking_hospitales_500m.csv
    ├── 03_comparacion_agregaciones.png
    ├── 03_secuencia_agregacion.png
    ├── 03_histograma_comparativo.png
    ├── 03_resumen_administrativo.csv
    ├── 03_resumen_hexagonal.csv
    └── 03_comparacion_estadisticas.csv
```

**Total esperado**: 6 RDS + 15 archivos de output (9 PNG + 6 CSV)

### Comando para verificar
```r
# En R
list.files("data/", pattern = ".rds")     # Debe mostrar 6 archivos
list.files("outputs/", pattern = ".png")  # Debe mostrar 9 mapas
list.files("outputs/", pattern = ".csv")  # Debe mostrar 6 tablas
```

---

## 💡 Tips para Presentación en Clase

### Orden Recomendado de Visualización

**Introducción (5 min)**
1. Mostrar `00_validacion_cobertura.png` - "Esto es lo que tenemos"
2. Mostrar `00_estado_datasets.csv` - "Así validamos la calidad"

**Demo 1 (15 min)**
1. Mostrar `01_mapa_validacion_puntos.png` - "Puntos con sus zonas"
2. Comparar `01_mapa_conteo_eventos.png` vs `01_mapa_tasa_eventos.png` 
   - "¿Cambia la historia?"
3. Abrir `01_resumen_por_zona.csv` - "Los números detrás del mapa"

**Demo 2 (15 min)**
1. Mostrar `02_mapa_buffers_proximidad.png` - "Áreas de cobertura"
2. Mostrar `02_grafico_sensibilidad.png` - "¿Más distancia = proporcionalmente más cobertura?"
3. Abrir `02_ranking_hospitales_500m.csv` - "¿Qué hospitales están saturados?"

**Demo 3 (20 min)**
1. Mostrar `03_secuencia_agregacion.png` - "Del punto a la agregación"
2. Mostrar `03_comparacion_agregaciones.png` - "Mismos datos, diferente historia"
3. Mostrar `03_histograma_comparativo.png` - "Distribuciones distintas"
4. Abrir `03_comparacion_estadisticas.csv` - "Números que lo confirman"

**Cierre (5 min)**
- Discusión abierta sobre elección de métodos
- Q&A

---

## 🐛 Problemas Comunes

### "Error: package 'sf' is not available"
```r
install.packages("sf")
```

### "Cannot open file 'data/poligonos_clean.rds'"
```r
# Ejecutar primero el script 00
source("scripts/00_setup_and_sanity_checks.R")
```

### "No se generan los mapas"
```r
# Verificar que ggplot2 esté instalado
library(ggplot2)

# Verificar directorio outputs
dir.create("outputs", showWarnings = FALSE)
```

### "CRS transformation error"
```r
# Reinstalar PROJ/GDAL si es necesario
# En Ubuntu/Debian:
# sudo apt-get install libgdal-dev libproj-dev

# Reinstalar sf
install.packages("sf", type = "source")
```

---

## 📊 Ejemplos de Interpretación

### ¿Qué significa una tasa de 6.9 por 10k hab.?
"En La Reina, hay 6.9 eventos de emergencia por cada 10,000 habitantes. 
Esto es ~40% más alto que el promedio del área."

### ¿Por qué 42% de cobertura a 500m es relevante?
"Menos de la mitad de los eventos está a distancia caminable de un hospital. 
Expandir a 1km cubre 71%, pero ¿es caminable 1km en emergencia?"

### ¿Por qué importa el MAUP?
"La zona administrativa dice 'Maipú tiene más eventos', pero los hexágonos 
muestran que están concentrados en 3-4 áreas específicas. La agregación 
administrativa promedia y oculta este patrón."

---

**Última actualización**: Enero 2026
