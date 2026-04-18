# Modelo Espacial Bivariado — Violencia contra la Mujer en Colombia

Modelo espacial bayesiano BYM2 bivariado estimado con R-INLA para analizar el
riesgo relativo departamental de violencia **sexual** y **no sexual** contra la mujer
en Colombia, a partir de datos SIVIGILA.

---

## Estructura del repositorio

```text
.
├── Dos_Tipos_Articulo.R            ← SCRIPT PRINCIPAL del artículo (bivariado)
├── Expected_bivariado_Articulo.R   ← Conteos esperados (bivariado)
├── Expected_univariado_Articulo.R  ← Conteos esperados (univariado)
├── predicciones.R                  ← Predicciones 2030
│
├── main.R                          ← Ejecuta el pipeline univariado completo
├── 00_config.R                     ← Librerías, rutas y parámetros
├── 01_cargar_datos.R               ← Carga SIVIGILA y población
├── 02_procesar_geometria.R         ← Shapefile departamental
├── 03_preparar_datos_modelo.R      ← Une casos, población y geometría
├── 04_vecindad_grafo.R             ← Grafo de vecindad Queen
├── 05_ajustar_modelos_basicos.R    ← Poisson-BYM2 y ZIP-BYM2
├── 06_busqueda_priors.R            ← Grid de priors PC (calibración)
├── 07_analisis_sensibilidad.R      ← Sensibilidad: Rook vs Queen, offsets
├── 08_visualizaciones.R            ← Mapas y tablas de RR
├── 09_resultados_finales.R         ← Resumen ejecutivo y ranking
├── 11_export_departamentos_geojson.R
│
├── mapas_interactivos/             ← Mapas Leaflet (abrir en navegador)
│   ├── index.html                  → RR violencia no sexual
│   ├── index_sexual.html           → RR violencia sexual
│   ├── index_rr_probabilidades.html      → P(RR > 1.25) no sexual
│   ├── index_rr_probabilidades_sexual.html → P(RR > 1.25) sexual
│   ├── index_tasas_crudas.html     → Tasas crudas no sexual
│   ├── index_tasas_crudas_sexual.html → Tasas crudas sexual
│   ├── index_nombres.html          → Mapa con nombres
│   └── relative_risk_map.html      → Mapa RR alternativo (D3)
│
├── resultados/                     ← Outputs del modelo
│   ├── dat_inla_resultados.csv                    ← RR por departamento
│   ├── dat_inla_resultados_con_probabilidades.csv ← + P(RR > 1.25)
│   ├── tasas_violencia_por_depto.csv
│   ├── ppc_density_response1.png   ← PPC violencia no sexual
│   └── ppc_density_response2.png   ← PPC violencia sexual
│
├── Figuras/                        ← Figuras para el artículo
│
├── datos/                          ← Datos de entrada (ver nota abajo)
│   ├── Data/Espacial/Imagenes/     ← Mapas estáticos PNG
│   ├── Data/Proyecciones/
│   │   └── proyecciones_dep_ano_edad.rds
│   └── Geografia/
│       └── departamentos.adj       ← Grafo de vecindad INLA
│
├── SHP_MGN2018_INTGRD_DEPTO/      ← Shapefile departamentos MGN 2018
├── departamentos.zip               ← Shapefile comprimido (para mapas web)
├── departamentos.geojson           ← GeoJSON departamentos
├── mgn_dptos.geojson
├── depto.adj
│
└── legacy/                         ← Versiones anteriores (no usar)
```

> **Nota sobre datos:** Los archivos de datos SIVIGILA (`.rds`, `.csv` con microdatos)
> están excluidos del repositorio por contener información sensible de salud.
> Solicítalos al equipo del proyecto.

---

## Cómo reproducir el análisis

### Análisis bivariado del artículo

```r
# Directorio de trabajo: raíz del repositorio
setwd("ruta/a/Modelo_Orquidea")
source("Dos_Tipos_Articulo.R")
```

### Pipeline univariado completo

```r
setwd("ruta/a/Modelo_Orquidea")
source("main.R")
```

### Mapas interactivos

Abre directamente en el navegador cualquier `.html` de `mapas_interactivos/`.
No requieren servidor — cargan los datos automáticamente desde `resultados/`.

---

## Dependencias R

```r
install.packages(c(
  "sf", "dplyr", "tidyr", "stringr", "stringi", "tibble",
  "spdep", "ggplot2", "colorspace", "scales", "readr", "readxl", "viridis"
))
# INLA (instalación especial):
install.packages("INLA", repos = c(INLA = "https://inla.r-inla-download.org/R/stable"), dep = TRUE)
```

---

## Modelo

El modelo central es un **BYM2 bivariado** estimado con INLA:

```r
y ~ 0 + resp_f +
  f(id_area, model = "bym2", graph = g,
    group = resp, control.group = list(model = "exchangeable"))
```

- `resp = 1`: violencia no sexual (`def_naturaleza` ∈ {1, 2, 3})
- `resp = 2`: violencia sexual (`def_naturaleza` ∈ {5, 6, 7, 10, 12, 14, 15, ...})
- El término `group = resp` modela correlación espacial compartida entre los dos outcomes.
- Priors PC: `P(σ > 1) = 0.01`, `P(φ < 0.5) = 2/3`
