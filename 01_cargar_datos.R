# =====================================================
# 01_cargar_datos.R
# Carga de datos SIVIGILA y población
# =====================================================

source("00_config.R")

# --- Cargar datos SIVIGILA ---
message("Cargando datos SIVIGILA...")
SIVIGILA_debugged_v2 <- readRDS(RUTAS$sivigila)

# --- Población femenina por departamento (2025) ---
message("Preparando tabla de población...")
pop_fem_depto <- tribble(
  ~cod_dpto, ~depto,                ~pop_fem,
  "05","Antioquia",3511904, "08","Atlántico",1435021, "11","Bogotá D.C.",4073592,
  "13","Bolívar",1113276,   "15","Boyacá",645247,      "17","Caldas",538315,
  "18","Caquetá",212120,    "19","Cauca",789780,       "20","Cesar",711373,
  "23","Córdoba",976570,    "25","Cundinamarca",1707716,"27","Chocó",292827,
  "41","Huila",592216,      "44","La Guajira",525562,  "47","Magdalena",753466,
  "50","Meta",563430,       "52","Nariño",863371,      "54","Norte de Santander",847927,
  "63","Quindío",287301,    "66","Risaralda",517123,   "68","Santander",1201688,
  "70","Sucre",501742,      "73","Tolima",694725,      "76","Valle del Cauca",2450458,
  "81","Arauca",137421,     "85","Casanare",230764,    "86","Putumayo",190271,
  "88","San Andrés",32286,  "91","Amazonas",40225,     "94","Guainía",27109,
  "95","Guaviare",39551,    "97","Vaupés",20724,       "99","Vichada",64102
) %>% 
  mutate(cod_dpto = fmt2(cod_dpto)) %>% 
  arrange(cod_dpto)

# --- Conteo de casos por residencia ---
message("Calculando conteos por departamento de residencia...")
conteo_residencia_named <- SIVIGILA_debugged_v2 %>%
  filter(!is.na(cod_dpto_r), cod_dpto_r != "0") %>%  
  group_by(cod_dpto_r) %>%
  summarise(n = n(), .groups = "drop") %>%
  left_join(pop_fem_depto %>% select(cod_dpto, depto),
            by = c("cod_dpto_r" = "cod_dpto")) %>%
  select(cod_dpto_r, depto, n) %>%
  arrange(desc(n))

# Tabla de casos observados
y_depto <- conteo_residencia_named %>%
  transmute(
    cod_dpto = fmt2(cod_dpto_r),
    y_obs    = as.integer(n)
  )

message("Datos cargados correctamente")
message(sprintf("  - Total casos: %d", sum(y_depto$y_obs)))
message(sprintf("  - Departamentos con casos: %d", nrow(y_depto)))
