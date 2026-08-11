# Título: IHEH version ecosistemas
#
# Autor(es): Alejandra Narváez Vallejo
#
# Descripción: Este script maestro coordina el cálculo de la Huella Espacial Humana (IHEH) bajo el enfoque
# de ecosistemas. Ejecuta y articula las rutinas de procesamiento de las variables explicatorias (uso del suelo,
# población, fragmentación e infraestructura vial), previamente definidas en scripts independientes, para
# integrarlas finalmente mediante un cálculo aditivo y generar la huella normalizada y sus versiones
# reclasificadas y reproyectadas (EPGS: 9377 y WGS 4326).
#
# Permite trabajar con insumos de cobertura de la tierra provenientes de Corine Land Cover Colombia o
# MapBiomas Colombia, lo cual se define en la sección de carga de variables. En esta misma sección se
# especifica el año de interés y el año de los datos de población, este último puede diferir del año de interés,
# ya que la fuente (GHS-POP R2023A - GHS population grid  multitemporal (1975–2030) (European Commission, 
# Joint Research Centre - JRC) genera datos con periodicidad de 5 años, y debe seleccionarse el año disponible más cercano al año de interés. 

# Nota: La capa de cobertura es la que define los limites exactos de la capa resultante de IHEH

# Detalles del cálculo: https://docs.google.com/document/d/1dvCtK18dAY-XiAYrFJlUD44stQDIA1E3/edit


#**********************************************************
# librerías o dependencias --------------------------------
#**********************************************************

library (sf)
library(terra)
library(tidyverse)
library(raster)

#**********************************************************
# Definir directorio(s) de trabajo -----------------------
#**********************************************************

setwd(file.path(this.path::this.path(), "..",".."))

dir_datos <- file.path("datos")
dir_Intermedios<- file.path ("Res_Intermedios")
dir_Resultados <- file.path("Resultados")
dir_source <- file.path("Codigos","pipelines")

#**********************************************************
# Cargar variables necesarias ----------------------------
#**********************************************************
## Año #### 
# Escriba el año de interes
Año <- 2018

# Escriba el año de los datos de población que va a usar
Año_pop <- 2020

# Capa base de cobertura (define insumos LU y TNT)
# Opciones: "corine" o "MB"

base_cobertura <- "corine" 


#**********************************************************
# Cargar los datos necesarios ----------------------------
#**********************************************************

# Raster base de referencia
# Raster plantilla: define resolución, extensión y CRS objetivo
r_base <- rast(file.path(dir_datos, "r_base.tif"))          # Resolución 100 m


# - - - Lectura de capas base previamente procesadas 
# Uso del suelo (LU)

LU <- rast(file.path(dir_Intermedios,paste0( "LU0_",base_cobertura, Año,".tif")))

# TNT
TNT <- rast(file.path(dir_Intermedios,paste0( "TNT0_",base_cobertura, Año,".tif")))

# Población
Pop0 <- rast(file.path(dir_Intermedios, paste0("pop_km2_", Año_pop, ".tif")))


# Vectores de infraestructura vial

vias8 <- file.path(dir_Intermedios, paste0 ("osm_IGAc8_proj_nal_ac", Año,".shp")) %>%
  st_read()
vias5 <- file.path(dir_Intermedios, paste0 ("osm_IGAc5_proj_nal_ac", Año,".shp")) %>%
  st_read()
vias4 <- file.path(dir_Intermedios, paste0 ("osm_IGAc4_proj_nal_ac", Año,".shp")) %>%
  st_read()
vias2 <- file.path(dir_Intermedios, paste0 ("osm_IGAc2_proj_nal_ac", Año,".shp")) %>%
  st_read()

# vias ferreas

V_ferreas4 <- rast(file.path(dir_Intermedios, paste0("pesos_trenes_", 4,"_proj_nal_", Año, ".tiff")))
V_ferreas6 <- rast(file.path(dir_Intermedios, paste0("pesos_trenes_", 6,"_proj_nal_", Año, ".tiff")))


#**********************************************************
# Rutas para guardar las capas intermedias ----------------
#**********************************************************

Lu_he_file <- paste0(dir_Resultados, "/LU_", base_cobertura, Año, ".tif")
Pd_he_file <- paste0(dir_Resultados, "/Pop", Año, ".tif")
if_he_file <- paste0(dir_Resultados, "/frag_", base_cobertura, Año, ".tif")
dr_he_file <- paste0(dir_Resultados, "/Viasac", Año, ".tif")

#**********************************************************
# Preparar datos ----------------------------
#**********************************************************

## lu ####
#**********************************************************

# Reescalar la cobertura de la tierra de 0 a 10 (original 0–5)

if (!all(file.exists(Lu_he_file))) {
  cat("Procesando y creando archivos lu_he...\n")
  
  source(file.path(dir_source,"lu_he.R"))
  
} else {
  Lu_he <- rast(Lu_he_file)
  
  cat("Los archivos de Lu_he ya existen. Se cargan las capas.\n")
}


plot(Lu_he)


## Pop Pd_he  ####
#**********************************************************

# Definición de los pesos basados en densidad poblacional (Venter 2016)


if (!all(file.exists(Pd_he_file))) {
  cat("Procesando y creando archivos Pd_he...\n")
  
  source(file.path(dir_source,"pd_he.R"))
  
} else {
  Pd_he <- rast(Pd_he_file)
  
  cat("Los archivos de Pd_he ya existen. Se cargan las capas.\n")
}


plot(Pd_he)


## vias- dr_he  ####
#**********************************************************
# Asignar los pesos a carreteras y vías férreas

source(file.path(dir_source,"dr_he.R"))




plot(Vias_4R$v8, main = "Influencia vías categoría 8")
plot(Vias_4R$v5, main = "Influencia vías categoría 5")
plot(Vias_2R$v4, main = "Influencia vías categoría 4")
plot(Vias_2R$v2, main = "Influencia vías categoría 2")
plot(V_ferreas4, main = "Vía férrea inactiva")
plot(V_ferreas6, main = "Vía férrea activa")
plot(dr_he, main = "Capa combinada de influencia vial (dr_he)")
hist(dr_he, main = "Histograma de influencia vial (dr_he)")

dr_he  # Resultado final


## if_he  ####
#**********************************************************
# Se calcula un indicador de fragmentación basado en densidad de píxeles naturales
# en un radio de 1 km, con pesos de huella por decaimiento exponencial 

if (!all(file.exists(if_he_file))) {
  cat("Procesando y creando archivos if_he...\n")
  
  source(file.path(dir_source,"if_he.R"))
  
} else {
  if_he <- rast(if_he_file)
  
  cat("Los archivos de if_he ya existen. Se cargan las capas.\n")
}


plot(if_he)


# Cálculo de Huella ####
#**********************************************************
## huella continua ####
# Integración aditiva de las cuatro variables explicatorias

IHEH <- Lu_he + Pd_he + if_he + dr_he
IHEH1002 <- 100 / 38 * IHEH  # Normalización a escala 0-100


plot(IHEH1002)

### Guardar resultado crs:9377####
writeRaster(
  IHEH1002,
  paste0(dir_Resultados, "/IHEH_IAVHac_",base_cobertura, Año, ".tif"), 
  overwrite=TRUE)

# Guardar capas intermedias

writeRaster(Lu_he, Lu_he_file, overwrite = TRUE)
writeRaster(Pd_he, Pd_he_file, overwrite = TRUE)
writeRaster(if_he, if_he_file, overwrite = TRUE)
writeRaster(dr_he, dr_he_file, overwrite = TRUE)


## Reclasificar a las categorías discretas ####

# Definir los breaks y las etiquetas
labels <- c("Natural", "Baja", "Media", "Alta", "Muy Alta")

# Reclasificar usando classify() + as.factor()
# Rangos de clasificación de intensidad de huella
rc_matrix <- matrix(c(-1, 0, 1,
                      0, 15, 2,
                      15, 30, 3,
                      30, 50, 4,
                      50,100, 5), 
                    ncol = 3, byrow = TRUE)

r_class <- classify(IHEH1002, rc_matrix)

# Convertir a factor y asignar etiquetas
levels(r_class) <- data.frame(ID = 1:5, clase = labels)

plot(r_class)

### Guardar reclass crs:9377####

writeRaster(
  r_class,
  paste0(dir_Resultados, "/IHEH_IAVH_class_",base_cobertura, Año, ".tif"), 
  overwrite=TRUE)

Sys.time()


## Proyectar datos WGS4326 ####

#IHEH1002 <- rast(paste0(dir_Resultados, "/IHEH_IAVH",base_cobertura, Año, ".tif")) # Activar de ser necesario

# # Creación del raster base (Solo es necesario correrlo una vez)
# IHEH1002_wgs <- project(IHEH1002,"EPSG:4326")
# r_base_wgs <- IHEH1002_wgs
# values( r_base_wgs) <- 0
# writeRaster(
#   r_base_wgs,
#   paste0(dir_datos, "/rbaseWgs.tif"), 
#   overwrite=TRUE)

# Reproyectar a WGS

r_base_wgs <- rast ( paste0(dir_datos, "/rbaseWgs.tif"))
IHEH1002_wgs <- project(IHEH1002,r_base_wgs)

# Reclasificar la huella en WGS
r_class_wgs <- classify(IHEH1002_wgs, rc_matrix)

plot(r_class_wgs)
# Convertir a factor y asignar etiquetas
levels(r_class_wgs) <- data.frame(ID = 1:5, clase = labels)

plot(r_class_wgs)

### Guardar  WGS 4326 ####

writeRaster(
  IHEH1002_wgs,
  paste0(dir_Resultados, "/IHEH_IAVHac_",base_cobertura,"_wgs", Año, ".tif"), 
  overwrite=TRUE)
writeRaster(
  r_class_wgs,
  paste0(dir_Resultados, "/IHEH_IAVH_classac_",base_cobertura,"_wgs", Año, ".tif"), 
  overwrite=TRUE)

