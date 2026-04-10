# Título: Preparación insumos por Huella
#
# Autor(es): Alejandra Narváez Vallejo
#
# Descripción: En este código se preparan los insumos necesarios para correr el IHEH
# Estos son :
## - Vias: 2018: IGAc de Julian y descarga 2019 de osm por que tiene fecha enero 2019
## - Vias: 2020: IGAc (https://www.colombiaenmapas.gov.co/?e=-82.43784778320864,-0.17644239911865092,-71.23179309571162,9.90326984502256,4686&b=igac&u=0&t=23&servicio=205) y descarga 2021 de osm por que tiene fecha enero 2021, Para el cálculo de años posteriores seguir esquema del 2020
## - Vias: 2022: IGAc (https://www.colombiaenmapas.gov.co/?e=-82.43784778320864,-0.17644239911865092,-71.23179309571162,9.90326984502256,4686&b=igac&u=0&t=23&servicio=205) y descarga 2023 de osm por que tiene fecha enero 2023, Para el cálculo de años posteriores seguir esquema del 2020

## - Población: Descarga (URl en código),  corte a zona de estudio , reproyección, cálculo de densidad. No es necesario correrlo si se va a usar la poblacióin del IHEH anterior (la población se calcula cada 5 años. Ej 2015, 2020, etc).

## LU y TNT: Se reclasifican Las clases Corine land Cover Colombia a :
#---# Pesos relacionados con las presiones Antrópicas según Correa 2020 y Etter 2011.
#---# Raster binario Natural y transformado que se usará en el análisis de fragmentación en el cálculo de la huella humana.

# Consideraciones:

## - Aunque el código tiene todas las partes bases Puede requerir Adaptación en varios puntos:
## - Definición de las rutas Y los argumentos necesarios para poder cargar las capas
## - Puede requerir algún tipo de Corrección geométrica para poder hacer rasterizaciones, Particularmente en la sección Lu y TNT

## La sección de vías y vías ferreas, es mejoer ejecutarlas una despues de la otra y no correr el código completo de procesamiento desde el comienzo. 

# Por hacer o  corregir:

## - Si es menor al 2016, aun se debe ver que hacer en vias
## - intentar automatizar la descargar de vias osm y organizar el nombre


#**********************************************************
# librerías o dependencias --------------------------------
#**********************************************************

# lectura de datos

library (sf)
library(terra)
library(dplyr)
library(rlang)

#**********************************************************
# Definir directorio(s) de trabajo -----------------------
#**********************************************************

setwd(file.path(this.path::this.path(),"..", ".."))

dir_datos <- file.path("Datos")
dir_Intermedios <- file.path ("Res_Intermedios")
dir_Resultados <- file.path ("Resultados")
dir_source <- file.path("Codigos","pipelines")


#**********************************************************
# Parametros globales ----------------------------
#**********************************************************

## Definir Periódo al calcular y actualidad los datos a usar
Año <- 2016 # definir el año que se quiere calcular
año_pop <- 2015 # escribir el año de los datos de población a usar-

scoord <- crs("EPSG:9377") # Sistema de coordenadas del raster base. Cambiar cuando se defina la proyección


# Capa base de cobertura (define insumos LU y TNT)
# Opciones: "corine" o "MB"

base_cobertura <- "MB"  


#**********************************************************
# Cargar los datos necesarios ----------------------------
#**********************************************************

## Capas Raster ####
# Necesarias para definir la extension espacial, proyeccion y sistema de coordenadas
r_base <- rast(file.path(dir_datos, "r_base.tif"))

# Defina las rutas de la cobertura de la tierra con los datos de su interés

## Datos de cobertura de la tierra ####
#### En caso de ser CORINE ####
# --------------------------- -
  
# En esta sección por favor ajustar nombre de la ruta del archivo Y el nombre de la capa en caso de que sea una GDB.

# Ruta a la GDB
gdb_path <- file.path(
  dir_datos,
  #"Corine/Cobertura_de_la_Tierra_100K_Periodo_2018/COBERTURAS CORINE 2018/ECOSISTEMAS_062021.gdb"# 2018
  "/Corine/Cobertura_de_la_tierra_100K_Periodo_2020_limite_administrativo (1)/Cobertura_de_la_tierra_100K_Periodo_2020_limite_administrativo/ECOSISTEMAS_14072024.gdb"
)

# Revisar qué capas hay dentro de la GDB
st_layers(gdb_path)

# Elegir La capa ambiental
corine <- st_read(gdb_path, layer = "e_cobertura_tierra_2020_amb")

# revisar nombres de los campos y definir columna para la clasificacion a pesos de huella, dicha columna dece ser la que tenga los códigos de las clases de cobertura. Estos son codigos númericos como estos:  323, 3132, 311121, 311121. Como puede darse cuenta estos pueden varian en longitud.

names(corine)
head(corine)
# Definir la columna qeu
Cod_ecos <- "codigo"

# En caso de ser Mapbiomas 
# --------------------------- -
# No defina ninguna ruta, la descarga se hará automaticamente en las siguiente sección "Preprocesamiento y asignacion de pesos"


## Datos de las vias ####

osm0 <- st_read(
  file.path(
    dir_datos,
    "vias",
    #"colombia-210101-free.shp",
    "colombia-170101-free.shp",
    #"210101.shp"
    "gis_osm_roads_free_1.shp"
  )
)

vias_IGAC0 <- st_read(file.path(dir_datos,"vias","ViasJulian2018","vias.shp"))# 2018
#vias_IGAC0 <- st_read(file.path(dir_datos, "vias", "IGAC_viasD2024", "Vias_IGAC.shp"))# 2022 y 2020

# Capa de red férrea oficial (fuente: IGAC / ANI) 
# ajustar el nombre de ser necesario 
Vias <- st_read(file.path(dir_datos, "vias", "ferreas", "RedFerrea_actuali.shp"))

#**********************************************************
# Preparar datos ----------------------------
#**********************************************************
##  Vías ----------------------------------------------

# Se crean variables con las rutas
OI_file8 <- file.path(dir_Intermedios, paste0("osm_IGAc8_proj_nal_", Año, ".shp"))
OI_file5 <- file.path(dir_Intermedios, paste0("osm_IGAc5_proj_nal_", Año, ".shp"))
OI_file4 <- file.path(dir_Intermedios, paste0("osm_IGAc4_proj_nal_", Año, ".shp"))
OI_file2 <- file.path(dir_Intermedios, paste0("osm_IGAc2_proj_nal_", Año, ".shp"))


# Ejecutar proceso solo si NO existen los archivos
archivos_osm <- c(OI_file8, OI_file5, OI_file4, OI_file2)


if (!all(file.exists(archivos_osm))) {
  cat("Procesando y creando archivos infraesructura vial...\n")
  
  source(file.path(dir_source, "osm_cat_vector.R"))
  
} else {
  cat("Los archivos de infraesructura vial ya existen. Se omite el proceso.\n")
}


##  Vías ferreas ----------------------------------------------

# Se crean variables con las rutas
ferreo_path6 <-   file.path(dir_Intermedios, paste0("pesos_trenes_6_proj_nal_", Año, ".tiff"))
ferreo_path4 <-   file.path(dir_Intermedios, paste0("pesos_trenes_4_proj_nal_", Año, ".tiff"))
list_ferreo <- c(ferreo_path6, ferreo_path4)

# Ejecutar proceso solo si NO existen los archivos
if (!all(file.exists(list_ferreo))) {
  cat("Procesando y creando archivos de vias ferreas...\n")
  
  source(file.path(dir_source, "vias_ferreas.R"))
  
} else {
  cat("Los archivos de vias ferreas ya existen. Se omite el proceso.\n")
}


##  Población  --------------------------------------------------

# Se crean variables con las rutas
popkm2_path <- file.path(dir_Intermedios, paste0("pop_km2_", año_pop, ".tif"))

# Ejecutar proceso solo si NO existen los archivos
if (!file.exists(popkm2_path)) {
  cat("Procesando y creando archivos OSM...\n")
  
  source(file.path(dir_source, "Descarga_y_mergePop.R"))
  
} else {
  cat("Los archivos pop_km2 ya existen. Se omite el proceso.\n")
}

pop_km2 <- rast(popkm2_path)
plot(pop_km2)


##  LU y TNT --------------------------------------------------

# Se crean variables con las rutas
archivo_LU0 <- file.path(dir_Intermedios, paste0("LU0_", base_cobertura, Año, ".tif"))
archivo_TNT0 <- file.path(dir_Intermedios, paste0("TNT0_", base_cobertura, Año, ".tif"))

# Ejecutar proceso solo si NO existen los archivos
if (!file.exists(archivo_LU0) &
    !file.exists(archivo_TNT0)) {
  cat("Procesando y creando archivos de LU y TNT...\n")
  if (base_cobertura == "MB") {
    source(file.path(dir_source, "creacion_LU0_y_TNT_mb.R"))
  } else if (base_cobertura == "corine") {
    source(file.path(dir_source, "creacion_LU0_y_TNT_corine.R"))
    
  } else{
    cat (" Definición de la capa de cobertura incorrecta")
  }
} else {
  cat("Los archivos LU y TNT ya existen. Se omite el
proceso.\n")
}


plot(LU0)
plot(TNT0)

##  Navegabilidad --------------------------------------------------

### Preprocesamiento y asignacion de pesos ####

archivo_nav <- file.path(dir_Intermedios, paste0("pesos_navegabilidad_proj_nal",base_cobertura ,Año,".tiff"))

# Condición para crear o no los archivos

if (!file.exists(archivo_nav)) {
  cat("Procesando y creando archivos navegabilidad...\n")
    source(file.path(dir_source, "Navegabilidad.R"))
  } else {
  cat("Los archivos de navegabilidad ya existen. Se omite el
proceso.\n")
}

plot(pesos_rnav)

