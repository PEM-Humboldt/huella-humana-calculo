# Título: Preparación insumos por Huella 
#
# Autor(es): Alejandra Narváez Vallejo
#
# Descripción: En este código se preparan los insumos necesarios para correr el IHEH
# Estos son :
## - Vias: 2018: IGAc de Julian y descarga 2019 de osm por que tiene fecha enero 2019 
## - Vias: 2020: IGAc (https://www.colombiaenmapas.gov.co/?e=-82.43784778320864,-0.17644239911865092,-71.23179309571162,9.90326984502256,4686&b=igac&u=0&t=23&servicio=205) y descarga 2021 de osm por que tiene fecha enero 2021, Para el cálculo de años posteriores seguir esquema del 2020
## - Vias: 2022: IGAc (https://www.colombiaenmapas.gov.co/?e=-82.43784778320864,-0.17644239911865092,-71.23179309571162,9.90326984502256,4686&b=igac&u=0&t=23&servicio=205) y descarga 2023 de osm por que tiene fecha enero 2023, Para el cálculo de años posteriores seguir esquema del 2020

## - Población: Descarga (URl en código),  corte a zona de estudio , reproyección, cálculo de densidad. No es necesario correrlo si se va a usar la poblacióin del IHEH anterior (la población se calcula cada 5 años. Ej 2015, 020, etc). 

## LU y TNT: Se reclasifican Las clases  de Mapbiomas a : 
#---# Pesos relacionados con las presiones Antrópicas según Correa 2020 y Etter 2011. 
#---# Raster binario Natural y transformado Que se usará en el análisis de fragmentación en el cálculo de la huella humana. 

# Consideraciones: 

## - Aunque el código tiene todas las partes bases Puede requerir Adaptación en varios puntos:
## - Definición de las rutas Y los argumentos necesarios para poder cargar las capas
## - Puede requerir algún tipo de Corrección geométrica para poder hacer rasterizaciones, Particularmente en la sección Lu y TNT

# Por hacer o  corregir: 

## - Si es menor al 2018, aun se debe ver que hacer en vias
## - Mapiomas se puede descargar directamente en Código

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

setwd(file.path(this.path::this.path(),"..","..",".."))

dir_datos<- file.path("Datos")
dir_Intermedios<- file.path ("Res_Intermedios")
dir_Resultados<- file.path ("Resultados")


#**********************************************************
# Parametros globales ----------------------------
#**********************************************************

## Definir Período al calcular y actualidad los datos a usar
Año <- 2018 # definir el año que se quiere calcular
año_pop <- 2020 # escribir el año de los datos de población a usar- 


resolucion <-  100   # Resolución objetivo para el análisis
scoord <- crs("EPSG:9377") # Sistema de coordenadas del raster base. Cambiar cuando se defina la proyección


## Parámetros de vias OSM
# Parametros para la asignación de pesos de la distancia a vias. Los pesos tienen una escala continua y es diferencial para tipos de vías
# La fracción que verá en los nombres a continuación significa el rango de valores que va a tener este tipo de vías. Ejemplo 8/4 Quiere decir que este tipo de vías tendrán valores entre 4 y 8.

# Pesos - 8/4. Vías vehiculares principales y secundarias
osm_class8 <- c( "trunk",  "tertiary", "secondary", "primary_link", "secondary_link", "primary",   "trunk_link",  "tertiary_link", "living_street", "residential",  "motorway_link", "motorway")

# Pesos - 5/4. Vías terciarias y rurales
osm_class5 <- c( "track",  "track_grade1", "track_grade2","track_grade3","track_grade4", "track_grade5", "service", "bridleway", "cycleway")

# Pesos - 4/2. Infraestructura peatonal y no clasificada
osm_class4 <- c( "pedestrian","footway","steps","unknown","unclassified")

# Pesos - 2/2. Senderos naturales
osm_class2 <- c("path")

#**********************************************************
# Cargar los datos necesarios ----------------------------
#**********************************************************
# Aqui debe Modificar el nombre de los insumos Para el año que quiera correr 

# Capas Vector

#osm0<-st_read(file.path(dir_datos,"vias", "colombia-190101-free.shp","gis_osm_roads_free_1.shp"))#2018
#osm0<-st_read(file.path(dir_datos,"vias", "colombia-210101-free.shp","210101.shp"))#2020
osm0<-st_read(file.path(dir_datos,"vias", "colombia-230101-free.shp","gis_osm_roads_free_1.shp"))#2022

#vias_IGAC0 <- st_read(file.path(dir_datos,"vias","ViasJulian2018","vias.shp"))# 2018
vias_IGAC0 <- st_read(file.path(dir_datos,"vias","IGAC_viasD2024","Vias_IGAC.shp"))# 2022 y 2020

# Capas Raster
r_base<-rast(file.path(dir_datos,"r_base.tif" ))
r_base10<-rast(file.path(dir_datos,"r_base10.tif" ))

#**********************************************************
# Preparar datos ----------------------------
#**********************************************************


##  Vías ----------------------------------------------

# Asignar un atributo de "peso" a cada clase (fclass) de OSM según su categoría de importancia
osm0 <- osm0 %>%
  mutate(peso = case_when(
    fclass %in% osm_class8 ~ 8,  
    fclass %in% osm_class5 ~ 5,  
    fclass %in% osm_class4 ~ 4,  
    fclass %in% osm_class2 ~ 2   
  ))

# Crear tabla auxiliar con combinaciones únicas de clases (fclass) y sus pesos asignados
# Esto es útil como control de calidad o para futuras referencias
h <- unique(st_drop_geometry(osm0[c("fclass", "peso")]))
h

# Separar las geometrías del OSM por grupo de peso para calcular distancias de forma separada
osm_groups <- split(osm0, osm0$peso)

# Convertir cada grupo en un objeto 'sf' sólo con geometría, y agregar un ID genérico
osm_groups <- lapply(osm_groups, st_geometry)
osm_groups <- lapply(osm_groups, function(x) { st_sf(data.frame(ID = 1, geom = x)) })

# Asignar pesos a las vías del IGAC según su tipo de vía (TIPO_VIA) si es 2022 o GP_RTP  si es 2018

if (Año == 2022 | Año == 2020 ) {
  vias_IGAC2 <- vias_IGAC0 %>%
    mutate(peso = case_when(
      TIPO_VIA %in% c(1:4) ~ 8,  # Vías principales
      TIPO_VIA %in% c(5:7) ~ 5,  # Vías secundarias
      TIPO_VIA %in% 8 ~ 2        # Caminos o vías terciarias
    ))
  
  
} else if (Año == 2018) {
  
  vias_IGAC2 <- vias_IGAC0 %>%
    mutate(peso = case_when(
      GP_RTP %in% c(1:3) ~ 8,  # Vías principales
      GP_RTP %in% c(4) ~ 5,  # Vías secundarias
      GP_RTP %in% 8 ~ 2        # Caminos o vías terciarias
    ))
  
  
  
}


# Reproyectar las vías del IGAC al sistema de coordenadas de OSM para que coincidan espacialmente
vias_IGAC_p2 <- st_transform(vias_IGAC2, crs = st_crs(osm0))

# Separar las geometrías del IGAC por grupo de peso
IGAC_groups <- split(vias_IGAC_p2, vias_IGAC_p2$peso)

# Convertir cada grupo en objeto 'sf' solo con geometría, agregando un ID genérico
IGAC_groups <- lapply(IGAC_groups, st_geometry)
IGAC_groups <- lapply(IGAC_groups, function(x) { st_sf(data.frame(ID = 1, geom = x)) })

# Unir las capas OSM e IGAC por cada categoría de peso, para posterior análisis (por ejemplo, cálculo de distancias)
osm_igac8 <- rbind(IGAC_groups$`8`, osm_groups$`8`)
osm_igac5 <- rbind(IGAC_groups$`5`, osm_groups$`5`)
osm_igac2 <- rbind(IGAC_groups$`2`, osm_groups$`2`)
osm_igac4 <- rbind(IGAC_groups$`4`, osm_groups$`4`)  # En este caso solo OSM tiene categoría 4


# revisar estructura de la capa
str(osm_igac8)

# Guardar la capa en resultados intermedios
st_write(osm_igac8, file.path(dir_Intermedios, paste0("osm_IGAc8_", Año, ".shp")), append = FALSE)
st_write(osm_igac5, file.path(dir_Intermedios, paste0("osm_IGAc5_", Año, ".shp")), append = FALSE)
st_write(osm_igac4, file.path(dir_Intermedios, paste0("osm_IGAc4_", Año, ".shp")), append = FALSE)
st_write(osm_igac2, file.path(dir_Intermedios, paste0("osm_IGAc2_", Año, ".shp")), append = FALSE)

##  Población  --------------------------------------------------

### descargar datos  ####
# Definir la URL del archivo
#url <- "https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/GHSL/GHS_POP_GLOBE_R2023A/GHS_POP_E2020_GLOBE_R2023A_54009_100/V1-0/GHS_POP_E2020_GLOBE_R2023A_54009_100_V1_0.zip"
url2 <- "https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/GHSL/GHS_POP_GLOBE_R2023A/GHS_POP_E2020_GLOBE_R2023A_54009_100/V1-0/tiles/GHS_POP_E2020_GLOBE_R2023A_54009_100_V1_0_R8_C11.zip"
url3 <- "https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/GHSL/GHS_POP_GLOBE_R2023A/GHS_POP_E2020_GLOBE_R2023A_54009_100/V1-0/tiles/GHS_POP_E2020_GLOBE_R2023A_54009_100_V1_0_R9_C11.zip"
url4 <- "https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/GHSL/GHS_POP_GLOBE_R2023A/GHS_POP_E2020_GLOBE_R2023A_54009_100/V1-0/tiles/GHS_POP_E2020_GLOBE_R2023A_54009_100_V1_0_R10_C11.zip"
url5 <- "https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/GHSL/GHS_POP_GLOBE_R2023A/GHS_POP_E2020_GLOBE_R2023A_54009_100/V1-0/tiles/GHS_POP_E2020_GLOBE_R2023A_54009_100_V1_0_R8_C12.zip"
url6 <- "https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/GHSL/GHS_POP_GLOBE_R2023A/GHS_POP_E2020_GLOBE_R2023A_54009_100/V1-0/tiles/GHS_POP_E2020_GLOBE_R2023A_54009_100_V1_0_R9_C12.zip"
url1 <- "https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/GHSL/GHS_POP_GLOBE_R2023A/GHS_POP_E2020_GLOBE_R2023A_54009_100/V1-0/tiles/GHS_POP_E2020_GLOBE_R2023A_54009_100_V1_0_R10_C12.zip"

# URLs en una lista
URLMos <- list(url1, url2, url3, url4, url5, url6)

# Definir el nombre del archivo descargado
destfile <- "GHS_POP_2023A.zip"

PopDescarga <- function(url){
  # Descargar el archivo zip
  download.file(url, file.path(dir_datos, "Pop", destfile), mode = "wb")
  
  # Descomprimir el archivo
  unzip(file.path(dir_datos, "Pop", destfile), exdir = file.path(dir_datos, "Pop"))
}

#descargar todos los URL
lapply(URLMos, PopDescarga)

# Definir la ruta los archivo TIFF descomprimido
tiff_file <- list.files(file.path(dir_datos, "Pop"), pattern = "\\.tif$", full.names = TRUE)

# Leer capas y hacer el mosaico

Tiles <- lapply(tiff_file, rast)
pop00 <- merge(sprc(Tiles))

gc()


### reproyectar ####

# cortar pop ##
# preparar la extension para cortar y proyectarla

ext_projected <- ext(r_base) %>%
  project(from= crs(r_base) , to= crs(pop00))

# cortar
pop00c <- crop(pop00, ext_projected)

# proyectar a otra extensión 

pop00cp <- project(pop00c, r_base, method= "bilinear")

### cálculo a km2 ####

pop_km2 <- pop00cp*100

writeRaster(pop_km2, file.path(dir_Intermedios, paste0("pop_km2_",año_pop,".tif")), overwrite=T)

##  LU y TNT --------------------------------------------------
# 1.Creación de atributos para definir los pesos acorde a la cobertura Y Para Definir transformado y no transformado
# 2. Rasterización  Para la creación de las capas LU (Pesos de huella de acuerdo con la cobertura) Y tnt(Transformado - no transformado)

### Cargar los datos ####
#Crear URL
urlmb <- sprintf(
  "https://storage.googleapis.com/mapbiomas-public/initiatives/colombia/collection_3/coverage/colombia_coverage_%d.tif",
  Año
)

# Nombre del archivo final
destfile <- sprintf("mapbiomas_colombia_collection3_%d.tif", Año)

# Ruta completa de salida
ruta_salida <- file.path(dir_datos, "Mapbiomas", destfile)

# Crear carpeta si no existe
dir.create(file.path(dir_datos, "Mapbiomas"), showWarnings = TRUE, recursive = TRUE)

# Descargar solo si el archivo no existe
if (!file.exists(ruta_salida)) {
  message("Descargando archivo MapBiomas ", Año, "...")
  download.file(urlmb, ruta_salida, mode = "wb")
} else {
  message("El archivo ya existe, no se descarga: ", destfile)
}

# Cargar raster
mapbiomas <- rast(ruta_salida)

# Si el raster no existe, reproyectar y guardar

archivo_LU0 <- file.path(dir_Intermedios,paste0( "LU0_MB",Año,".tif"))


# Condición para crear o no los archivos
if (!file.exists(archivo_LU0) ) {
  
  LU0 <- mapbiomas %>%
    project(r_base10, method = "near") %>%
    aggregate(fact = 10, fun = "modal")
  
    # Guardar los archivos
  writeRaster(LU0, archivo_LU0, datatype = "INT1U", overwrite = TRUE)
  
  
} else {
  # Leer desde disco
  LU0 <- rast(archivo_LU0)
  
}


# Reclasificar a pesos/ presiones humanas
# Crear una matriz de reclasificación 

reclass_mat <- matrix(c(
  3,  0,# natural
  5,  0,# natural
  6,  0,# natural
  49, 0,# natural
  11, 0,# natural
  12, 0,# natural
  32, 0,# natural
  33, 0,# natural
  34, 0,# natural
  27, 0, # es no observado , pero se encentra en lazona de los glaciares de SNSM
  29, 0,# natural
  50, 0,# natural
  13, 0,# natural
  81, 0,# natural
  82, 0,# natural
  23, 0,# natural
  68, 0,# natural
  35, 4,# agro palma
  21, 3,# agro y pasture
  74, 4,# agro banano
  25, 1,# No vegetal, # desnudo quema o degrdado
  75, 4.5,# Parque solar
  9,  2,# plantacion forestal
  31, 2,# acuicultura 
  24, 5,# Infraestructura
  30, 5 # Minería
), ncol = 2, byrow = TRUE)

# Relasificar y guardar con nombres que incluyan el año

LU <- classify(LU0, rcl = reclass_mat)

plot(LU)
  
writeRaster(LU,
            file.path(dir_Intermedios, paste0("mapbiomas_pesos_",Año,".tif")), 
            datatype = "INT1U",
            overwrite=T)

# Reclasificar a Transformado - no Transformado
# Crear una matriz de reclasificación 

naturales <- c(3,5,6,49,11,12,32,33,34,27,29,50,13,81,82,23,68)

transformados <- c(35,21,74,25,75,9,31,24,30)


reclass01 <- rbind(
  cbind(naturales, 0),
  cbind(transformados, 1)
)

print(reclass01)

# Relasificar y guardar con nombres que incluyan el año

TNT <- classify(LU0, rcl = reclass01)
plot(TNT)


writeRaster(TNT,
            file.path(dir_Intermedios, paste0("mapbiomas_TNT_",Año,".tif")), 
            datatype = "INT1U",
            overwrite=T)
