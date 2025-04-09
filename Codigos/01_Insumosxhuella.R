# Título: Preparación insumos por Huella 
#
# Autor(es): Alejandra Narváez Vallejo
#
# Descripción: En este código se preparan los insumos necesarios para correr el IHEH
# Estos son :
## - Población: Descarga (URl en código),  corte a zona de estudio , reproyección, cálculo de densidad. No es necesario correlo si se va a usar la poblacióin del IHEH anterior (la población se calcula cada 5 años). 
## - Vias: 2018: IGAc de Julian y descarga 2019 de osm por que tiene fecha enero 2019 
## - Vias: 2022: IGAc (https://www.colombiaenmapas.gov.co/?e=-82.43784778320864,-0.17644239911865092,-71.23179309571162,9.90326984502256,4686&b=igac&u=0&t=23&servicio=205) y descarga 2023 de osm por que tiene fecha enero 2023, Para el cálculo de años posteriores seguir esquema del 2022

 
# Por hacer o  corregir: 

## - Si es menor al 2018, aun se debe ver que hacer en vias


#**********************************************************
# librerías o dependencias --------------------------------
#**********************************************************

# lectura de datos 

library (sf) 
library(terra)
library(dplyr)

#**********************************************************
# Definir directorio(s) de trabajo -----------------------
#**********************************************************

setwd(file.path(this.path::this.path(),"..",".."))

dir_datos<- file.path("Datos")
dir_Intermedios<- file.path ("Res_Intermedios")
dir_Resultados<- file.path ("Resultados")

#**********************************************************
# Cargar los datos necesarios ----------------------------
#**********************************************************

# Capas Vector

#osm<-st_read(file.path(dir_datos,"vias", "colombia-190101-free.shp","gis_osm_roads_free_1.shp"))#2018
osm0<-st_read(file.path(dir_datos,"vias", "colombia-230101-free.shp","gis_osm_roads_free_1.shp"))#2022
#vias_IGAC <- st_read(file.path(dir_datos,"vias","ViasJulian2018","vias.shp"))# 2018
vias_IGAC0 <- st_read(file.path(dir_datos,"vias","IGAC_viasD2024","Vias_IGAC.shp"))# 2022

# Capas Raster
r_base<-rast(file.path(dir_datos,"r_base.tif" )) 


#**********************************************************
# Parametros globales ----------------------------
#**********************************************************
 
Año <- 2022 # definir el año que se quiere calcular
año_pop <- 2020 # escribir el año de los datos de población a usar- 

# clases de OSM Que se tendrán en cuenta para los cálculos de Huella humana.

# Opción 1- Clasificación usada por Julián Díaz
osm_class <- c( "trunk",  "tertiary", "secondary", "primary_link", "secondary_link", "primary",   "trunk_link",  "tertiary_link")

# Opción 2. Clasificación de opción continua y diferencial para tipos de vías
# Opción 2 - 8/4
osm_class8 <- c( "trunk",  "tertiary", "secondary", "primary_link", "secondary_link", "primary",   "trunk_link",  "tertiary_link", "living_street", "residential")

# opción 2 - 5/4
osm_class5 <- c( "track",  "track_grade1", "track_grade2","track_grade3","track_grade4", "track_grade5", "service", "bridleway")

# opción 2 - 4/4
osm_class4 <- c( "pedestrian","footway","steps","unknown","unclassified")

# opción 2 - 2/2
osm_class2 <- c("path")


#**********************************************************
# Preparar datos ----------------------------
#**********************************************************

##  Vías Principales ----------------------------------------------

# Filtrar las geometrías de OSM seleccionando solo aquellas con clases relevantes
osm <- osm0[osm0$fclass %in% osm_class, ] %>%
  st_geometry()  # Extraer solo la geometría

# Crear un objeto sf con un ID genérico para la capa OSM
osm <- st_sf(data.frame(ID = 1, geom = osm))

# Filtrar y extraer geometría de las vías del IGAC según el año
if (Año <= 2018) {
  # Para años hasta 2018, usar el atributo GP_RTP para filtrar las vías principales (categorías 1 a 3)
  vias_IGAC <- vias_IGAC0[vias_IGAC0$GP_RTP %in% c(1:3), ] %>%
    st_geometry()
} else {
  # Para años posteriores, usar el atributo TIPO_VIA para seleccionar vías relevantes (categorías 1 a 4)
  vias_IGAC <- vias_IGAC0[vias_IGAC0$TIPO_VIA %in% c(1:4), ] %>%
    st_geometry()
}

# Crear un objeto sf con un ID genérico para las vías del IGAC
vias_IGAC <- st_sf(data.frame(ID = 1, geom = vias_IGAC))

# Reproyectar las vías del IGAC al mismo sistema de coordenadas que las geometrías OSM
vias_IGAC_p <- st_transform(vias_IGAC, crs = st_crs(osm))

# Unir las capas OSM y vías del IGAC en un solo objeto espacial
osm_igac <- rbind(osm, vias_IGAC_p)


# revisar estructura de la capa
str(osm_igac)

# Guardar la capa en resultados intermedios
st_write(osm_igac, file.path(dir_Intermedios, paste0("osm_IGAc_", Año, ".shp")))



##  Vías 2.Método ----------------------------------------------

# Asignar un valor de "peso" a cada clase (fclass) de OSM según su categoría de importancia
osm0 <- osm0 %>%
  mutate(peso = case_when(
    fclass %in% osm_class8 ~ 8,  # Vías principales
    fclass %in% osm_class5 ~ 5,  # Vías pequeñas
    fclass %in% osm_class4 ~ 4,  # Caminos rurales o terciarios
    fclass %in% osm_class2 ~ 2   # Caminos menores u otros
  ))

# Crear tabla auxiliar con combinaciones únicas de clases (fclass) y sus pesos asignados
# Esto es útil como control de calidad o para futuras referencias
h <- unique(st_drop_geometry(osm0[c("fclass", "peso")]))

# Separar las geometrías del OSM por grupo de peso para calcular distancias de forma separada
osm_groups <- split(osm0, osm0$peso)

# Convertir cada grupo en un objeto 'sf' sólo con geometría, y agregar un ID genérico
osm_groups <- lapply(osm_groups, st_geometry)
osm_groups <- lapply(osm_groups, function(x) { st_sf(data.frame(ID = 1, geom = x)) })

# Asignar pesos a las vías del IGAC según su tipo de vía (TIPO_VIA)
vias_IGAC2 <- vias_IGAC0 %>%
  mutate(peso = case_when(
    TIPO_VIA %in% c(1:4) ~ 8,  # Vías principales
    TIPO_VIA %in% c(5:7) ~ 5,  # Vías secundarias
    TIPO_VIA %in% 8 ~ 2        # Caminos o vías terciarias
  ))

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
st_write(osm_igac8, file.path(dir_Intermedios, paste0("osm_IGAc8_", Año, ".shp")))
st_write(osm_igac5, file.path(dir_Intermedios, paste0("osm_IGAc5_", Año, ".shp")))
st_write(osm_igac4, file.path(dir_Intermedios, paste0("osm_IGAc4_", Año, ".shp")))
st_write(osm_igac2, file.path(dir_Intermedios, paste0("osm_IGAc2_", Año, ".shp")))

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

ext_projected<-ext(r_base) %>%
  project(from= crs(r_base) , to= crs(pop00))

# cortar
pop00c<- crop(pop00, ext_projected)

# proyectar a otra extensión 

pop00cp<- project(pop00c, r_base, method= "bilinear")

## cálculo a km2 ####

pop_km2<-pop00cp*100

writeRaster(pop_km2, file.path(dir_Intermedios, paste0("pop_km2_",año_pop,".tif")), overwrite=T)
