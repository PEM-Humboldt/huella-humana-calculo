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

## LU y TNT: Se reclasifican Las clases Corine land Cover Colombia a : 
#---# Pesos relacionados con las presiones Antrópicas según Correa 2020 y Etter 2011. 
#---# Raster binario Natural y transformado Que se usará en el análisis de fragmentación en el cálculo de la huella humana. 

# Consideraciones: 

## - Aunque el código tiene todas las partes bases Puede requerir Adaptación en varios puntos:
## - Definición de las rutas Y los argumentos necesarios para poder cargar las capas
## - Puede requerir algún tipo de Corrección geométrica para poder hacer rasterizaciones, Particularmente en la sección Lu y TNT

# Por hacer o  corregir: 

## - Si es menor al 2018, aun se debe ver que hacer en vias


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

setwd(file.path(this.path::this.path(),"..","..","..",".."))

dir_datos<- file.path("Datos")
dir_Intermedios<- file.path ("Res_Intermedios")
dir_Resultados<- file.path ("Resultados")





#**********************************************************
# Parametros globales ----------------------------
#**********************************************************

## Definir Período al calcular y actualidad los datos a usar
Año <- 2026 # definir el año que se quiere calcular
año_pop <- 2025 # escribir el año de los datos de población a usar- 


resolucion <-  10   # Resolución objetivo para el análisis
scoord <- crs("EPSG:9377") # Sistema de coordenadas del raster base. Cambiar cuando se defina la proyección


## Parámetros de vias OSM
# Parametros para la asignación de pesos de la distancia a vias. Los pesos tienen una escala continua y es diferencial para tipos de vías
# La fracción que verá en los nombres a continuación significa el rango de valores que va a tener este tipo de vías. Ejemplo 8/4 Quiere decir que este tipo de vías tendrán valores entre 4 y 8.

# Pesos - 8/4. Vías vehiculares principales y secundarias
osm_class8 <- c( "trunk",  "tertiary", "secondary", "primary_link", "secondary_link", "primary",   "trunk_link",  "tertiary_link", "living_street", "residential",  "motorway_link", "motorway", "busway")

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

#osm0<-st_read(file.path(dir_datos,"vias", "colombia-240101-free.shp","gis_osm_roads_free_1.shp"))#2024 comienzos
#osm0<-st_read(file.path(dir_datos,"vias", "colombia-250101-free.shp","gis_osm_roads_free_1.shp"))#2025 comienzos
osm0<-st_read(file.path(dir_datos,"vias", "colombia-260101-free.shp","gis_osm_roads_free_1.shp"))#2026 comienzos


# Capas Raster

r_base<-rast(file.path(dir_datos,"r_base.tif" ))

r_base10<-rast(file.path(dir_datos,"r_base10.tif" ))


# nucleos
region <- st_read("Datos/amazonas/NDFyB_V5-Amazonia_proHuella.shp") %>% st_transform(scoord) #nucleos viejos
#region <- st_read("Datos/amazonas/Gobernanza_Forestal_-3293470095607577430/Núcleos_de_Desarrollo_Forestal_y_de_la_Biodiversidad.shp") %>% st_transform(scoord)

#**********************************************************
# Preparar datos ----------------------------
#**********************************************************
## region ####

# region para vias
region_buf <- st_buffer(region, 15000) %>% st_union() 

plot(region_buf)
##  Vías ----------------------------------------------

# Asignar un atributo de "peso" a cada clase (fclass) de OSM según su categoría de importancia


#osm0<-osm0 %>%st_transform(scoord) %>% st_intersection(region_buf)

osm0<-osm0 %>%st_transform(scoord)

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

vias_IGAC0 <- st_read(file.path(dir_datos,"vias","ViasSinchiOct25","ViasSinchiOct25","Vías Terrestres NDFyB 25K Octubre 2025.shp"))# 2022 y 2020
  vias_IGAC2 <- vias_IGAC0 %>%
    mutate(peso = case_when(
      v_tipo %in% c(1:3) ~ 8,  # Vías principales
      v_tipo %in% c(4,5,6) ~ 5,  # Vías secundarias
      v_tipo %in% c(7,8) ~ 2        # caminos senderos varios
  
)
)



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


A <- lapply(list(osm_igac8,osm_igac5,osm_igac4,osm_igac2),  function(x){st_intersection(x,region_buf)})
 plot(A[[1]])


# Guardar la capa en resultados intermedios Amazonas
st_write(A[[1]], file.path(dir_Intermedios, paste0("osm_IGAc8AS_", Año, "enero.shp")), append = FALSE)
st_write(A[[2]], file.path(dir_Intermedios, paste0("osm_IGAc5AS_", Año, "enero.shp")), append = FALSE)
st_write(A[[3]], file.path(dir_Intermedios, paste0("osm_IGAc4AS_", Año, "enero.shp")), append = FALSE)
st_write(A[[4]], file.path(dir_Intermedios, paste0("osm_IGAc2AS_", Año, "enero.shp")), append = FALSE)


##  Población  --------------------------------------------------

### descargar datos  ####
# Definir la URL del archivo
#url <- "https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/GHSL/GHS_POP_GLOBE_R2023A/GHS_POP_E2020_GLOBE_R2023A_54009_100/V1-0/GHS_POP_E2020_GLOBE_R2023A_54009_100_V1_0.zip"
url2 <- "https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/GHSL/GHS_POP_GLOBE_R2023A/GHS_POP_E2025_GLOBE_R2023A_54009_100/V1-0/tiles/GHS_POP_E2025_GLOBE_R2023A_54009_100_V1_0_R8_C11.zip"
url3 <- "https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/GHSL/GHS_POP_GLOBE_R2023A/GHS_POP_E2025_GLOBE_R2023A_54009_100/V1-0/tiles/GHS_POP_E2025_GLOBE_R2023A_54009_100_V1_0_R9_C11.zip"
url4 <- "https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/GHSL/GHS_POP_GLOBE_R2023A/GHS_POP_E2025_GLOBE_R2023A_54009_100/V1-0/tiles/GHS_POP_E2025_GLOBE_R2023A_54009_100_V1_0_R10_C11.zip"
url5 <- "https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/GHSL/GHS_POP_GLOBE_R2023A/GHS_POP_E2025_GLOBE_R2023A_54009_100/V1-0/tiles/GHS_POP_E2025_GLOBE_R2023A_54009_100_V1_0_R8_C12.zip"
url6 <- "https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/GHSL/GHS_POP_GLOBE_R2023A/GHS_POP_E2025_GLOBE_R2023A_54009_100/V1-0/tiles/GHS_POP_E2025_GLOBE_R2023A_54009_100_V1_0_R9_C12.zip"
url1 <- "https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/GHSL/GHS_POP_GLOBE_R2023A/GHS_POP_E2025_GLOBE_R2023A_54009_100/V1-0/tiles/GHS_POP_E2025_GLOBE_R2023A_54009_100_V1_0_R10_C12.zip"

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
  terra::project(from= crs(r_base) , to= crs(pop00))

# cortar
pop00c <- crop(pop00, ext_projected)

# proyectar a otra extensión 

pop00cp <- project(pop00c, r_base, method= "bilinear")

### cálculo a km2 ####

pop_km2 <- pop00cp*100



writeRaster(pop_km2, file.path(dir_Intermedios, paste0("pop_km2_",año_pop,".tif")), overwrite=T)

# amazonas

pop_km2A <- crop(pop_km2, region, mask=T) 
pop_km2A10 <- disagg(pop_km2A, fact = 10)

writeRaster(pop_km2A10, file.path(dir_Intermedios, paste0("pop_km2A_",año_pop,".tif")), overwrite=T)


##  LU y TNT --------------------------------------------------
# 1.Creación de atributos para definir los pesos acorde a la cobertura Y Para Definir transformado y no transformado
# 2. Rasterización  Para la creación de las capas LU (Pesos de huella de acuerdo con la cobertura) Y tnt(Transformado - no transformado)
# 2018: "Corine/Cobertura_de_la_Tierra_100K_Periodo_2018/COBERTURAS CORINE 2018/ECOSISTEMAS_062021.gdb"
# 2020: "/Corine/Cobertura_de_la_tierra_100K_Periodo_2020_limite_administrativo (1)/Cobertura_de_la_tierra_100K_Periodo_2020_limite_administrativo/ECOSISTEMAS_14072024.gdb"
# 2022: "Corine/Cobertura_tierra_100K_periodo_2022_limite_administrativo/Cobertura_tierra_100K_periodo_2022_limite_administrativo/ECOSISTEMAS_18062025.gdb"

### Cargar los datos ####
# En esta sección por favor ajustar Nombre de la ruta Del archivo Y el nombre de la capa en caso de que sea una GDB. 


#corine <- st_read("Datos/Corine/amazonas/Coberturas_de_la_Tierra_2024_SI_Escala_1_25000-20260211T213750Z-1-001/Coberturas_de_la_Tierra_2024_SI_Escala_1_25000/Coberturas_de_la_Tierra_2024_SI_Escala_1_25000.shp")


corine <- st_read("Datos/Corine/amazonas/Coberturas_de_la_Tierra_2025_SI_Escala_1_25000-20260211T213757Z-1-001/Coberturas_de_la_Tierra_2025_SI__Escala_1_25000.shp")
corine <- st_read("Datos/Corine/amazonas/COBERT~2/CAPA_D~1.SHP")
d <- st_read("C:/Users/alejandra.narvaez/Downloads/Coberturas_de_la_Tierra_NDFyB_25K_Enero_2026.geojson")
d <- st_read("C:/Users/alejandra.narvaez/Downloads/Coberturas_de_la_Tierra_NDFyB_25K_Octubre_2025.geojson")
d <- st_read("C:/Users/alejandra.narvaez/Downloads/Coberturas_de_la_Tierra_NDFyB_25K_Ene2025/Coberturas_de_la_Tierra_NDFyB_25K_Ene2025.shp")

d %>% st_drop_geometry()
names(d)

# proyectar a sistema de referencia  base
corine_col_p <- st_transform(corine,scoord)

# Reclasificar a pesos huella
# revisar nombres de los campos y definir columna para la clasificacion a pesos de huella
names(corine)
plot(corine$geometry)

corine_col_p <- st_intersection(corine_col_p, region_buf)

# Definir la columna que quieres usar
Cod_ecos <- "codigo"

### Crear atributo de peso huella Y transformando - No transformado ####

# Si el raster no existe, reproyectar y guardar

archivo_LU0 <- file.path(dir_Intermedios,paste0( "LU0_corineA10",Año,"enero.tif"))
archivo_TNT0 <- file.path(dir_Intermedios,paste0( "TNT0_corineA10",Año,"enero.tif"))

r_base10A <- crop(r_base10,region_buf)


# Condición para crear o no los archivos
if (!file.exists(archivo_LU0) &
    !file.exists(archivo_TNT0) ) {
  

corine_col_p <- corine_col_p %>%
#  select(1,7:12) %>% 
  mutate(
    pesos_det = case_when(
      grepl("^1", as.character(!!sym(Cod_ecos))) ~ 5,   # Artificial humana
      grepl("^21", as.character(!!sym(Cod_ecos))) ~ 3,  # Agricultura extensiva
      grepl("^224", as.character(!!sym(Cod_ecos))) ~ 2,  # Agriforestal. como la condicion aparece primero que la siguiente. los valores no se reescriben. 
      grepl("^22", as.character(!!sym(Cod_ecos))) ~ 4,  # Agricultura intensiva
      
      grepl("^23", as.character(!!sym(Cod_ecos))) ~ 3,  # Pasto
      #grepl("^24", as.character(!!sym(Cod_ecos))) ~ 2,  # Agricultura mosaico. En las siguientes dos líneas se desabregó
      grepl("^241|^242", as.character(!!sym(Cod_ecos))) ~ 3,   # Mosaico de cultivos Y pastos
      grepl("^243|^244|^245", as.character(!!sym(Cod_ecos))) ~ 2,   # Mosaico de cultivos, pastos y espacios naturales
      grepl("^311|^312|^314", as.character(!!sym(Cod_ecos))) ~ 0,   # Bosques
      grepl("^313|^315", as.character(!!sym(Cod_ecos))) ~ 2,   # Fragmentado y plantación
      grepl("^323", as.character(!!sym(Cod_ecos))) ~ 1,   # Vegetación secundaria
      grepl("^321|^322", as.character(!!sym(Cod_ecos))) ~ 0,   # Arbustivo y herbazal
      grepl("^331|^332|^335", as.character(!!sym(Cod_ecos))) ~ 0,   # Desnudo natural
      grepl("^333|^334", as.character(!!sym(Cod_ecos))) ~ 1,   # Desnudo degradado
      grepl("^4", as.character(!!sym(Cod_ecos))) ~ 0,   # Humedales
      grepl("^511|^512", as.character(!!sym(Cod_ecos))) ~ 0,   # Agua natural
      grepl("^513|^514", as.character(!!sym(Cod_ecos))) ~ 5,   # Agua no natural
      grepl("^521|^522", as.character(!!sym(Cod_ecos))) ~ 0,   # Agua natural
      grepl("^523", as.character(!!sym(Cod_ecos))) ~ 2,   # Acuicultura marina
      TRUE ~ NA_real_
    )
  ) %>% 
  mutate( # 1: Transformado, 0: No transformado
  TNT = case_when(
    pesos_det %in% c(2:5) ~ 0,   # Transformado
    pesos_det %in% c(0) ~ 1,   # Natural
    grepl("^323", as.character(!!sym(Cod_ecos))) ~ 1,   # Vegetación secundaria
    grepl("^333|^334", as.character(!!sym(Cod_ecos))) ~ 0,   # Desnudo degradado
    TRUE ~ NA_real_
  )
)

### Rasterizar ####
corine_col_p <- st_cast(corine_col_p, "MULTIPOLYGON")

LU0 <- terra::rasterize(corine_col_p, r_base10, field="pesos_det")
TNT0 <- terra::rasterize(corine_col_p, r_base10, field="TNT")

# Guardar los archivos
writeRaster(LU0, archivo_LU0, datatype = "INT1U", overwrite = TRUE)
writeRaster(TNT0, archivo_TNT0, datatype = "INT1U", overwrite = TRUE)

} else {
  print("Los rasters TNT0 y LU0 ya fueron creados")  
}




