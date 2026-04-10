# Título: Preparación insumos por Huella 
#
# Autor(es): Alejandra Narváez Vallejo
#
# Descripción: En este código se preparan los insumos necesarios para correr el IHEH
# Estos son :
## - Vias: 2018: IGAc de Julian y descarga 2019 de osm por que tiene fecha enero 2019 
## - Vias: 2020: IGAc (https://www.colombiaenmapas.gov.co/?e=-82.43784778320864,-0.17644239911865092,-71.23179309571162,9.90326984502256,4686&b=igac&u=0&t=23&servicio=205) y descarga 2021 de osm por que tiene fecha enero 2021, Para el cálculo de años posteriores seguir esquema del 2020
## - Vias: 2022: IGAc (https://www.colombiaenmapas.gov.co/?e=-82.43784778320864,-0.17644239911865092,-71.23179309571162,9.90326984502256,4686&b=igac&u=0&t=23&servicio=205) y descarga 2023 de osm por que tiene fecha enero 2023, Para el cálculo de años posteriores seguir esquema del 2020



# Por hacer o  corregir: 
## revisar en detalle para etar seguro que todo funcione  y se acople
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

osm0<-st_read(file.path(dir_datos,"vias", "colombia-190101-free.shp","gis_osm_roads_free_1.shp"))#2018
#osm0<-st_read(file.path(dir_datos,"vias", "colombia-210101-free.shp","210101.shp"))#2020
#osm0<-st_read(file.path(dir_datos,"vias", "colombia-230101-free.shp","gis_osm_roads_free_1.shp"))#2022

vias_IGAC0 <- st_read(file.path(dir_datos,"vias","ViasJulian2018","vias.shp"))# 2018
#vias_IGAC0 <- st_read(file.path(dir_datos,"vias","IGAC_viasD2024","Vias_IGAC.shp"))# 2022 y 2020

# Capas Raster
r_base<-rast(file.path(dir_datos,"r_base.tif" ))
#r_base10<-rast(file.path(dir_datos,"r_base10.tif" ))

#**********************************************************
# Preparar datos ----------------------------
#**********************************************************
## adaptacion a código independiente ####

archivos <- list(
  igac8 = file.path(dir_Intermedios, paste0("osm_IGAc8_", Año, ".shp")),
  igac5 = file.path(dir_Intermedios, paste0("osm_IGAc5_", Año, ".shp")),
  igac4 = file.path(dir_Intermedios, paste0("osm_IGAc4_", Año, ".shp")),
  igac2 = file.path(dir_Intermedios, paste0("osm_IGAc2_", Año, ".shp"))
)


if (all(file.exists(unlist(archivos)))) {
  
  message("✔️ Capas OSM–IGAC ya existen para el año ", Año, 
          ". No se reprocesan.")
  
} else {
  
  message("⏳ Procesando capas OSM–IGAC para el año ", Año)
  
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
  
  # ---- Guardar resultados intermedios ----
  st_write(osm_igac8, archivos$igac8, append = FALSE, quiet = TRUE, append = FALSE)
  st_write(osm_igac5, archivos$igac5, append = FALSE, quiet = TRUE, append = FALSE)
  st_write(osm_igac4, archivos$igac4, append = FALSE, quiet = TRUE, append = FALSE)
  st_write(osm_igac2, archivos$igac2, append = FALSE, quiet = TRUE, append = FALSE)
  
  message("✅ Capas OSM–IGAC guardadas correctamente para ", Año)
}

