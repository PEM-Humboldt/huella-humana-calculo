# Título: IHEH version ecosistemas
#
# Autor(es): Alejandra Narváez Vallejo
#
# Descripción: En este código calcula la huella con el método de ecosistemas. Esta incluye los siguiientes cambios.
## - Variables continuas como no continuas
##    - Distancia a vias según Venter et al 2016. Inclusión de vía secundarias terciarias y caminos. Inclusión de vías férreas
##    - Población según Venter et al 2016
##    - Densidad de áreas naturales,los pesos relacionados con la presion humana disminuye exponencialmente con los mayores valores del índice
##    - Uso de la tierra: Según Correa et al. (2020) y Etter et al. (2011), 


# Por hacer o corregir: 



#**********************************************************
# librerías o dependencias --------------------------------
#**********************************************************


# lectura de datos
library (sf)
library(terra)
library(tidyverse)
library(raster)


#**********************************************************
# Definir directorio(s) de trabajo -----------------------
#**********************************************************

setwd(file.path(this.path::this.path(),"..", "..", "..",".."))

dir_datos <- file.path("datos")
dir_Intermedios<- file.path ("Res_Intermedios")
dir_Resultados <- file.path("Resultados")


#**********************************************************
# Cargar los datos necesarios ----------------------------
#**********************************************************

scoord     <- crs("EPSG:9377") # Sistema de coordenadas oficial para el análisis

## Año #### 
# Escriba el año de interes
Año <- 2025

# Escriba el año de los datos de población que va a usar
Año_pop <- 2025

# Raster base de referencia
r_base10 <- rast(file.path(dir_datos, "r_base10.tif"))          # Resolución 100 m

# Uso del suelo (LU)

LU <- rast(file.path(dir_Intermedios,paste0( "LU0_corineA10",Año,"enero.tif")))

# TNT
TNT <- rast(file.path(dir_Intermedios,paste0( "TNT0_corineA10",Año,"enero.tif")))

# Población
Pop0 <- rast(file.path(dir_Intermedios, paste0("pop_km2A_", Año_pop, ".tif")))


# Vectores de infraestructura vial

vias8 <- file.path(dir_Intermedios, paste0 ("osm_IGAc8AS_",Año,"enero.shp")) %>%
  st_read()
vias5 <- file.path(dir_Intermedios, paste0 ("osm_IGAc5AS_",Año,"enero.shp")) %>%
  st_read()
vias4 <- file.path(dir_Intermedios, paste0 ("osm_IGAc4AS_",Año,"enero.shp")) %>%
  st_read()
vias2 <- file.path(dir_Intermedios, paste0 ("osm_IGAc2AS_",Año,"enero.shp")) %>%
  st_read()

# vias ferreas

V_ferreas4 <- rast(file.path(dir_Intermedios, paste0("pesos_trenesA_", 4,"_",Año, ".tiff")))
V_ferreas6 <- rast(file.path(dir_Intermedios, paste0("pesos_trenesA_", 6,"_",Año, ".tiff")))


# rios
rios_pesos <- rast(file.path(dir_Intermedios, paste0("pesos_navegabilidadA_filtroSonly.tiff")))


# region de interes
region <- st_read("Datos/amazonas/NDFyB_V5-Amazonia_proHuella.shp") %>% st_transform(scoord)


#**********************************************************
# Cargar variables necesarias ----------------------------
#**********************************************************

# Necesita que se vuelvan a correr las proyecciones porque hubo un cambio antes? TRUE Si quiere volver a guardar los rasters a pesar que ya existan

#rerun <-  TRUE
rerun <-  FALSE



#**********************************************************
# Preparar datos ----------------------------
#**********************************************************

## lu ####
#**********************************************************
# Reescalar la cobertura de la tierra De cero a 10 Ya que está de cero a 5

plot(LU)
LU <-  crop(LU, region)
Lu_he <- LU * 2
gc()
plot(Lu_he)


## Pop Pd_he  ####
#**********************************************************

# Definición de los pesos Basados En la densidad poblacional Según Venter 2016

plot(Pop0)
Pd_he <- 3.333 * log10(Pop0 + 1)  # Log-transformación
Pd_he[Pd_he > 10] <- 10           # Limitar a máximo 10

plot(Pd_he)


## vias- dr_he  ####
#**********************************************************
# Asignar los pesos A las carreteras y vías férreas

r_base10A <- crop(r_base10,Pop0)
# Crear una lista con las capas de vías transformadas a la misma proyección de r_base, 
# luego rasterizar cada una sobre la cuadrícula base y calcular la distancia euclidiana desde cada celda
vias_groups <- lapply(list(vias2, vias4, vias5, vias8), function(x) {
  p <- st_transform(x, crs(r_base10A)) %>%
    rasterize(r_base10A) %>%
    terra::distance()
})


# Asignar nombres representativos a cada categoría de vía
names(vias_groups) <- c("v2", "v4", "v5", "v8")

# Curva de decaimiento basada en función exponencial ajustada desde literatura
# 2.426123*exp(-1*(seq(.5,15,0.1)-1)) parecido a el de un artículo

# Definir función de clasificación basada en distancia a vías
# 'max' define el valor máximo de influencia directa
clsDisVias <- function(x, max = 4) {
  max * exp(-0.33 * (x / 1000 - 0.5))
}

# Aplicar la función de clasificación a las vías de categorías 4, 5 y 8 (mayor impacto)
Vias_4R <- lapply(vias_groups[3:4], clsDisVias)

# Asignar valores máximos de influencia directa según el tipo de vía
Vias_4R$v5[Vias_4R$v5 > 4] <- 5 
Vias_4R$v8[Vias_4R$v8 > 4] <- 8 

# ajustar valores mínimos
Vias_4R$v5[Vias_4R$v5 < 0.034] <- 0 
Vias_4R$v8[Vias_4R$v8 < 0.034] <- 0 

# Aplicar la función de clasificación a la vía de categoría 2 (menor impacto)
Vias_2R <- lapply(vias_groups[1:2], clsDisVias, max = 2)

# Asignar el valor máximo de influencia directa para la vía de categoría 2 y 4
Vias_2R$v2[Vias_2R$v2 > 2] <- 2
Vias_2R$v4[Vias_2R$v4 > 2] <- 4


# ajustar valores mínimos
Vias_2R$v2[Vias_2R$v2 < 0.017] <- 0 
Vias_2R$v4[Vias_2R$v4 < 0.017] <- 0 

# Asignar ceros a los valores NA En las vías férreas

V_ferreas4[is.na(V_ferreas4)] <- 0
V_ferreas6[is.na(V_ferreas6)] <- 0

V_ferreas4 <- crop(V_ferreas4,Pop0)
V_ferreas6 <- crop(V_ferreas6,Pop0)

# revisar Rios que la estructura este correcta

rios_pesosA <- crop(rios_pesos,Pop0)



# Combinar todas las capas y calcular el valor máximo por celda entre las capas de influencia
dr_he <- app(c(Vias_2R$v2, Vias_2R$v4, Vias_4R$v5, Vias_4R$v8, V_ferreas4, V_ferreas6, rios_pesosA), max)
# Se hace la corrección para que a partir de 15 km el valor sea cero
#dr_he[dr_he < 0.035] <- 0 

# Visdr_he0# Visualización de las capas intermedias y resultado final
gc()
plot(Vias_4R$v8, main = "Influencia vías categoría 8")
plot(Vias_4R$v5, main = "Influencia vías categoría 5")
plot(Vias_2R$v4, main = "Influencia vías categoría 4")
plot(Vias_2R$v2, main = "Influencia vías categoría 2")
plot(V_ferreas4, main = "Vía férrea inactiva")
plot(V_ferreas6, main = "Vía férrea activa")
plot(dr_he, main = "Capa combinada de influencia vial (dr_he)")
hist(dr_he, main = "Histograma de influencia vial (dr_he)")

dr_he  # Resultado final


plot(dr_he)


## if_he  ####
#**********************************************************
# Se calcula Un indicativo de la fragmentación basado en densidad de píxeles Naturales En un área específica De 1 km de radio y se le asignan los pesos The huella Basado en decaimiento exponencial

TNT <-  crop(TNT, region)

LU24  <- rast(paste0(dir_Resultados, "/LUA", 2024, ".tif")) # Se usa una máscara con La cobertura del 24 para evitar diferencias con el 2025 Ya que este tiene una extensión diferente y causada Bordes De impacto bajo
TNT <-  mask(TNT, LU24)
plot(TNT)
vecindad <- focalMat(TNT, type = "circle", d = 1000)  # Ventana de 1 km

# Sumar área transformada en vecindad

densidad_0 <- focal(TNT,
                    w = vecindad,
                    fun = sum,
                    na.rm = TRUE) * 100

if_he <- 10 * exp(-0.05 * densidad_0)
# Se hace la corrección para que Si la cobertura es Completamente natural este sirve valor 100 del if_he sea 0
if_he[if_he < 0.07] <- 0 # ajuste para que apartir de 99 y 100  de 0, o sino no habría huella natural 
densidad_0

plot(densidad_0)

plot(if_he)


# Cálculo de Huella ####
#**********************************************************
#*
Lu_he <- rast(
   paste0(dir_Resultados, "/LUA", Año, ".tif")
 )
Pd_he <- rast(
   paste0(dir_Resultados, "/PopA", Año, ".tif")
 )
if_he <- rast(
  paste0(dir_Resultados, "/fragA", Año, ".tif")
  )

# if_he <- rast(
#   paste0(dir_Resultados, "/fragAregion2024", Año, ".tif") #### mucho cuidado super necesasrio para el 2025
# )

dr_he <- rast(
  paste0(dir_Resultados, "/ViasAS", Año, ".tif"))

#*
#*

Pd_he <- crop(Pd_he,Lu_he)
dr_he <- crop(dr_he,Lu_he)

IHEH <- Lu_he + Pd_he + if_he + dr_he
IHEH1002 <- 100 / 38 * IHEH  # Normalización a escala 0-100

### Revisar resultado####

plot(Pd_he)
plot(if_he, add=T)
plot(dr_he)
plot(Lu_he)
plot(IHEH1002)

## Guardar resultado crs:9377####
writeRaster(
  IHEH1002,
  paste0(dir_Resultados, "/IHEH_IAVHA_S_enerosh", Año, ".tif"), 
  overwrite=TRUE)


# writeRaster(
#   IHEH1002,
#   paste0(dir_Resultados, "/IHEH_IAVHAS_enerohalo", Año, ".tif"), 
#   overwrite=TRUE)


#Guardar capas intermedias

writeRaster(
  Lu_he,
  paste0(dir_Resultados, "/LUA", Año, ".tif"), 
  overwrite=TRUE)
writeRaster(
  Pd_he,
  paste0(dir_Resultados, "/PopA", Año, ".tif"), 
  overwrite=TRUE)
writeRaster(
  if_he,
  paste0(dir_Resultados, "/fragA2024", Año, ".tif"), 
  overwrite=TRUE)
writeRaster(
  dr_he,
  paste0(dir_Resultados, "/ViasAS", Año, ".tif"), 
  overwrite=TRUE)


#IHEH1002 <- rast( paste0(dir_Resultados, "/IHEH_IAVH", Año, ".tif")) # Activar de ser necesario

# Reclasificar a las categorías discretas

# Definir los breaks y las etiquetas
labels <- c("Natural", "Baja", "Media", "Alta", "Muy Alta")

# Reclasificar usando classify() + as.factor()
# Primero, convertir a clases numéricas
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
# Guardar resultado
writeRaster(
  r_class,
  paste0(dir_Resultados, "/IHEH_IAVHA_S_class_enero2sh", Año, ".tif"), 
  overwrite=TRUE)

Sys.time()


## Guardar resultado crs:4326####
Año <- 2018

IHEH1002 <- rast(
    paste0(dir_Resultados, "/IHEH_IAVH1", Año, ".tif"))

# # creacion del raster base (Solo es necesario correrlo una vez)
# IHEH1002_wgs <- project(IHEH1002,"EPSG:4326")
# r_base_wgs <- IHEH1002_wgs
# values( r_base_wgs) <- 0
# writeRaster(
#   r_base_wgs,
#   paste0(dir_datos, "/rbaseWgs.tif"), 
#   overwrite=TRUE)


IHEH1002_wgs <- project(IHEH1002,r_base_wgs)

r_class_wgs <- classify(IHEH1002_wgs, rc_matrix)

plot(r_class_wgs)
# Convertir a factor y asignar etiquetas
levels(r_class_wgs) <- data.frame(ID = 1:5, clase = labels)

plot(r_class_wgs)
# Guardar resultado

writeRaster(
  IHEH1002_wgs,
  paste0(dir_Resultados, "/IHEH_IAVH_-wgs", Año, ".tif"), 
  overwrite=TRUE)
writeRaster(
  r_class_wgs,
  paste0(dir_Resultados, "/IHEH_IAVH_class_wgs", Año, ".tif"), 
  overwrite=TRUE)

