# Título: IHEH version ecosistemas
#
# Autor(es): Alejandra Narváez Vallejo
#
# Descripción: En este código calcula la huella con el método de ecosistemas. Esta incluye los siguiientes cambios.
## - Variables continuas como no continuas
##    - Distancia a vias según Venter et al 2016. Inclusión de vía secundarias terciarias y caminos. Inclusión de vías férreas
##    - Población según Venter et al 2016
##    - Densidad de áreas naturales, presion humana disminuye exponencialmente con los mayores valores del índice
##    - uso de la tierra

## - Indice de fragmentación se calculó filtando el raster con un filtro  circular de 1000 metros de diametro y usando al función suma. Esta parte está identica a la versión Diáz de la Huella, pero diferente al modelo construido en ArcMap. 


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

setwd(file.path(this.path::this.path(), "..", "..",".."))

dir_datos <- file.path("datos")
dir_Intermedios<- file.path ("Res_Intermedios")
dir_Resultados <- file.path("Resultados")


#**********************************************************
# Cargar los datos necesarios ----------------------------
#**********************************************************

## Año #### 
# Escriba el año de interes
Año <- 2022

# Escriba el año de los datos de población que va a usar
Año_pop <- 2020

# Raster base de referencia
r_base <- rast(file.path(dir_datos, "r_base.tif"))          # Resolución 100 m

# Uso del suelo (LU)

LU <- rast(file.path(dir_Intermedios,paste0( "LU0_corine",Año,".tif")))

# TNT
TNT <- rast(file.path(dir_Intermedios,paste0( "TNT0_corine",Año,".tif")))

# Población
Pop0 <- rast(file.path(dir_Intermedios, paste0("pop_km2_", Año_pop, ".tif")))


# Vectores de infraestructura vial

vias8 <- file.path(dir_Intermedios, paste0 ("osm_IGAc8_",Año,".shp")) %>%
  st_read()
vias5 <- file.path(dir_Intermedios, paste0 ("osm_IGAc5_",Año,".shp")) %>%
  st_read()
vias4 <- file.path(dir_Intermedios, paste0 ("osm_IGAc4_",Año,".shp")) %>%
  st_read()
vias2 <- file.path(dir_Intermedios, paste0 ("osm_IGAc2_",Año,".shp")) %>%
  st_read()

# vias ferreas

V_ferreas4 <- rast(file.path(dir_Intermedios, paste0("pesos_trenes_", 4,"_",Año, ".tiff")))
V_ferreas6 <- rast(file.path(dir_Intermedios, paste0("pesos_trenes_", 6,"_",Año, ".tiff")))


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

# Crear una lista con las capas de vías transformadas a la misma proyección de r_base, 
# luego rasterizar cada una sobre la cuadrícula base y calcular la distancia euclidiana desde cada celda
vias_groups <- lapply(list(vias2, vias4, vias5, vias8), function(x) {
  p <- st_transform(x, crs(r_base)) %>%
    rasterize(r_base) %>%
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

# Aplicar la función de clasificación a la vía de categoría 2 (menor impacto)
Vias_2R <- lapply(vias_groups[1:2], clsDisVias, max = 2)

# Asignar el valor máximo de influencia directa para la vía de categoría 2 y 4
Vias_2R$v2[Vias_2R$v2 > 2] <- 2
Vias_2R$v4[Vias_2R$v4 > 2] <- 4

# Asignar ceros a los valores NA En las vías férreas

V_ferreas4[is.na(V_ferreas4)] <- 0
V_ferreas6[is.na(V_ferreas6)] <- 0



# Combinar todas las capas y calcular el valor máximo por celda entre las capas de influencia
dr_he <- app(c(Vias_2R$v2, Vias_2R$v4, Vias_4R$v5, Vias_4R$v8, V_ferreas4, V_ferreas6 ), max)
# Se hace la corrección para que a partir de 15 km el valor sea cero
dr_he[dr_he < 0.035] <- 0 

# Visdr_he0# Visualización de las capas intermedias y resultado final

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
# Se calcula Un indicativo de la fragmentación basado en densidad de píxeles Naturales En un área específica De 1 km de radio y se le asignan los pesos The huella Basado en decaimiento exponencial

vecindad <- focalMat(TNT, type = "circle", d = 1000)  # Ventana de 1 km

# Sumar área transformada en vecindad

densidad_0 <- focal(TNT,
                    w = vecindad,
                    fun = sum,
                    na.rm = TRUE) * 100

if_he <- 10 * exp(-0.05 * densidad_0)
# Se hace la corrección para que Si la cobertura es Completamente natural este sirve valor 100 del if_he sea 0
if_he[if_he < 0.07] <- 0
densidad_0

plot(densidad_0)

plot(if_he)


# Cálculo de Huella ####
#**********************************************************
IHEH <- Lu_he + Pd_he + if_he + dr_he
IHEH1002 <- 100 / 38 * IHEH  # Normalización a escala 0-100

### Revisar resultado####

plot(Pd_he)
plot(if_he)
plot(dr_he)
plot(Lu_he)
plot(IHEH1002)

# Guardar resultado
writeRaster(
  IHEH1002,
  paste0(dir_Resultados, "/IHEH_IAVH1", Año, ".tif"), 
  overwrite=TRUE)

#Guardar capas intermedias

writeRaster(
  Lu_he,
  paste0(dir_Resultados, "/LU1", Año, ".tif"), 
  overwrite=TRUE)
writeRaster(
  Pd_he,
  paste0(dir_Resultados, "/Pop", Año, ".tif"), 
  overwrite=TRUE)
writeRaster(
  if_he,
  paste0(dir_Resultados, "/frag", Año, ".tif"), 
  overwrite=TRUE)
writeRaster(
  dr_he,
  paste0(dir_Resultados, "/Vias", Año, ".tif"), 
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
  paste0(dir_Resultados, "/IHEH_IAVH_class", Año, ".tif"), 
  overwrite=TRUE)

Sys.time()




