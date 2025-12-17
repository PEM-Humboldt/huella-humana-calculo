# Título: IHEH version ecosistemas
#
# Autor(es): Alejandra Narváez Vallejo
#
# Descripción: En este código calcula la huella con el método de ecosistemas. Esta incluye los siguiientes cambios.
## - Variables continuas como no continuas
##    - Distancia a vias según Venter et al 2016. Falta revisar función ??????????????????  con más categorías incluidas   
##    - Población según Venter et al 2016
##    - Densidad de áreas naturales, presion humana disminuye exponencialmente con los mayores valores del índice
##    - uso de la tierra

## - Variables no tomadas en cuanta en el cálculo
##    - Distancia a asentamientos
##    - Biomasa
##    - Tiempo de intervención

## - Indice de fragmentación se calculó filtando el raster con un filtro  circular de 1000 metros de diametro y usando al función suma. Esta parte está identica a la versión Diáz de la Huella, pero diferente al modelo construido en ArcMap. 


# Por hacer o corregir: 

# En principio es igual que Corinne Por eso no corregiré el código acá
# es el mismo que Corine, llamar source, 
# recordar que en la carpeta no hay ferreo debe ser integrado 




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
LU <- rast( file.path(dir_Intermedios, paste0("mapbiomas_pesos_",Año,".tif")))

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


# Tablas de leyenda y combinaciones

Leyenda_LU <- read.csv2(file.path(dir_datos, "Leyenda_LU.txt"))


#**********************************************************
# Cargar variables necesarias ----------------------------
#**********************************************************


## Años tiempo de intervención ####
# escriba en años la diferencia entre el año de la última IHEH y el el año de la huella a calcular

AñosTI <-  0


## TNT ####
# definicion transformado no transformado De acuerdo a los nombres de Mapbiomas
transformado <-  c(
  'Acuicultura',
  'Infraestructura urbana',
  'Minería',
  'Mosaico de agricultura y/o pasto',
  'Palma aceitera',
  'Silvicultura',
  'Otra área sin vegetación'
)


Ntransformado <- c(
  'Afloramiento rocoso',
  'Bosque' ,
  'Bosque inundable'  ,
  'Formación herbácea',
  'Formación natural no forestal inundable',
  'Glaciar' ,
  'Manglar'  ,
  'Otra formación natural no forestal'  ,
  'Planicie de marea hipersalina' ,
  'Playas, dunas y bancos de arena'  ,
  'Río, lago u océano'  ,
  'Vegetación herbácea sobre arena',
  'Vegetación leñosa sobre arena'
)

# Necesita que se vuelvan a correr las proyecciones porque hubo un cambio antes? TRUE Si quiere volver a guardar los rasters a pesar que ya existan

#rerun <-  TRUE
#rerun <-  FALSE

#**********************************************************
# Preparar datos ----------------------------
#**********************************************************


## TNT  ####
#**********************************************************

# Crear una matriz de reclasificación para áreas transformadas y no transformadas.
# 1: Transformado, 0: No transformado, NA: No observado (valor 27)
m <- matrix(c(0,0,0,1,5,1), ncol=3, byrow = T)
  
m
# Reclasificar el raster de uso del suelo a transformado / no transformado
TNT <- classify(LU, m, include.lowest=T)
names(TNT) <- "TNT"
plot(TNT)
freq(TNT)
freq(LU)


## lu ####
#**********************************************************

Lu_he <- LU * 2
gc()
plot(Lu_he)


## Pop Pd_he  ####
#**********************************************************

plot(Pop0)
Pd_he <- 3.333 * log10(Pop0 + 1)  # Log-transformación
Pd_he[Pd_he > 10] <- 10           # Limitar a máximo 10

plot(Pd_he)


## vias- dr_he  ####
#**********************************************************

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

# Asignar el valor máximo de influencia directa para la vía de categoría 2
Vias_2R$v2[Vias_2R$v2 > 2] <- 2
Vias_2R$v4[Vias_2R$v4 > 2] <- 4

# Combinar todas las capas y calcular el valor máximo por celda entre las capas de influencia
dr_he <- app(c(Vias_2R$v2, Vias_2R$v4, Vias_4R$v5, Vias_4R$v8), max)

# Visualización de las capas intermedias y resultado final
plot(Vias_4R$v8, main = "Influencia vías categoría 8")
plot(Vias_4R$v5, main = "Influencia vías categoría 5")
plot(Vias_2R$v4, main = "Influencia vías categoría 4")
plot(Vias_2R$v2, main = "Influencia vías categoría 2")
plot(dr_he, main = "Capa combinada de influencia vial (dr_he)")
hist(dr_he, main = "Histograma de influencia vial (dr_he)")

dr_he  # Resultado final


## if_he  ####
#**********************************************************

vecindad <- focalMat(TNT, type = "circle", d = 1000)  # Ventana de 1 km
r_reclass <- classify(TNT, cbind(0:1, 1:0))            # Invertir 0 y 1

# Sumar área transformada en vecindad

densidad_0 <- focal(r_reclass,
                    w = vecindad,
                    fun = sum,
                    na.rm = TRUE) * 100

if_he <- 10 * exp(-0.05 * densidad_0)

densidad_0

plot(densidad_0)

plot(if_he)


# Cálculo de Huella ####
#**********************************************************
IHEH <- Lu_he + Pd_he + if_he + dr_he
IHEH1002 <- 100 / 38 * IHEH  # Normalización a escala 0-100

plot(IHEH1002)

# Guardar resultado
writeRaster(
  IHEH1002,
  paste0(dir_Resultados, "/IHEHc2_", Año, ".tif"), 
  overwrite=TRUE)


IHEH1002 <- rast( paste0(dir_Resultados, "/IHEHc2_", Año, ".tif"))

### Revisar resultado####

plot(IHEH1002)
#click(IHEH100)
plot(IHEH1002, breaks = c(0, 15, 40, 60, 100),col=c("blue","yellow","orange","red" ))
plot(IHEH1002, breaks = c(0, 1,15,  30, 60,100),col=c("blue","yellow","orange","orange4","red" ))

plot(Pd_he)
plot(if_he)
plot(dr_he)
plot(Lu_he)
plot(GTF)

IHEH1002 <- rast(paste0(dir_Resultados, "/IHEHc2_", 2022, ".tif"))

# Reclasificar a las categorías discretas
# Definir los breaks y las etiquetas
breaks <- c(0, 0, 4,10,22, 100)
labels <- c("Natural", "Baja", "Media", "Alta", "Muy Alta")


breaks/100*38

# Reclasificar usando classify() + as.factor()
# Primero, convertir a clases numéricas
rc_matrix <- matrix(c(0, 0, 1,
                      0, 15, 2,
                      15, 60, 3,
                      60,100, 4), 
                    ncol = 3, byrow = TRUE)

r_class <- classify(IHEH1002, rc_matrix)

# Convertir a factor y asignar etiquetas
levels(r_class) <- data.frame(ID = 1:4, clase = labels)

# Resultado: raster categórico con etiquetas
r_class <- project(r_class, "EPSG:4326")

# Guardar resultado
writeRaster(
  r_class,
  paste0(dir_Resultados, "/IHEHc2_cls", Año, ".tif"), 
  overwrite=TRUE)


r_class <- rast(paste0(dir_Resultados, "/IHEHc2_cls", Año, ".tif"))
plot(r_class )
