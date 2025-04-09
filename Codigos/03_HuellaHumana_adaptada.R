# Título: IHEH cersion ecosistemas
#
# Autor(es): Alejandra Narváez Vallejo
#
# Descripción: En este código calcula la huella con el método de ecosistemas. Esta incluye los siguiientes cambios.
## - Variables continuas como no continuas
##    - Distancia a vias según Venter et al 2016. Falta revisar función ??????????????????   
##    - Población según Venter et al 2016
##    - Densidad de áreas naturales, presion humana disminuye exponencialmente con los mayores valores del índice
##    - uso de la tierra

## - Variables no tomadas en cuanta en el cálculo
##    - Distancia a asentamientos
##    - Biomasa
##    - Tiempo de intervención

## - Indice de fragmentación se calculó filtando el raster con un filtro  circular de 1000 metros de diametro y usando al función suma. Esta parte está identica a la versión Diáz de la Huella, pero diferente al modelo construido en ArcMap. 


# Por hacer o corregir: 

## - Ver como paralelizar LU.
# falta el de TI 2022 genérico



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

setwd(file.path(this.path::this.path(), "..", ".."))

dir_datos <- file.path("datos")
dir_Intermedios<- file.path ("Res_Intermedios")
dir_Resultados <- file.path("Resultados")


#**********************************************************
# Cargar los datos necesarios ----------------------------
#**********************************************************

## Año #### 
# Escriba el año de interes
Año <- 2018
# Escriba el año de los datos de población
Año_pop <- 2020

# Raster base de referencia
r_base <- rast(file.path(dir_datos, "r_base.tif"))          # Resolución 100 m
r_base10 <- rast(file.path(dir_datos, "r_base10.tif"))      # Resolución 10 m

# Uso del suelo (LU)
LU0 <- rast(file.path(dir_datos, "Mapbiomas", paste0("colombia_coverage_", Año, ".tif")))

# Población
Pop0 <- file.path(dir_Intermedios, paste0("pop_km2_", Año_pop, ".tif")) %>% rast()

# Ecosistemas potenciales
EcoPotif1 <- file.path(dir_datos, "Eco_100Rc1.tif") %>% rast()

# Tiempo de intervención (capa base)
Ti_0 <- file.path(dir_Intermedios, "TiempoInt_2018_100.tif") %>% rast()

# Vectores de infraestructura vial
vias <- file.path(dir_Intermedios, "osm_IGAc_2018.shp") %>% st_read()

# Tablas de leyenda y combinaciones
Leyenda_LU <- read.csv2(file.path(dir_datos, "Leyenda_LU.txt"))
combinaciones <- read.csv2(file.path(dir_datos, "combinaciones.csv"))[-1]

#**********************************************************
# Cargar variables necesarias ----------------------------
#**********************************************************


## Años tiempo de intervención ####
# escriba en años la diferencia entre el año de la última IHEH y el el año de la huella a calcular

AñosTI <-  0


## TNT ####
# definicion transformado no transformado
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

#**********************************************************
# Preparar datos ----------------------------
#**********************************************************


## Preparar LU  ####
#**********************************************************

### ajustar resolución
# reproyectar con r_base 10 m
# con filtro de mayoría obtener 100m

raster_paths <- paste0(dir_Intermedios, "/LU_", Año,".tif")


if (file.exists(raster_paths)) {
  LU <- rast (raster_paths)
} else {
  # Si el raster no existe, rasterizar y guardar el resultado
  LU <- LU0 %>%
    project(r_base10, method = "near") %>%
    aggregate(fact = 10, fun = "modal")
  
  levels(LU) <- Leyenda_LU[, 2:3]
  writeRaster(LU, raster_paths, datatype= "INT1U")
  
}


## TNT  ####
#**********************************************************

# Crear una matriz de reclasificación para áreas transformadas y no transformadas.
# 1: Transformado, 0: No transformado, NA: No observado (valor 27)
m <- rbind(
  cbind(Leyenda_LU[Leyenda_LU$Leyenda %in% transformado, "Id"], 1), 
  cbind(Leyenda_LU[Leyenda_LU$Leyenda %in% Ntransformado, "Id"], 0), 
  cbind(27, NA)# significa No observado, por esto van a haber huecos en el producto
)

# Reclasificar el raster de uso del suelo a transformado / no transformado
TNT <- classify(LU, m)
names(TNT) <- "TNT"
plot(TNT)

# Desactivado porque no se va a usar 
## Ti_he  ####
# #**********************************************************
# 
# Ti_0
# 
# x0 <- 0  # Ajusta el desplazamiento si es necesario
# k <- 0.02  # Ajusta la rapidez del crecimiento
# 
# Ti_he <-10 * (1 - exp(-k * (Ti_0 - x0))) # esta curva se estabiliza llegando a 300
# 
# Ti_he [Ti_he > 10] <- 10
  

## GTF_lu ####
#**********************************************************

names(combinaciones)
# Crear raster combinando TNT, ecosistemas potenciales y uso del suelo
GTF <- TNT + EcoPotif1 * 1000 + LU * 10
# Asociar la tabla de combinaciones como niveles del raster
levels(GTF) <- as.data.frame(combinaciones)


# Reclasificar para obtener Lu_he (columna 12 de la tabla de combinaciones)
m <- cbind(combinaciones[, 1], combinaciones[, 12])
Lu_he <- classify(GTF, m, other = NA) * 2
head(m)
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

vias <- st_transform(vias, crs(r_base))            # Asegurar proyección compatible
r_vias <- rasterize(vias, r_base)                  # Rasterizar
vias_d <- terra::distance(r_vias)                  # Distancia a vías

# Función de decaimiento exponencial
dr_he <- 4 * exp(-0.319 * (vias_d / 1000 - 0.5))
dr_he[dr_he > 4] <- 8                              # Límite superior

#2.426123*exp(-1*(seq(.5,15,0.1)-1)) parecido a el de un artículo

hist(dr_he)
plot(dr_he)



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
  paste0(dir_Resultados, "/IHEHc_", Año, ".tif"), 
  overwrite=TRUE)



### Revisar resultado####

plot(IHEH1002)
#click(IHEH100)
plot(IHEH1002, breaks = c(0, 15, 40, 60, 100),col=c("blue","yellow","red","red" ))

plot(Pd_he)
plot(if_he)
plot(dr_he)
plot(Lu_he)
plot(GTF)


