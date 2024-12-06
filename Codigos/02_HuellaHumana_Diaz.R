
# Título: IHEH casi como Diaz 2022.
#
# Autor(es): Alejandra Narváez Vallejo
#
# Descripción: En este código calcula la huella 2018 replicando los pasos de los modelos de ArcMap de Julian Díaz. 
# Por motivos computacionales fue necesario hacer hacer las siguientes modificaciones:
## - Indice de fragmentación se calculó filtando el raster con un filtro  circular de 1000 metros de diametro y usando al función suma.


# Por hacer o  corregir: 

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

# Capas raster

r_base<-rast(file.path(dir_datos,"r_base.tif" )) 

r_base10 <- rast(file.path(dir_datos, "r_base10.tif"))
  
LU0 <- rast(file.path(dir_datos,
                 "Mapbiomas_2018",
                 "colombia_coverage_2018.tif"))
  

Pop0 <- file.path(dir_Intermedios, paste0("pop_km2_2020.tif")) %>%
  rast()


EcoPotif1 <- file.path(dir_datos, "Eco_100Rc1.tif") %>%
  rast()

Ti_0 <- file.path(dir_Intermedios, "TiempoInt_2018_100.tif") %>%
  rast()


# vector

vias <- file.path(dir_Intermedios, "osm_IGAc_2018.shp") %>%
  st_read()


# texto

Leyenda_LU <- read.csv2(file.path(dir_datos, "Leyenda_LU.txt"))
combinaciones <- read.csv2(file.path(dir_datos, "combinaciones.csv"))[-1]


#**********************************************************
# Cargar variables necesarias ----------------------------
#**********************************************************

## Año #### 
# Escriba el año de interes
Año <- 2018

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
# reptoyectar con r_base 10 m
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

m <- rbind(cbind(Leyenda_LU[Leyenda_LU$Leyenda %in% transformado, "Id"], 1), 
           cbind(Leyenda_LU[Leyenda_LU$Leyenda %in% Ntransformado, "Id"], 0), 
           cbind(27, NA)) # significa No observado, por esto van a haber huecos en el producto
           


TNT <- classify(LU, m)
names(TNT) <- "TNT"
plot(TNT)

writeRaster(TNT,paste0(dir_Intermedios, "/TNT_", Año,".tif"))


## Ti_he  ####
#**********************************************************

Ti_0

ma <- matrix(
  c(0, 1, 0,
    1, 15, 1,
    15, 50, 2,
    50, 150, 3,
    150, 300, 4,
    300, 600, 5),
  ncol = 3,
  byrow = TRUE
)
ma

Ti_he <- classify(Ti_0, ma, include.lowest = T) # por defecto cerrado a la derecha igual que arc map
plot(Ti_0)
plot(Ti_he)
hist(Ti_0)
unique(Ti_0)

## GTF  ####
#**********************************************************

names(combinaciones)

# crear raster de combinaciones y vincularle la tabla
GTF <- TNT+ EcoPotif1*1000+ LU*10
levels(GTF) <- as.data.frame(combinaciones)



## lu+bi=IHEH 1 ####
#**********************************************************

# crear el raster de suma preliminar

m <- cbind(combinaciones[, 1], combinaciones[, 14]) # rc matrix basado en categorias de raster y valores de IHEH que estan en la columna 14
head(m)
gc()
IHEH1 <- classify(GTF, m, other = NA)

plot(IHEH1)


## Pop Pd_he  ####
#**********************************************************

plot(Pop0)

m <- matrix(
  c( 0, 0, 0,
    0, 200, 1,
    200, 700, 2,
    700, 1500, 3,
    1500, 3500, 4,
    3500, 700000, 5
  ),
  ncol = 3,
  byrow = TRUE
)
m

Pd_he <- classify(Pop0, m, include.lowest = F, right = T)

plot(Pd_he)


## vias- dr_he  ####
#**********************************************************

vias <-  st_transform(vias, crs(r_base))

#plot(vias)

r_vias <- rasterize(vias, r_base)

vias_d <- terra::distance( r_vias)



mv <- matrix(
  c(
    0, 1500, 5,
    1500, 3000, 4,
    3000, 5000, 3,
    5000, 8000, 2,
    8000, 20000,  1,
    20000, 3897000, 0
  ),
  ncol = 3,
  byrow = TRUE
)
mv

dr_he <- classify(vias_d, mv, include.lowest = TRUE, right = TRUE)
plot(dr_he)


## Asentamientos ds_he  ####
#**********************************************************

Asentamientos <- classify(LU, cbind(24, 1), others = NA)

plot(LU)
plot(Asentamientos, col = "red", add = TRUE)

Asentamientos_d <- distance(
  classify(LU, cbind(24, 1), others = NA)# asentamientos
  )
  

ma <- matrix(
    c(
      0, 3000, 5,
      3000, 6000, 4,
      6000, 10000, 3,
      10000, 15000, 2,
      15000, 25000, 1,
      25000, 5000400, 0
    ),
    ncol = 3,
    byrow = TRUE
  )

  
ds_he <- classify(Asentamientos_d,
                  ma,
                  include.lowest = TRUE,
                  right = TRUE)

plot(ds_he)
plot(Asentamientos, add = TRUE, col = "red")

## if_he  ####
#**********************************************************

# Crear una vecindad de 1 km
vecindad <- focalMat(TNT, type = "circle", d = 1000)
vecindad

# Reclasificar los valores: 1 -> 0 y 0 -> 1
r_reclass <- classify(TNT, cbind(0:1, 1:0))


densidad_0 <- focal(r_reclass,
                    w = vecindad,
                    fun = sum,
                    na.rm = TRUE) * 100

densidad_0

plot(densidad_0)

mk <- matrix(
  c(0, 10, 5,
    10, 30, 4, 
    30, 50, 3,
    50, 70, 2,
    70, 90, 1,
    90, 100, 0),
  ncol = 3,
  byrow = TRUE
)

mk



if_he <- classify(densidad_0, mk, right = TRUE, include.lowest = TRUE)
plot(if_he)

# Calculo de Huella ####
#**********************************************************

IHEH <- IHEH1 + Ti_he + Pd_he + if_he + ds_he + dr_he
plot(IHEH)

IHEH100 <- 100 / 35 * IHEH


writeRaster(IHEH100,
            paste0(dir_Resultados, "/IHEH_", Año, ".tif"),
            overwrite = TRUE)
  
### Revisar resultado####
  
plot(IHEH100)
click(IHEH100)
plot(IHEH100, breaks = c(0, 15, 40, 60, 100))

plot(Ti_he)
plot(Pd_he)
plot(if_he)
plot(dr_he)
plot(ds_he)
activeCat(GTF) <- 10 #bi
plot(GTF)
activeCat(GTF) <- 9 #lu
plot(GTF)
activeCat(GTF) <- 11 #lu
plot(GTF)

  
# crear el raster de suma preliminar

m <- cbind(tablaK0[, 1], tablaK0[, 10])
Lu <- classify(GTF, m, other = NA)
Lu
plot(Lu)

m <- cbind(tablaK0[, 1], tablaK0[, 11])
bi <- classify(GTF, m, other = NA)
plot(bi)

plot(IHEH1)

