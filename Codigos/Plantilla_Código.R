# Título: Soy una plantilla
#
# Autor(es): Nombres Apellidos
#
# Descripción: Esté código da una plantilla de código para trabajar
#
# Fuentes: 
#* IGAC_Depto.gpkg: Instituto geográfico Agustín Codazzi
#* pop00c.tif: SEDAC
#

# 
# Por hacer o  corregir: 

## - MOstrar las secciones : hecho
## - Errores de código : hecho
## - corregir estilo : xxxxxxx
## - función Hola no funciona : xxxxxxxxxxx
## - ...
## - ...


#**********************************************************
# librerías o dependencias --------------------------------
#**********************************************************

# lectura de datos 

library (sf) 
library(terra)


#**********************************************************
# Definir directorio(s) de trabajo -----------------------
#**********************************************************

# # Alternativa 1: Si se encuentra dentro de un projecto de RStudio
# 
# dir_vector<- here::here("Datos","Vector")
# dir_raster<- here::here("Datos","Raster")
# 
# dir_ouput<-  here::here("Resultados")
# 
# 
# # Alternativa 2: Si no tiene projecto de RStudio defina
# 
# setwd( "C:/Users/alejandra.narvaez/Documents/3_CódgosR/Estilo" ) # "ruta a la carpeta del proyecto"
# 
# dir_vector<- paste0("Datos/Vector")
# dir_raster<- paste0("Datos/Raster")

# Alternativa 3: Dentro de la definición de la plantilla propuesta puede usar

setwd(file.path(this.path::this.path(),"..",".."))

dir_vector<- file.path("Datos", "vector")
dir_raster<- file.path("Datos", "raster")
dir_Resultados<- file.path ("Resultados")


#**********************************************************
# Cargar los datos necesarios ----------------------------
#**********************************************************

# Capas Vector
v_adm<-st_read(file.path(dir_vector, "IGAC_Depto.gpkg"))

# Capas Raster
r_pop<-rast(file.path(dir_raster, "pop00c.tif")) 


#**********************************************************
# Parametros globales ----------------------------
#**********************************************************

scr <- "EPSG:4326"
r_base <- rast(ncols=1895, nrows= 16032, xmin=-80.16466, xmax=-65.77263, ymin=-4.181089, ymax=12.422 , crs=scr )

# umbral de población
umbral <- 5


#**********************************************************
# Preparar datos ----------------------------
#**********************************************************

##  completar datos Adm ####

v_adm$area <- st_area(v_adm)

plot(v_adm$geom)

## Reproyectar pop ####

r_pop<- project(r_pop, r_base)
plot(r_pop)


#**********************************************************
# Análisis ----------------------------
#**********************************************************


## Mascara de la región de estudio ####

r_pop_maskCol<- crop(r_pop, v_adm, mask=T)
plot(r_pop_maskCol)

## Mascara de población mayor a 200 ####

r_pop_cond<- r_pop_maskCol > umbral
plot(r_pop_cond)


# escribir resultados

writeRaster(r_pop_cond, file.path(dir_Resultados, "Mascara_Póblación.tif"))


