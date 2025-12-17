
# Título: Preparación General de los insumos Huella 
#
# Autor(es): Alejandra Narváez Vallejo
#
# Descripción: En este código se preparan los insumos base para correr el IHEH. Dichos insumos no cambian comunmente, ya que son los que definen los parametros generales de la misma; proyección, extensión.
# Además se preparan:
## - Capas constantes con los parametros generales como: Ecosistemas potenciales y Tiempo de Intervención.
## - Tabla de consulta (de nombre "combinaciones") para la definición de clases (pesos) de biomasa, uso del suelo y IHEH1
#Esta tabla contiene la clasificación combinada de biomasa y uso del suelo, junto con los valores correspondientes del índice IHEH1, calculado como la suma de las huellas por biomasa y por uso del suelo. La tabla servirá como una tabla de consulta ("lookup table") en el cálculo posterior del índice de huella espacial humana. En ese proceso, se compararán los valores observados de biomasa y uso del suelo del año analizado con esta tabla para asignar automáticamente el valor de IHEH1 correspondiente.

#Este código se debe correr si cambia la proyección.En Caso tal se reescribirán los Raster base que son los moldes para la definición espacial de la huella humana y se reproyectarán también las capas de ecosistemas potenciales y tiempo de intervención. 
# En principio la tabla "combinaciones" no se ve alterada por las proyección. Este paso sólo es importante si se quiere hacer un cambio en la forma en que se le asignan los pesos a la cobertura del suelo o la presion definida por biomasa

# Para mayor información sobre el origen, la construcción de las capas y la asignación de los pesos consultar: https://docs.google.com/document/d/14dT_hxkIE3wAdL95E-zL7I29OZrjDU8_/edit

# Por hacer o  corregir: 

## - Es posible que vias cambien la forma en que los datos del IGAC dan el tipo. Tener en cuenta y cambiar cuando sea necesario.

# Observaciones ##################################################
# Se va a usar la misma que Para Corin por eso no la modificaré acá

#**********************************************************
# librerías o dependencias --------------------------------
#**********************************************************

# lectura de datos 

library (sf) 
library(terra)
library(dplyr)
library(tidyr)

#**********************************************************
# Definir directorio(s) de trabajo -----------------------
#**********************************************************
# Se define el directorio de trabajo relativo a la ubicación del script
setwd(file.path(this.path::this.path(), "..", "..",".."))

# Directorios para datos, resultados intermedios y finales
dir_datos <- file.path("Datos")
dir_Intermedios <- file.path("Res_Intermedios")
dir_Resultados <- file.path("Resultados")

#**********************************************************
# Cargar los datos necesarios -----------------------------
#**********************************************************

# Raster base para análisis
r_base <- rast(file.path(dir_datos, "r_base.tif"))

# Tablas con leyendas y ecosistemas potenciales
Leyenda_LU <- read.csv2(file.path(dir_datos, "Leyenda_LU.txt"))

# Funciones

# Función para verificar si el CRS coincide
crs_igual <- function(r, crs_ref) {
  tryCatch({
    crs(r) == crs_ref
  }, error = function(e) FALSE)
}

#**********************************************************
# Parametros globales ----------------------------
#**********************************************************
 
resolucion <-  100   # Resolución objetivo para el análisis
scoord <- crs("EPSG:9377") # Sistema de coordenadas del raster base. Cambiar cuando se defina la proyección

# Verificar si el sistema de coordenadas a usar coincide con el guardado, se puede definir coincidencia_crs a FALSE en caso que a pesar de la existencia del raster base se quiera reescribir
coincidencia_crs <- crs_igual(r_base, scoord)

#coincidencia_crs <- FALSE
#**********************************************************
# Preparar datos ----------------------------
#**********************************************************

##  raster base --------------------------------------------------
# En caso que se necesiten los productos en un sistema de referencia y diferente al preestablecido,esta sección modificará los raster base para ajustarlos a la nueva proyección

# Definir rutas de los archivos
archivo_r_base <- file.path(dir_datos, "r_base.tif")
archivo_r_base10 <- file.path(dir_datos, "r_base10.tif")


# Condición para crear o no los archivos
if (!file.exists(archivo_r_base) || 
    !file.exists(archivo_r_base10) || 
    !coincidencia_crs) {
  
  # Crear r_base proyectado
  r_base <- project(r_base, scoord, res = resolucion, method = "near")
  
  # Crear r_base10
  r_base10 <- disagg(r_base, fact = 10)
  
  # Guardar los archivos
  writeRaster(r_base, archivo_r_base, datatype = "INT1U", overwrite = TRUE)
  writeRaster(r_base10, archivo_r_base10, datatype = "INT1U", overwrite = TRUE)
  
} else {
  # Leer desde disco
  r_base <- rast(archivo_r_base)
  r_base10 <- rast(archivo_r_base10)
}










