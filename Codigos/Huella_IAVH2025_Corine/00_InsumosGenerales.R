
# Título: Preparación General de los insumos Huella 
#
# Autor(es): Alejandra Narváez Vallejo
#
# Descripción: En este código se preparan los insumos base para correr el IHEH. Dichos insumos no cambian comunmente, ya que son los que definen los parametros generales de la misma; proyección, extensión.

#Este código se debe correr si cambia la proyección.En Caso tal se reescribirán los Raster base que son los moldes para la definición espacial de la huella humana.

# Por hacer o  corregir: 

## -...

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

# Verificar si el sistema de coordenadas a usar coincide con el guardado
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

