
# Título: Cálculo persitencia biotablero
#
# Autor(es): Alejandra Narváez Vallejo
#
# Descripción: 
# Este script calcula una capa de persistencia a partir de la comparación
# temporal de una capa de clasificación del IHEH entre dos años de referencia.
#
# El procesamiento permite seleccionar la capa base de cobertura utilizada
# (Corine o MapBiomas) y comparar los rasters correspondientes a los años
# definidos en el parámetro "años".
#
# La persistencia se identifica mediante la comparación píxel a píxel de
# ambas capas. Cuando el valor del raster del primer año y el del segundo
# año son diferentes, el píxel se reclasifica con el valor 6, indicando
# un cambio entre los dos periodos. Cuando no existe diferencia, se
# conserva el valor original de la capa correspondiente al primer año.
#
# El resultado corresponde a un raster que integra la información de la
# clasificación inicial y los cambios identificados entre los dos años.
# Finalmente, la capa resultante se exporta en formato GeoTIFF a la carpeta
# de resultados, con el nombre correspondiente al periodo analizado.
#
# Insumos:
# - Capas raster de clasificación del IHEH para los años definidos.
# - Capa base de cobertura: Corine o MapBiomas.
#
# Productos:
# - Raster de persistencia para el periodo analizado.
#
# Parámetros principales:
# - "años": años inicial y final utilizados para la comparación.
# - "base_cobertura": fuente de la capa de cobertura utilizada.

#**********************************************************
# librerías o dependencias --------------------------------
#**********************************************************

# lectura de datos espaciales y manipulación
library(sf)
library(terra)
library(tidyverse)
library(raster)
library(gpkg)


#**********************************************************
# Definir directorio(s) de trabajo -----------------------
#**********************************************************

setwd(file.path(this.path::this.path(), "..",".."))

dir_datos <- file.path("datos")
dir_Intermedios<- file.path ("Res_Intermedios")
dir_Resultados <- file.path("Resultados")


#**********************************************************
# Parámetros globales -----------------------
#**********************************************************

# Años a procesar ara procesamiento
años <- c(2018, 2024)

# Capa base de cobertura 
# Opciones: "corine" o "MB"
base_cobertura <- "corine" 

# Nombres de capas auxiliares
capas <- c(
  IHEH = paste0("IHEH_IAVH_classac_",base_cobertura)
)


#**********************************************************
# Procesamiento -----------------------
#**********************************************************

r_list <- lapply(capas, function(pref) {
  rast(paste0(dir_Resultados, "/", pref, "wgs",años, ".tif"))
})

# Crear un stack de los rasters contenidos en la lista
r_stack <- rast(r_list)

# Calcular la diferencia entre el primer y segundo raster
dif <- r_stack[[1]] - r_stack[[2]]

# Asignar el valor 6 donde exista una diferencia entre ambos rasters;
# donde no haya diferencia (dif = 0), conservar el valor del primer raster
resultado <- ifel(dif != 0, 6, r_stack[[1]])
plot(dif)

plot(resultado)
  
# Exportar raster 

writeRaster(
  resultado,
  filename = paste0(dir_Resultados, "/Biotablero/Persitencia_", años[1],"_", años[2], ".tif"),
  overwrite = TRUE
)
