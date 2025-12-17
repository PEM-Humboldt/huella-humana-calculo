
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
library(gpkg)


#**********************************************************
# Definir directorio(s) de trabajo -----------------------
#**********************************************************

setwd(file.path(this.path::this.path(), "..", "..",".."))

dir_datos <- file.path("datos")
dir_Intermedios<- file.path ("Res_Intermedios")
dir_Resultados <- file.path("Resultados")


############################################
#   STACK IHEH MULTIANUAL EN UN SOLO GPKG   #
############################################

#-------------------------------------------------
# CONFIGURACIÓN
#-------------------------------------------------

años <- c(2018, 2020, 2022)

# prefijos de archivos raster (sin año ni .tif)
capas <- c(
  IHEH = "IHEH_IAVH1",
  LU   = "LU1",
  Pop  = "Pop",
  frag = "frag",
  Vias = "Vias"
)


#-------------------------------------------------
# PROCESAMIENTO
#-------------------------------------------------
año <- 2018
for (año in años) {
  
  message(">>> Procesando año: ", año)
  
  # 1. Leer rasters del año
  r_list <- lapply(capas, function(pref) {
    rast(paste0(dir_Resultados, "/", pref, año, ".tif"))
  })

   # 3. Crear stack raster
  r_stack <- rast(r_list)
  
  # 4. Asignar nombres a las bandas
  names(r_stack) <- c(
    "IHEH",
    "Uso_suelo",
    "Poblacion",
    "Fragmentacion",
    "Vias"
  )
  
  # 5. Guardar stack como UNA capa dentro del mismo GPKG
  writeRaster(
    r_stack,
  filename=paste0(dir_Resultados, "/Geonetwork/IHEH_",año,".tif"),
    overwrite = TRUE
  )
}


# Gráfico vista previa ####

plot(
       r_stack[[1]],
       col = hcl.colors(100, "RdYlGn", rev=TRUE),
       main = "2022",
     axes=F, box = T
  )
