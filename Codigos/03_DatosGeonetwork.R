
# Título: IHEH stack para geonetwork
#
# Autor(es): Alejandra Narváez Vallejo
#
# Descripción: Este script organiza y prepara las capas de la Huella Espacial Humana (IHEH) y sus variables
# asociadas (uso del suelo, población, fragmentación y vías) para múltiples años. Para cada año, carga los
# raster, los agrupa en un stack multibanda, ajusta su estructura y los exporta en formato compatible con
# GeoNetwork para su visualización y distribución.

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

# - - - Sugerencia: aquí podrías aclarar que se trabaja con rutas relativas
setwd(file.path(this.path::this.path(), "..",".."))

dir_datos <- file.path("datos")
dir_Intermedios<- file.path ("Res_Intermedios")
dir_Resultados <- file.path("Resultados")


#**********************************************************
# Parámetros globales -----------------------
#**********************************************************

# - - - Sugerencia: indicar que estos son los años disponibles para procesamiento
años <- c(2018, 2020, 2022)

# - - - Sugerencia: aclarar que los nombres deben coincidir exactamente con los archivos
capas <- c(
  IHEH = "IHEH_IAVH1Cor",
  LU   = "LU1",
  Pop  = "Pop",
  frag = "frag",
  Vias = "ViasCor"
)

#**********************************************************
# Procesamiento -----------------------
#**********************************************************
# La siguiente línea se usa Si se quiere probar La iteración. De lo contrario ignorarla
año <- 2018

for (año in años) {
  
  message(">>> Procesando año: ", año)
  
  # 1. Leer rasters del año
  # - - - Sugerencia: aclarar que se construyen dinámicamente los nombres de archivo
  r_list <- lapply(capas, function(pref) {
    rast(paste0(dir_Resultados, "/", pref, año, ".tif"))
  })
  
  # 2. Crear stack raster multibanda
  # - - - Sugerencia: explicar que se agrupan todas las capas en un solo objeto
  r_stack <- rast(r_list)
  
  # 3. Asignar nombres a las bandas
  # - - - Sugerencia: importante para interpretación en GeoNetwork
  names(r_stack) <- c(
    "IHEH",
    "Uso_suelo",
    "Poblacion",
    "Fragmentacion",
    "Vias"
  )
  
  # 4. Exportar raster multibanda
  # - - - Sugerencia: aclarar que cada año se guarda como archivo independiente
  writeRaster(
    r_stack,
    filename = paste0(dir_Resultados, "/Geonetwork/IHEHc_", año, ".tif"),
    overwrite = TRUE
  )
}


# Gráfico vista previa ####

# fraficar los resultados de un año
plot(
  r_stack[[1]],
  col = hcl.colors(100, "RdYlGn", rev = TRUE),
  main = "2022",
  axes = FALSE,
  box = TRUE
)