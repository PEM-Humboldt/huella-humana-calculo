#**********************************************************
# Título: Preparación de insumos para Huella Humana – Componente Férreo
#
# Autor(es): Alejandra Narváez Vallejo
#
# Descripción:
# Este script calcula las presiones antrópicas potenciales asociadas a las vías férreas,
# como parte de la construcción del Índice de Huella Ecológica Humana (IHEH).
#
# El procedimiento se basa en los principios metodológicos de Venter et al. (2016) y en los
# pesos definidos por Wollmer (2008), siguiendo los pasos:
#
# 1. Diferenciar las vías férreas entre **activas** y **abandonadas/inactivas**.
# 2. Rasterizar la capa vectorial de vías sobre una grilla base con resolución de 100 m.
# 3. Calcular la distancia euclidiana desde cada celda del raster hacia la vía más cercana.
# 4. Asignar un peso de presión humana de acuerdo con el estado de la vía:
#       - Activa → peso máximo = 6
#       - Inactiva → peso máximo = 4
# 5. Los pesos se asignan únicamente hasta los **500 m** desde la vía, más allá de esa
#    distancia se considera presión nula.
#
# El resultado es un conjunto de rasters que representan la presión de las vías férreas,
# que luego se integrarán con otros factores (población, carreteras, ríos, etc.)
# para el cálculo total del IHEH.
#
#**********************************************************

#**********************************************************
# Librerías necesarias
#**********************************************************
library(sf)       # Manejo de datos vectoriales
library(terra)    # Manejo de datos raster
library(dplyr)    # Manipulación de datos tabulares

#**********************************************************
# Directorios de trabajo
#**********************************************************
setwd(file.path(this.path::this.path(),"..","..","..")) # Ruta relativa al script

dir_datos       <- file.path("Datos")            # Carpeta de insumos originales
dir_Intermedios <- file.path("Res_Intermedios")  # Carpeta de resultados intermedios
dir_Resultados  <- file.path("Resultados")       # Carpeta de resultados finales

#**********************************************************
# Parámetros globales
#**********************************************************
resolucion <- 100              # Resolución espacial del análisis (en metros)
scoord     <- crs("EPSG:9377") # Sistema de coordenadas oficial para Colombia
Año        <- 2022             # Año de referencia para el cálculo

#**********************************************************
# Carga de datos
#**********************************************************
# Capa de red férrea oficial (fuente: IGAC / ANI)
Vias <- st_read(file.path(dir_datos,"vias","ferreas","RedFerrea_actuali.shp"))

# Raster base de referencia (grilla con resolución de 100 m)
r_base <- rast(file.path(dir_datos, "r_base.tif"))

#**********************************************************
# Preprocesamiento de datos
#**********************************************************
# Reproyección de la capa de vías al sistema de coordenadas definido
Vias <- st_transform(Vias, scoord)

#**********************************************************
# Clasificación de vías férreas
#**********************************************************
# Crear una columna simplificada de funcionamiento
# Vias <- Vias %>% 
#   mutate(Funcionamiento = case_when(
#     ESTADO == "Activo (Privado)" ~ "Activo",
#     .default = ESTADO
#   ))



Vias <- Vias %>% 
  mutate(
    Construcci  = as.numeric(Construcci),
    Desactivac  = as.numeric(Desactivac),
    Activació   = as.numeric(Activació)
  ) %>% 
  mutate(
    Funcionamiento = case_when(
      # 1. Si existe Activació y es >= año → Activo
      !is.na(Activació) & Activació <= Año ~ "Activo",
      
      # 2. Si NO hay Activació, NO hay Desactivac y Construcci >= año → Activo
      is.na(Activació) & is.na(Desactivac) & Construcci <= Año ~ "Activo",
      
      # 3. Si NO hay Activació pero sí Desactivac 
      #    y Desactivac < año → Inactivo
      is.na(Activació) & !is.na(Desactivac) & Desactivac <= Año ~ "Inactivo"
      # ,
      # 
      # # 4. Todo lo demás → Inactivo (en vez de NA)
      # TRUE ~ "Inactivo"
    )
  )


# Dividir la capa en una lista según el funcionamiento (Activo/Inactivo)
Vias_ls <- split(Vias, Vias$Funcionamiento)

# Definir los pesos a asignar según funcionamiento
pesos_trenes <- c(6,4)  # Activas = 6, Inactivas = 4

#**********************************************************
# Cálculo de presiones
#**********************************************************
# Rasterizar cada grupo de vías, calcular distancias y asignar pesos
vias_pesos <- mapply(function(x, y) {
  p <- x %>%
    rasterize(r_base) %>%    # Rasterización sobre la grilla base
    terra::distance()        # Distancia euclidiana a la vía más cercana
  
  # Asignar peso solo hasta 500 m de la vía
  p_peso <- ifel(p <= 500, y, NA)
  
  # Guardar el resultado intermedio como raster GeoTIFF
  writeRaster(p_peso, file.path(dir_Intermedios, paste0("pesos_trenes_", y, ".tiff")), overwrite = TRUE)
  
  return(p_peso)
}, Vias_ls, pesos_trenes, SIMPLIFY = FALSE)

#**********************************************************
# Resultados
#**********************************************************
# La salida es una lista de rasters (`vias_pesos`), uno por categoría de funcionamiento.
# Cada raster representa la presión espacial ejercida por las vías férreas
# según su estado (activa o inactiva).
#**********************************************************
                                                                                                               