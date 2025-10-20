#*#**********************************************************
# Título: Preparación de insumos para Huella Humana – Componente de Navegabilidad de Ríos
#
# Autor(es): Alejandra Narváez Vallejo
#
# Descripción:
# Este script calcula las presiones antrópicas potenciales asociadas a la 
# navegabilidad de ríos, como parte de la construcción del Índice de Huella Ecológica Humana (IHEH).
# 
# El procedimiento se basa en:
# 1. Considerar como navegables los ríos que:
#    - Están incluidos en el listado oficial de ríos navegables del documento “XX” 
#      (referencia técnica), o
#    - Coinciden con la lista de nombres definida en el código.
# 2. Adicionalmente, se filtran solo los ríos que estén a una distancia igual o menor a 4 km
#    de algún centro poblado.
# 3. Una vez identificados estos ríos navegables, se asigna un peso máximo de presión humana 
#    igual a 4 en la orilla del río, que decae exponencialmente hasta 0 a los 15 km de distancia.
# 
# La lógica del decaimiento se implementa con la fórmula:
#     peso = 4 * exp(-k * distancia_km)
# donde k se ajusta para que el valor tienda a 0 a 15 km.
#
# Este componente se integrará posteriormente con otros factores (población, vías, etc.) 
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
resolucion <- 100             # Resolución espacial del análisis (en metros)
scoord     <- crs("EPSG:9377") # Sistema de coordenadas oficial para el análisis
Año        <- 2022            # Año de referencia para el cálculo

#**********************************************************
# Carga de datos
#**********************************************************
# Ríos con drenaje simple y doble línea
rios_s <- st_read(file.path(dir_datos,"rios","Drenaje_Sencillo.shp"))
rios_d <- st_read(file.path(dir_datos,"rios","Drenaje_Doble.shp"))

# Raster base para referencia espacial
r_base <- rast(file.path(dir_datos,"r_base.tif"))

# Cobertura de uso del suelo

LU0 <- rast(file.path(dir_datos,paste0( "LU0_",Año,".tif")))



# Elevacion digital

#dem <- rast(file.path(dir_datos,"elevacion", "COL_msk_alt","Datos/elevacion/COL_msk_alt/COL_msk_alt.gri" ,"COL_msk_alt.vrt"))


#**********************************************************
# Preprocesamiento de datos
#**********************************************************
# Reproyección de capas 
rios_s <- st_transform(rios_s, scoord)
rios_d <- st_transform(rios_d, scoord)

#dem <- project(dem,r_base"Datos/elevacion/COL_msk_alt/COL_msk_alt.gri""Datos/elevacion/COL_msk_alt/COL_msk_alt.gri")

#**********************************************************
# Selección de ríos navegables
#**********************************************************
# Lista de nombres de ríos considerados navegables
l_rios_nav <- c("Amazonas", "Putumayo", "Vaupés", "Meta", "Guaviare", "Atrato", "Sinú", "Baudó", "Jiguamiandó", "Chintadó", "Truandó", "Magdalena", "Esteros del Pacífico", "Iscuandé", "Guapi", "Patía", "Tapaje", "Canal del Dique", "Cauca", "Nechí", "Cesar", "San Jorge", "San Juan", "Orinoco", "Arauca", "Inírida", "Vichada", "Unilla", "Caquetá", "Tumaco", "Buenaventura", "Afluentes río Atrato", "Igara Paraná", "Brazos del río Magdalena", "Cagúan", "Catatumbo", "Mira", "Suarez", "Ranchería", "Apaporis", "Casanare",
                 "Baudo", "Sinu", "Caguan")

# Patrón para búsqueda parcial de nombres
patron <- paste(l_rios_nav, collapse = "|")
patron_extra <- "Río"

# Filtrar ríos dobles y sencillos por nombre

rios_nav_d <- rios_d[grepl(patron, rios_d$NOMBRE_GEO, ignore.case = TRUE) & 
                       grepl(patron_extra, rios_d$NOMBRE_GEO, ignore.case = TRUE), ] %>% 
  mutate(ID_rast = 3)

rios_nav_s <- rios_s[
  grepl(patron, rios_s$NOMBRE_GEO, ignore.case = TRUE) & 
    grepl(patron_extra, rios_s$NOMBRE_GEO, ignore.case = TRUE),
] %>% 
  mutate(ID_rast = 10)

# Convertir ríos filtrados a raster
r_rios_nav_d <- rasterize(rios_nav_d, r_base, field = "ID_rast", background = 0)
r_rios_nav_s <- rasterize(rios_nav_s, r_base, field = "ID_rast", background = 0)

#**********************************************************
# Filtrado por cercanía a centros poblados (4 km)
#**********************************************************
# Mantener solo valores de infraestructura (valor 5 en Mapbiomas)
LU0[LU0 != 24] <- NA

# Calcular distancia desde infraestructura
lu_dist <- distance(LU0)
lu_dist[lu_dist > 4000]  <- NA  # Más de 4 km = no relevante
lu_dist[lu_dist <= 4000] <- 1   # Menos o igual a 4 km = zona de influencia

# Intersección de ríos navegables con áreas cercanas a población
lu_n_rnav <- lu_dist + r_rios_nav_d + r_rios_nav_s
lu_n_rnav[lu_n_rnav == 1] <- NA  # Zonas sin coincidencia eliminadas

#**********************************************************
# Cálculo de distancias y pesos de presión
#**********************************************************
# Distancia desde ríos navegables seleccionados
lu_n_rnav_dis <- distance(lu_n_rnav)

# Calcular pesos con decaimiento exponencial
pesos_rnav <- 4 * exp(-0.33 * lu_n_rnav_dis / 1000) # Distancia convertida a km
pesos_rnav[pesos_rnav > 15] <- 0 # Asignar peso a valores extremos


save(pesos_rnav, file.path(dir_Intermedios, paste0("pesos_navegabilidad.Rdata")))

writeRaster(pesos_rnav, file.path(dir_Intermedios, paste0("pesos_navegabilidad1.tiff")))
