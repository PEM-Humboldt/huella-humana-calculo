#*#**********************************************************
# Título: Preparación de insumos para Huella Humana – Componente de Población
#
# Autor(es): Alejandra Narváez Vallejo
#
# Descripción:
# Este script calcula la presión antrópica asociada a la distribución de la
# población, como parte de la construcción del Índice de Huella Ecológica Humana (IHEH).
#
# El procedimiento se basa en:
# 1. Utilizar datos de población global del conjunto GHSL (Global Human Settlement Layer)
#    proporcionado por el Joint Research Centre (JRC) de la Comisión Europea, correspondientes
#    al año de análisis definido (variable `año_pop`).
#
# 2. Descargar y ensamblar las teselas (tiles) raster necesarias que cubren el territorio de interés,
#    generando un mosaico continuo de la distribución espacial de la población a resolución de 100 m.
#
# 3. Recortar el mosaico al área de estudio (extensión base) y reproyectarlo al sistema de referencia
#    espacial definido para el análisis, garantizando consistencia con las demás capas del modelo.
#
# 4. Convertir los valores de población a densidad por kilómetro cuadrado, multiplicando por el factor
#    correspondiente según la resolución espacial (100 m), para obtener una medida comparable de presión
#    antrópica en el territorio.
#
# 5. El resultado final es una capa raster continua que representa la intensidad de la presión humana
#    derivada de la concentración poblacional.
#**********************************************************

##  Población  --------------------------------------------------

### descargar datos  ####
# Definir la URL del archivo
#url <- "https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/GHSL/GHS_POP_GLOBE_R2023A/GHS_POP_E2020_GLOBE_R2023A_54009_100/V1-0/GHS_POP_E2020_GLOBE_R2023A_54009_100_V1_0.zip"
url2 <- paste0("https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/GHSL/GHS_POP_GLOBE_R2023A/GHS_POP_E",año_pop,"_GLOBE_R2023A_54009_100/V1-0/tiles/GHS_POP_E",año_pop,"_GLOBE_R2023A_54009_100_V1_0_R8_C11.zip")
url3 <-paste0( "https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/GHSL/GHS_POP_GLOBE_R2023A/GHS_POP_E",año_pop,"_GLOBE_R2023A_54009_100/V1-0/tiles/GHS_POP_E",año_pop,"_GLOBE_R2023A_54009_100_V1_0_R9_C11.zip")
url4 <- paste0 ("https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/GHSL/GHS_POP_GLOBE_R2023A/GHS_POP_E",año_pop,"_GLOBE_R2023A_54009_100/V1-0/tiles/GHS_POP_E",año_pop,"_GLOBE_R2023A_54009_100_V1_0_R10_C11.zip")
url5 <- paste0("https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/GHSL/GHS_POP_GLOBE_R2023A/GHS_POP_E",año_pop,"_GLOBE_R2023A_54009_100/V1-0/tiles/GHS_POP_E",año_pop,"_GLOBE_R2023A_54009_100_V1_0_R8_C12.zip")
url6 <- paste0("https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/GHSL/GHS_POP_GLOBE_R2023A/GHS_POP_E",año_pop,"_GLOBE_R2023A_54009_100/V1-0/tiles/GHS_POP_E",año_pop,"_GLOBE_R2023A_54009_100_V1_0_R9_C12.zip")
url1 <- paste0("https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/GHSL/GHS_POP_GLOBE_R2023A/GHS_POP_E",año_pop,"_GLOBE_R2023A_54009_100/V1-0/tiles/GHS_POP_E",año_pop,"_GLOBE_R2023A_54009_100_V1_0_R10_C12.zip")

# URLs en una lista
URLMos <- list(url1, url2, url3, url4, url5, url6)

# Definir el nombre del archivo descargado
destfile <- "GHS_POP_2023A.zip"

# Función para Descargar y descomprimir
PopDescarga <- function(url){
  # Descargar el archivo zip
  download.file(url, file.path(dir_datos, "Pop", destfile), mode = "wb")
  
  # Descomprimir el archivo
  unzip(file.path(dir_datos, "Pop", destfile), exdir = file.path(dir_datos, "Pop"))
}

#descargar todos los URL
lapply(URLMos, PopDescarga)

# Definir la ruta los archivo TIFF descomprimido
tiff_file <- list.files(file.path(dir_datos, "Pop"), pattern = "\\.tif$", full.names = TRUE)

# Leer capas y hacer el mosaico

Tiles <- lapply(tiff_file, rast)
pop00 <- merge(sprc(Tiles))

### reproyectar ####

# cortar pop ##
# preparar la extension para cortar y proyectarla

ext_projected <- ext(r_base) %>%
  project(from= crs(r_base) , to= crs(pop00))

# cortar
pop00c <- crop(pop00, ext_projected)

# proyectar a otra extensión 

pop00cp <- project(pop00c, r_base, method= "bilinear")

### cálculo a km2 ####

pop_km2 <- pop00cp*100

writeRaster(pop_km2, popkm2_path, overwrite=T)

gc()
cat("Generación de Capa de pesos de la poblacion terminado.\n")