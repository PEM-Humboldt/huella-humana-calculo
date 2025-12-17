
# Título: Preparación General de los insumos Huella 
#
# Autor(es): Alejandra Narváez Vallejo
#
# Descripción: En este código se preparan los insumos base para correr el IHEH. Dichos insumos no cambian comunmente, ya que son los que definen los parametros generales de la misma; proyección, extensión.

#Este código se debe correr si cambia la proyección.En Caso tal se reescribirán los Raster base que son los moldes para la definición espacial de la huella humana.

# es el mismo código "Codigos/Huella_IAVH2025_Corine/00_InsumosGenerales.R"

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


#**********************************************************
# Cargar los datos necesarios -----------------------------
#**********************************************************
# Se llama y corre el código 

source("Codigos/Huella_IAVH2025_Corine/00_InsumosGenerales.R")
