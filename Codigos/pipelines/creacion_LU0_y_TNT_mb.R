##  LU y TNT --------------------------------------------------
# 1.Creación de atributos para definir los pesos acorde a la cobertura según: 
# Etter, A., McAlpine, C. A., Seabrook, L., & Wilson, K. A. (2011). Incorporating temporality and biophysical vulnerability to quantify the human spatial footprint on ecosystems. Biological Conservation, 144(5), 1585–1594. https://doi.org/10.1016/j.biocon.2011.02.004 
# Correa Ayram, Camilo Andrés, Etter, A., Díaz-Timoté, J., Rodríguez Buriticá, S., Ramírez, W., & Corzo, G. (2020). Spatiotemporal evaluation of the human footprint in Colombia: Four decades of anthropic impact in highly biodiverse ecosystems. 

# 2. Rasterización  Para la creación de las capas LU (Pesos de huella de acuerdo con la cobertura) Y tnt(Transformado - no transformado)

# *******************************************************************************************

### Cargar los datos ####

#Crear URL
urlmb <- sprintf(
  "https://storage.googleapis.com/mapbiomas-public/initiatives/colombia/collection_3/coverage/colombia_coverage_%d.tif",
  Año
)

# Nombre del archivo final
destfile <- sprintf("mapbiomas_colombia_collection3_%d.tif", Año)

# Ruta completa de salida
ruta_salida <- file.path(dir_datos, "Mapbiomas", destfile)

# Crear carpeta si no existe
dir.create(file.path(dir_datos, "Mapbiomas"),
           showWarnings = TRUE,
           recursive = TRUE)

# Descargar solo si el archivo no existe
if (!file.exists(ruta_salida)) {
  message("Descargando archivo MapBiomas ", Año, "...")
  download.file(urlmb, ruta_salida, mode = "wb")
} else {
  message("El archivo ya existe, no se descarga: ", destfile)
}

# Cargar raster
mapbiomas <- rast(ruta_salida)

# Si el raster no existe, reproyectar y guardar

archivo_LU100cober <- file.path(dir_Intermedios,
                               paste0("LU100cobertura_", base_cobertura, Año, ".tif"))


# Condición para crear o no los archivos
if (!file.exists(archivo_LU100cober)) {
  
  LU100 <- mapbiomas %>%
    project(r_base, method = "near")
  
  # Guardar los archivos
  writeRaster(LU100,
              archivo_LU100cober,
              datatype = "INT1U",
              overwrite = TRUE)
  
} else {
  # Leer desde disco
  LU100 <- rast(archivo_LU100cober)
  cat ("LU100: Lu a 100 metros ya existe no se procesa")
  
}

# Reclasificar a pesos/ presiones humanas
# Crear una matriz de reclasificación

reclass_mat <- matrix(
  c(
    3,    0,    # natural
    5,    0,    # natural
    6,    0,    # natural
    49,   0,    # natural
    11,   0,    # natural
    12,   0,    # natural
    32,   0,    # natural
    33,   0,    # natural
    34,   0,    # natural
    27,   0,    # es no observado , pero se encentra en lazona de los glaciares de SNSM
    29,   0,    # natural
    50,   0,    # natural
    13,   0,    # natural
    81,   0,    # natural
    82,   0,    # natural
    23,   0,    # natural
    68,   0,    # natural
    35,   4,    # agro palma
    21,   3,    # agro y pasture
    74,   4,    # agro banano
    25,   1,    # No vegetal, # desnudo quema o degrdado
    75,   4.5,  # Parque solar
    9,    2,    # plantacion forestal
    31,   2,    # acuicultura
    24,   5,    # Infraestructura
    30,   5     # Minería
  ),
  ncol = 2,
  byrow = TRUE
)

# Relasificar a pesos de huella

LU0 <- classify(LU100, rcl = reclass_mat)


# Reclasificar a Transformado - no Transformado
# Crear una matriz de reclasificación

naturales <- c(3, 5, 6, 49, 11, 12, 32, 33, 34, 27, 29, 50, 13, 81, 82, 23, 68)

transformados <- c(35, 21, 74, 25, 75, 9, 31, 24, 30)

reclass01 <- rbind(cbind(naturales, 1), cbind(transformados, 0))

# Reclasificar el raster

TNT0 <- classify(LU100, rcl = reclass01)


# Guardar los archivos
writeRaster(LU0, archivo_LU0, datatype = "INT1U", overwrite = TRUE)
writeRaster(TNT0, archivo_TNT0, datatype = "INT1U", overwrite = TRUE)
