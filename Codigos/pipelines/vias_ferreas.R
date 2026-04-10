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


#**********************************************************
# Preprocesamiento de datos
#**********************************************************
# Reproyección de la capa de vías al sistema de coordenadas definido
Vias <- st_transform(Vias, scoord)

#**********************************************************
# Clasificación de vías férreas
#**********************************************************

# Crear una columna simplificada de funcionamiento
# Tiene en cuenta las combinaciones De El año de construcción (Contrucci), Años de desactivación (Desactivac), y de activación después de desactivación (Activació)

Vias <- Vias %>%
  mutate(
    Construcci  = as.numeric(Construcci),
    Desactivac  = as.numeric(Desactivac),
    Activació   = as.numeric(Activació)
  ) %>%
  mutate(
    Funcionamiento = case_when(
      # 1. Si existe Activació y es <= año → Activo!is.na(Activació) &
      Activació <= Año ~ "Activo",
      
      # 2. Si NO hay Activació, NO hay Desactivac y Construcci <= año → Activo
      is.na(Activació) & is.na(Desactivac) & Construcci <= Año ~ "Activo",
      
      # 3. Si NO hay Activació pero sí Desactivac
      #    y Desactivac < año → Inactivo
      is.na(Activació) & !is.na(Desactivac) & Desactivac <= Año ~ "Inactivo",
      
      # 4. Si hay Activació  y Desactivac y la Fecha Desactivación Menor Que el año evaluado Y la de activación Mayor que el año evaluado
      #    y Desactivac < año → Inactivo!is.na(Activació) &
      !is.na(Desactivac) & Desactivac <= Año & Activació >= Año  ~ "Inactivo",
      
      # 5. Si hay Desactivac Y la fecha de desactivación Es mayor que el año evaluado y la de construccion menor!is.na(Desactivac) &
      Desactivac > Año & Construcci <= Año  ~ "Activo",
      
      #
      # # 4. Todo lo demás → Inactivo (en vez de NA) #
      .default = "Inactivo1"
    )
  )

# Revisar la calidad. No Deben haber vacías En la columna funcionamiento
View(Vias)
cat("⚠ Revisar la calidad. No Deben haber vacías En la columna Funcionamiento.\n")

repeat {
  resp <- tolower(readline("¿Deseas continuar? (s/n): "))
  if (resp %in% c("s", "n"))
    break
  cat("Respuesta no válida. Escribe 's' o 'n'.\n")
}

if (resp == "n") {
  stop("Proceso detenido por el usuario.")
}

cat("Continuando con el proceso...\n")


# Dividir la capa en una lista según el funcionamiento (Activo/Inactivo)
Vias_ls <- split(Vias, Vias$Funcionamiento)

# Definir los pesos a asignar según funcionamiento
pesos_trenes <- c(6, 4)  # Activas = 6, Inactivas = 4

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
  writeRaster(p_peso, file.path(
    dir_Intermedios,
    paste0("pesos_trenes_", y, "_", Año, ".tiff")
  ), overwrite = TRUE)
  
  return(p_peso)
}, Vias_ls, pesos_trenes, SIMPLIFY = FALSE)

cat("Generación de Capa de pesos de las vías férreas terminado.\n")



#**********************************************************
# Resultados
#**********************************************************
# La salida es una lista de rasters (`vias_pesos`), uno por categoría de funcionamiento.
# Cada raster representa la presión espacial ejercida por las vías férreas
# según su estado (activa o inactiva).
#**********************************************************
