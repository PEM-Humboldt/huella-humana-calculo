##  LU y TNT --------------------------------------------------
# 1.Creación de atributos para definir los pesos acorde a la cobertura según: 
# Etter, A., McAlpine, C. A., Seabrook, L., & Wilson, K. A. (2011). Incorporating temporality and biophysical vulnerability to quantify the human spatial footprint on ecosystems. Biological Conservation, 144(5), 1585–1594. https://doi.org/10.1016/j.biocon.2011.02.004 
# Correa Ayram, Camilo Andrés, Etter, A., Díaz-Timoté, J., Rodríguez Buriticá, S., Ramírez, W., & Corzo, G. (2020). Spatiotemporal evaluation of the human footprint in Colombia: Four decades of anthropic impact in highly biodiverse ecosystems. 

# 2. Rasterización  Para la creación de las capas LU (Pesos de huella de acuerdo con la cobertura) Y tnt(Transformado - no transformado)


# Preprocesamiento y asignacion de pesos --------------------------------------------------

# proyectar a sistema de referencia  base
corine_col_p <- st_transform(corine, scoord)

# Reclasificar a pesos huella

### Crear atributo de peso huella Y transformando - No transformado ####

# Reproyectar y guardar

corine_col_p <- corine_col_p %>%
  #  select(1,7:12) %>%
  mutate(
    pesos_det = case_when(
      grepl("^1", as.character(!!sym(Cod_ecos))) ~ 5,      # Artificial humana
      grepl("^21", as.character(!!sym(Cod_ecos))) ~ 3,      # Agricultura extensiva
      grepl("^224", as.character(!!sym(Cod_ecos))) ~ 2,      # Agriforestal. como la condicion aparece primero que la siguiente. los valores no se reescriben.
      grepl("^22", as.character(!!sym(Cod_ecos))) ~ 4,      # Agricultura intensiva
      
      grepl("^23", as.character(!!sym(Cod_ecos))) ~ 3,      # Pasto
      #grepl("^24", as.character(!!sym(Cod_ecos))) ~ 2,  # Agricultura mosaico. En las siguientes dos líneas se desabregó
      grepl("^241|^242", as.character(!!sym(Cod_ecos))) ~ 3,      # Mosaico de cultivos Y pastos
      grepl("^243|^244|^245", as.character(!!sym(Cod_ecos))) ~ 2,      # Mosaico de cultivos, pastos y espacios naturales
      grepl("^311|^312|^314", as.character(!!sym(Cod_ecos))) ~ 0,      # Bosques
      grepl("^313|^315", as.character(!!sym(Cod_ecos))) ~ 2,      # Fragmentado y plantación
      grepl("^323", as.character(!!sym(Cod_ecos))) ~ 1,      # Vegetación secundaria
      grepl("^321|^322", as.character(!!sym(Cod_ecos))) ~ 0,      # Arbustivo y herbazal
      grepl("^331|^332|^335", as.character(!!sym(Cod_ecos))) ~ 0,      # Desnudo natural
      grepl("^333|^334", as.character(!!sym(Cod_ecos))) ~ 1,      # Desnudo degradado
      grepl("^4", as.character(!!sym(Cod_ecos))) ~ 0,      # Humedales
      grepl("^511|^512", as.character(!!sym(Cod_ecos))) ~ 0,      # Agua natural
      grepl("^513|^514", as.character(!!sym(Cod_ecos))) ~ 5,      # Agua no natural
      grepl("^521|^522", as.character(!!sym(Cod_ecos))) ~ 0,      # Agua natural
      grepl("^523", as.character(!!sym(Cod_ecos))) ~ 2,      # Acuicultura marina
      TRUE ~ NA_real_
    )
  ) %>%
  mutate(
    # 1:No Transformado, 0: Transformado
    TNT = case_when(
      
      pesos_det %in% c(2:5) ~ 0,# Transformado
      
      pesos_det %in% c(0) ~ 1, # Natural
      
      grepl("^323", as.character(!!sym(Cod_ecos))) ~ 1,# Vegetación secundaria
      grepl("^333|^334", as.character(!!sym(Cod_ecos))) ~ 0, # Desnudo degradado
      
      TRUE ~ NA_real_
    )
  )

### Rasterizar ####
corine_col_p <- st_cast(corine_col_p, "MULTIPOLYGON")

LU0 <- terra::rasterize(corine_col_p, r_base, field = "pesos_det")
TNT0 <- terra::rasterize(corine_col_p, r_base, field = "TNT")

# Guardar los archivos
writeRaster(LU0, archivo_LU0, datatype = "INT1U", overwrite = TRUE)
writeRaster(TNT0, archivo_TNT0, datatype = "INT1U", overwrite = TRUE)
