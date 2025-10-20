library(terra)

# Crear una secuencia de años
years <- 2016:2023

# Ruta base
ruta_base <- "~/GitHub/huella-humana-analisis/Datos/Mapbiomas/"

# Generar los nombres de archivo usando sprintf
nombres_archivos <- sprintf("mapbiomas_colombia_collection2_integration_v1-classification_%d.tif", years)

# Crear rutas completas
rutas_completas <- file.path(ruta_base, nombres_archivos)

# Cargar todos los rasters en una lista o stack
mapbiomas_list <- lapply(rutas_completas, rast)

names(mapbiomas_list) <- years

# Opcional: combinar todos los rasters en un solo objeto multilayer
mapbiomas_stack <- rast(mapbiomas_list)


##################################################################
# Crear una matriz de reclasificación
# Cada fila es: valor_original, valor_nuevo
reclass_mat <- matrix(c(
  3,  0,# natural
  5,  0,# natural
  6,  0,# natural
  49, 0,# natural
  11, 0,# natural
  12, 0,# natural
  32, 0,# natural
  33, 0,# natural
  34, 0,# natural
  27, 0, # es no observado , pero se encentra en lazona de los glaciares de SNSM
  29, 0,# natural
  50, 0,# natural
  13, 0,# natural
  23, 0,# natural
  68, 0,# natural
  35, 4,# agro
  21, 4,# agro
  25, 1,# No vegetal, # desnudo quema o degrdado
  9,  2,# plantacion forestal
  31, 3,# acuacultura # peso 2 pero 3 para diferenciar
  24, 5,# Infraestructura
  30, 5 # Minería
), ncol = 2, byrow = TRUE)

# reclasificar clase 123
reclass_mat_123 <- matrix(c(
  3,  3,# natural
  5,  5,# natural
  6,  6,# natural
  49, 49,# natural
  11, 11,# natural
  12, 123,# natural
  32, 32,# natural
  33, 33,# natural
  34, 34,# natural
  27, 27, # es no observado , pero se encentra en lazona de los glaciares de SNSM
  29, 29,# natural
  50, 50,# natural
  13, 123,# natural
  23, 23,# natural
  68, 68,# natural
  35, 35,# agro
  21, 21,# agro
  25, 25,# No vegetal, # desnudo quema o degrdado
  9,  9,# plantacion forestal
  31, 31,# acuacultura # peso 2 pero 3 para diferenciar
  24, 24,# Infraestructura
  30, 30 # Minería
), ncol = 2, byrow = TRUE)


reclass_mat123 <- matrix(c(
  11, 123,# natural
  12, 123# natural
), ncol = 2, byrow = TRUE)


# Aplicar la reclasificación
x <- mapbiomas_list[[1]]
year <- years[1]
# Clasificar y guardar con nombres que incluyan el año

mapbiomas_list <- mapply(function(x, year){
  print(year)
  rc <- classify(x, rcl = reclass_mat)
  print(year)
  map_col <- project(rc,"EPSG:9377", res=100)
  print(year)
  writeRaster(map_col,
              filename = paste0( "~/Descargas/Resultados/mapbiomas_pesos_", year, ".tif"),
              datatype = "INT1U",
              overwrite = TRUE)
  return(map_col)
}, mapbiomas_list, years, SIMPLIFY = FALSE)


# 123

mapbiomas_list <- mapply(function(x, year){
  print(year)
  map_p <- project(x,"EPSG:9377", res=100, method="near")
  rc123 <- classify(map_p, rcl = reclass_mat_123)
  rc <- classify(map_p, rcl = reclass_mat)
  print(year)
  writeRaster(rc123,
              filename = paste0("~/GitHub/huella-humana-analisis/Resultados/mapbiomas_rcl123_", year, ".tif"),
              datatype = "INT1U",
              overwrite = TRUE)
  writeRaster(rc,
              filename = paste0("~/GitHub/huella-humana-analisis/Resultados/mapbiomas_pesos_", year, ".tif"),
              datatype = "INT1U",
              overwrite = TRUE)
  }, mapbiomas_list[2:8], years[2:8], SIMPLIFY = FALSE)

unique(as.int(map_col))
unique(as.int(rc123))
freq(as.int(map_col))

# corine #########

# Cargar Corine
library(sf)
library(dplyr)
corine <-  st_read("~/GitHub/huella-humana-analisis/Datos/Corine/12 Guane/Guane 10K/12Guane10K.shp")

corine <- st_read("~/GitHub/huella-humana-analisis/Datos/Corine/11. Simití/Simití 10K/11PlataSimiti10k.shp")

corine <- st_read("~/GitHub/huella-humana-analisis/Datos/Corine/10. Becerril/Becerril 10K/10Becerril.shp")
corine <-  st_read("Datos/Corine/13.Cusiana/13Cusiana.shp")
#corine <-  st_read("Datos/Corine/15.Amazonía 2019-2/13Amazonia_2019_II.shp")
#corine <-  st_read("Datos/Corine/Mojana/Mojana_2013.shp")
#corine <-  st_read("Datos/Corine/Pisba/Pisba_2022.shp")
#corine <- st_read("Datos/Corine/Providencia/Providencia.shp")


# Clasificar Corine
corine_col_p <- st_transform(corine,"EPSG:9377")


# Suponiendo que tu tabla se llama `df`
corine_col <- corine_col_p %>%
  #dplyr::select(2,3,8:13) %>% 
  mutate( # Son los pesos para comparar con Map Biomas
    pesos_com_mb = case_when(
      startsWith(as.character(N3_COBERT), "1") ~ 500, # Artificial humana
      startsWith(as.character(N3_COBERT), "2") ~ 400, # agro agri
      N3_COBERT %in% c(311, 312, 314) ~ 0, # Bosque
      N3_COBERT == 313 ~ 200, # Bosque fragmentado
      N3_COBERT == 315 ~ 200,# Plantación forestal
      startsWith(as.character(N3_COBERT), "32") ~ 0, # herbazal, arbustivo o veg secundaria # no esta mal que este secundaria aca.
      N3_COBERT %in% c(331,332 ,335) ~ 0,# desnudo natural 
      N3_COBERT %in% c(333, 334) ~ 100, # desnudo quema o degradado
      startsWith(as.character(N3_COBERT), "4") ~ 0,  #humedo natural
      N3_COBERT %in% c(511, 512) ~ 0, #agua natural
      N3_COBERT %in% c(513, 514) ~ 500, #agua no natural
      N3_COBERT %in% c(521, 522) ~ 0,#agua natural
      N3_COBERT %in% c(523) ~ 300, # acuicultura marina, pero 3 para diferenciar, pero deberia ser 2
      TRUE ~ NA_real_  # Si no cumple ninguna condición
    )
  )%>%
  mutate( # Son pesos más detallados que permite la resolución temática
    pesos_det = case_when(
      startsWith(as.character(N3_COBERT), "1") ~ 500,# Artificial humana
      startsWith(as.character(N3_COBERT), "21") ~ 300, # Transitorio
      startsWith(as.character(N3_COBERT), "22") ~ 400,# Perenne
      startsWith(as.character(N3_COBERT), "23") ~ 300,# Pastos
      startsWith(as.character(N3_COBERT), "24") ~ 200,# Heterogéneo agrícola
      N3_COBERT %in% c(311, 312, 314) ~ 0, # Bosque
      N3_COBERT == 313 ~ 200, # Bosque fragmentado
      N3_COBERT == 315 ~ 200,# Plantación forestal
      startsWith(as.character(N3_COBERT), "323") ~ 100, # Vegetación secundaria
      grepl("^321|^322", as.character(N3_COBERT)) ~ 0, # Herbazales/ Arbustales
      N3_COBERT %in% c(331,332 ,335) ~ 0, # desnudo natural 
      N3_COBERT %in% c(333, 334) ~ 100, # desnudo quema o degrdado
      startsWith(as.character(N3_COBERT), "4") ~ 0, #humedo natural
      N3_COBERT %in% c(511, 512) ~ 0, #agua natural
      N3_COBERT %in% c(513, 514) ~ 500, #agua no natural
      N3_COBERT %in% c(521, 522) ~ 0,#agua natural
      N3_COBERT %in% c(523) ~ 300, # acuicultura marina, pero 3 para diferenciar, pero deberia ser 2
      
      
      TRUE ~ NA_real_  # Si no cumple ninguna condición
    ) 
  )%>%  # reclasisificacion a mapbiomas
  mutate( # Son los pesos para comparar con Map Biomas
    mapbio_cat = case_when(
      grepl("^1", as.character(N3_COBERT)) & !grepl("^131", as.character(N3_COBERT)) ~ 24,# infraestructura
      grepl("^131", as.character(N3_COBERT)) ~ 30,# mineria
      grepl("^2", as.character(N3_COBERT)) & !grepl("^2232", as.character(N4_COBERT)) ~ 21,# agro
      grepl("^2232", as.character(N4_COBERT)) ~ 35,# palma de aceite
      
      N3_COBERT %in% "315" ~ 9,# silvicultura
      N5_COBERT %in% c(31112, 31122, 31212, 31222) ~ 6, # Bosque inundable
      
      startsWith(as.character(N3_COBERT), "321") ~ 123,#  herbazal y natural no forestal
      startsWith(as.character(N3_COBERT), "322") ~ 3, # arbustivo o veg secundaria # no esta mal que este secundaria aca. ????? bosque
      N5_COBERT %in% c(32221) ~ 49, # leñosa sobre arena, vegetacion esclerofila
      N4_COBERT %in% c(3232) ~ 123, # no bosque herbazal
      N3_COBERT %in% c(331) ~ 23,# PLayas, dunas
      N3_COBERT %in% c(335) ~ 34, # glaciar
      N3_COBERT %in% c(332) ~ 29, # afloramiento rocoso
      N3_COBERT %in% c(333, 334) ~ 25, # desnudo quema o degradado
      startsWith(as.character(N3_COBERT), "3") ~ 3, # bosque
      
      N3_COBERT %in% c(422) ~ 32,  #salitral
      startsWith(as.character(N3_COBERT), "4") ~ 11,  #humedo natural
      startsWith(as.character(N3_COBERT), "523") ~ 31, # acuicultura
      startsWith(as.character(N3_COBERT), "5") ~ 33, # cuerpo de agua
      
      TRUE ~ NA_real_  # Si no cumple ninguna condición
    )
  ) %>% 
  mutate(
    mapbio_cat = case_when(N6_COBERT %in% c(311122) ~ 5, # manglar,
                           N5_COBERT %in% c(32112) ~ 11, # no Bosque inundable
                           N5_COBERT == 32121 ~ 50, # herbaceo en arena
                           TRUE ~ mapbio_cat  # Si no cumple ninguna condición
    )
    
  )


#revisar
u <- unique(st_drop_geometry(corine_col))
u <- unique(st_drop_geometry(corine_col[,c(2:8,12)]))
u
st_write(corine_col, "~/GitHub/huella-humana-analisis/Resultados/corine_reclass_Guane10k.shp")

st_write(corine_col, "~/GitHub/huella-humana-analisis/Resultados/corine_reclass_Simiti10k.shp")

st_write(corine_col, "~/GitHub/huella-humana-analisis/Resultados/corine_reclass_Berrecil10k.shp")

st_write(corine_col, "~/GitHub/huella-humana-analisis/Resultados/corine_reclass_Cusiana10k.shp")




# corine_AMAZONAs ##############
corine <-  st_read("Datos/Corine/15.Amazonía 2019-2/13Amazonia_2019_II.shp")
##corine <-  st_read("Datos/Corine/Mojana/Mojana_2013.shp")




# Clasificar Corine
corine_col_p <- st_transform(corine,"EPSG:9377")


# Suponiendo que tu tabla se llama `df`
corine_col <- corine_col_p %>%
  mutate(
    pesos_com_mb = case_when(
      grepl("^1", as.character(codigo)) ~ 500,  # Artificial humana
      grepl("^2", as.character(codigo)) ~ 400,  # Agro
      grepl("^311|^312|^314", as.character(codigo)) ~ 0,  # Bosques
      grepl("^313|^315", as.character(codigo)) ~ 200,  # Fragmentado y plantación
      grepl("^32", as.character(codigo)) ~ 0,  # Herbazales y arbustivos
      grepl("^331|^332|^335", as.character(codigo)) ~ 0,  # Desnudo natural
      grepl("^333|^334", as.character(codigo)) ~ 100,  # Desnudo degradado
      grepl("^4", as.character(codigo)) ~ 0,  # Humedales
      grepl("^511|^512", as.character(codigo)) ~ 0,  # Agua natural
      grepl("^513|^514", as.character(codigo)) ~ 500,  # Agua no natural
      grepl("^521|^522", as.character(codigo)) ~ 0,  # Agua natural
      grepl("^523", as.character(codigo)) ~ 300,  # Acuicultura marina. Código 3 pero es 2
      TRUE ~ NA_real_  # Si no cumple ninguna condición
    )
  ) %>%
  mutate(
    pesos_det = case_when(
      grepl("^1", as.character(codigo)) ~ 500,  # Artificial humana
      grepl("^21", as.character(codigo)) ~ 300,  # Agricultura extensiva
      grepl("^22", as.character(codigo)) ~ 400,  # Agricultura intensiva
      grepl("^23", as.character(codigo)) ~ 300,  # Pasto
      grepl("^24", as.character(codigo)) ~ 200,  # Agricultura mosaico
      grepl("^311|^312|^314", as.character(codigo)) ~ 0,  # Bosques
      grepl("^313|^315", as.character(codigo)) ~ 200,  # Fragmentado y plantación
      grepl("^323", as.character(codigo)) ~ 100,  # Vegetación secundaria
      grepl("^321|^322", as.character(codigo)) ~ 0,  # Arbustivo y herbazal
      grepl("^331|^332|^335", as.character(codigo)) ~ 0,  # Desnudo natural
      grepl("^333|^334", as.character(codigo)) ~ 100,  # Desnudo degradado
      grepl("^4", as.character(codigo)) ~ 0,  # Humedales
      grepl("^511|^512", as.character(codigo)) ~ 0,  # Agua natural
      grepl("^513|^514", as.character(codigo)) ~ 500,  # Agua no natural
      grepl("^521|^522", as.character(codigo)) ~ 0,  # Agua natural
      grepl("^523", as.character(codigo)) ~ 300,  # Acuicultura marina. Código 3 pero es 2
      TRUE ~ NA_real_  # Si no cumple ninguna condición
    )
  ) %>%
  mutate(
    mapbio_cat = case_when(
      grepl("^1", as.character(codigo)) & !grepl("^131", as.character(codigo)) ~ 24,  # Urbano y artificial
      grepl("^131", as.character(codigo)) ~ 30,  # Minería
      grepl("^2", as.character(codigo)) & !grepl("^2232", as.character(codigo)) ~ 21,  # Agropecuario
      grepl("^2232", as.character(codigo)) ~ 35,  # Palma de aceite
      grepl("^315", as.character(codigo)) ~ 9,  # Plantación forestal
      grepl("^31112|^31122|^31212|^31222", as.character(codigo)) ~ 6,  # Bosques Inundado
      grepl("^321", as.character(codigo)) ~ 123,  # Arbustivo Y natural no forestal
      grepl("^322", as.character(codigo)) ~ 3,  # arbustivo o veg secundaria # no esta mal que este secundaria aca. ????? bosque
      grepl("^32221", as.character(codigo)) ~ 49,  # leñosa sobre arena, vegetacion esclerofila
      grepl("^3232", as.character(codigo)) ~ 123,  # No bosque el bazal
      grepl("^331", as.character(codigo)) ~ 23,  # Arena, playas
      grepl("^335", as.character(codigo)) ~ 34,  # glaciar
      grepl("^332", as.character(codigo)) ~ 29,  # Afloramiento rocoso
      grepl("^333|^334", as.character(codigo)) ~ 25,  # Quema y degradado
      grepl("^3", as.character(codigo)) ~ 3,  # Bosque
      grepl("^422", as.character(codigo)) ~ 32,  # Salitral
      grepl("^4", as.character(codigo)) ~ 11,  # Humedales
      grepl("^523", as.character(codigo)) ~ 31,  # Acuicultura marina
      grepl("^5", as.character(codigo)) ~ 33,  # Agua
      TRUE ~ NA_real_  # Si no cumple ninguna condición
    )
  ) %>%
  mutate(
    mapbio_cat = case_when(
      grepl("^311122", as.character(codigo)) ~ 5,  # manglar
      grepl("^32112", as.character(codigo)) ~ 11,  # No bosque inundable
      grepl("^32121", as.character(codigo)) ~ 50,  # herbaceo en arena
      TRUE ~ mapbio_cat
    )
  )



#revisar
u <- unique(st_drop_geometry(corine_col))
u <- unique(st_drop_geometry(corine_col[,c(3,10:12)]))
u


st_write(corine_col, "~/GitHub/huella-humana-analisis/Resultados/corine_reclass_13Amazonia_2019_II10k.shp")

# providencia ########

##corine <-  st_read("Datos/Corine/Mojana/Mojana_2013.shp")

corine <- st_read("Datos/Corine/Providencia/Providencia.shp")
corine <-  st_read("Datos/Corine/Pisba/Pisba_2022.shp")


# Clasificar Corine

corine_col_p <- st_transform(corine,"EPSG:9377")

corine_col <- corine_col_p %>%
  mutate(
    pesos_com_mb = case_when(
      grepl("^1", as.character(CODIGO)) ~ 500,  # Artificial humana
      grepl("^2", as.character(CODIGO)) ~ 400,  # Agro
      grepl("^311|^312|^314", as.character(CODIGO)) ~ 0,  # Bosques
      grepl("^313|^315", as.character(CODIGO)) ~ 200,  # Fragmentado y plantación
      grepl("^32", as.character(CODIGO)) ~ 0,  # Herbazales y arbustivos
      grepl("^331|^332|^335", as.character(CODIGO)) ~ 0,  # Desnudo natural
      grepl("^333|^334", as.character(CODIGO)) ~ 100,  # Desnudo degradado
      grepl("^4", as.character(CODIGO)) ~ 0,  # Humedales
      grepl("^511|^512", as.character(CODIGO)) ~ 0,  # Agua natural
      grepl("^513|^514", as.character(CODIGO)) ~ 500,  # Agua no natural
      grepl("^521|^522", as.character(CODIGO)) ~ 0,  # Agua natural
      grepl("^523", as.character(CODIGO)) ~ 300,  # Acuicultura marina. Código 3 pero es 2
      TRUE ~ NA_real_  # Si no cumple ninguna condición
    )
  ) %>%
  mutate(
    pesos_det = case_when(
      grepl("^1", as.character(CODIGO)) ~ 500,  # Artificial humana
      grepl("^21", as.character(CODIGO)) ~ 300,  # Agricultura extensiva
      grepl("^22", as.character(CODIGO)) ~ 400,  # Agricultura intensiva
      grepl("^23", as.character(CODIGO)) ~ 300,  # Pasto
      grepl("^24", as.character(CODIGO)) ~ 200,  # Agricultura mosaico
      grepl("^311|^312|^314", as.character(CODIGO)) ~ 0,  # Bosques
      grepl("^313|^315", as.character(CODIGO)) ~ 200,  # Fragmentado y plantación
      grepl("^323", as.character(CODIGO)) ~ 100,  # Vegetación secundaria
      grepl("^321|^322", as.character(CODIGO)) ~ 0,  # Arbustivo y herbazal
      grepl("^331|^332|^335", as.character(CODIGO)) ~ 0,  # Desnudo natural
      grepl("^333|^334", as.character(CODIGO)) ~ 100,  # Desnudo degradado
      grepl("^4", as.character(CODIGO)) ~ 0,  # Humedales
      grepl("^511|^512", as.character(CODIGO)) ~ 0,  # Agua natural
      grepl("^513|^514", as.character(CODIGO)) ~ 500,  # Agua no natural
      grepl("^521|^522", as.character(CODIGO)) ~ 0,  # Agua natural
      grepl("^523", as.character(CODIGO)) ~ 300,  # Acuicultura marina. Código 3 pero es 2
      TRUE ~ NA_real_  # Si no cumple ninguna condición
    )
  ) %>%
  mutate(
    mapbio_cat = case_when(
      grepl("^1", as.character(CODIGO)) & !grepl("^131", as.character(CODIGO)) ~ 24,  # Urbano y artificial
      grepl("^131", as.character(CODIGO)) ~ 30,  # Minería
      grepl("^2", as.character(CODIGO)) & !grepl("^2232", as.character(CODIGO)) ~ 21,  # Agropecuario
      grepl("^2232", as.character(CODIGO)) ~ 35,  # Palma de aceite
      grepl("^315", as.character(CODIGO)) ~ 9,  # Plantación forestal
      grepl("^31112|^31122|^31212|^31222", as.character(CODIGO)) ~ 6,  # Bosques Inundado
      grepl("^321", as.character(CODIGO)) ~ 123,  # Arbustivo Y natural no forestal
      grepl("^322", as.character(CODIGO)) ~ 3,  # arbustivo o veg secundaria # no esta mal que este secundaria aca. ????? bosque
      grepl("^32221", as.character(CODIGO)) ~ 49,  # leñosa sobre arena, vegetacion esclerofila
      grepl("^3232", as.character(CODIGO)) ~ 123,  # No bosque el bazal
      grepl("^331", as.character(CODIGO)) ~ 23,  # Arena, playas
      grepl("^335", as.character(CODIGO)) ~ 34,  # glaciar
      grepl("^332", as.character(CODIGO)) ~ 29,  # Afloramiento rocoso
      grepl("^333|^334", as.character(CODIGO)) ~ 25,  # Quema y degradado
      grepl("^3", as.character(CODIGO)) ~ 3,  # Bosque
      grepl("^422", as.character(CODIGO)) ~ 32,  # Salitral
      grepl("^4", as.character(CODIGO)) ~ 11,  # Humedales
      grepl("^523", as.character(CODIGO)) ~ 31,  # Acuicultura marina
      grepl("^5", as.character(CODIGO)) ~ 33,  # Agua
      TRUE ~ NA_real_  # Si no cumple ninguna condición
    )
  ) %>%
  mutate(
    mapbio_cat = case_when(
      grepl("^311122", as.character(CODIGO)) ~ 5,  # manglar
      grepl("^32112", as.character(CODIGO)) ~ 11,  # No bosque inundable
      grepl("^32121", as.character(CODIGO)) ~ 50,  # herbaceo en arena
      TRUE ~ mapbio_cat
    )
  )



#revisar
u <- unique(st_drop_geometry(corine_col))
u <- unique(st_drop_geometry(corine_col[,c(8,19:21)]))
u

names(corine_col)

#st_write(corine_col, "~/GitHub/huella-humana-analisis/Resultados/corine_reclass_Mojana10k.shp")

st_write(corine_col, "~/GitHub/huella-humana-analisis/Resultados/corine_reclass_Providencia10k.shp")
st_write(corine_col, "~/GitHub/huella-humana-analisis/Resultados/corine_reclass_Pisba10k.shp", append = F)


# car####

# instalar si hace falta
if (!requireNamespace("sf", quietly = TRUE)) install.packages("sf")

library(sf)
library(dplyr)

gdb_path <- "Datos/Corine/CAR/COB_JurisdisccionCar_2017_25K.gdb/COB_JurisdisccionCar_2017_25K.gdb"

# Ver versiones (GDAL/PROJ) útiles para diagnosticar drivers
sf::sf_extSoftVersion()

# Listar todas las capas dentro del .gdb
st_layers(gdb_path)

# Leer una capa concreta (ej: "JurisdisccionCar")
# reemplaza "NombreDeLaCapa" por el nombre exacto que devolvió st_layers()
corine <- st_read(gdb_path, layer = "COB_JurisdisccionCar_2017_25K", quiet = FALSE)

# Vista rápida
print(capa)
plot(st_geometry(capa))

# Clasificar Corine
corine_col_p <- st_transform(corine,"EPSG:9377")


# Suponiendo que tu tabla se llama `df`
corine_col <- corine_col_p %>%
  mutate(
    pesos_com_mb = case_when(
      grepl("^1", as.character(Codigo)) ~ 500,  # Artificial humana
      grepl("^2", as.character(Codigo)) ~ 400,  # Agro
      grepl("^311|^312|^314", as.character(Codigo)) ~ 0,  # Bosques
      grepl("^313|^315", as.character(Codigo)) ~ 200,  # Fragmentado y plantación
      grepl("^32", as.character(Codigo)) ~ 0,  # Herbazales y arbustivos
      grepl("^331|^332|^335", as.character(Codigo)) ~ 0,  # Desnudo natural
      grepl("^333|^334", as.character(Codigo)) ~ 100,  # Desnudo degradado
      grepl("^4", as.character(Codigo)) ~ 0,  # Humedales
      grepl("^511|^512", as.character(Codigo)) ~ 0,  # Agua natural
      grepl("^513|^514", as.character(Codigo)) ~ 500,  # Agua no natural
      grepl("^521|^522", as.character(Codigo)) ~ 0,  # Agua natural
      grepl("^523", as.character(Codigo)) ~ 300,  # Acuicultura marina. Código 3 pero es 2
      TRUE ~ NA_real_  # Si no cumple ninguna condición
    )
  ) %>%
  mutate(
    pesos_det = case_when(
      grepl("^1", as.character(Codigo)) ~ 500,  # Artificial humana
      grepl("^21", as.character(Codigo)) ~ 300,  # Agricultura extensiva
      grepl("^22", as.character(Codigo)) ~ 400,  # Agricultura intensiva
      grepl("^23", as.character(Codigo)) ~ 300,  # Pasto
      grepl("^24", as.character(Codigo)) ~ 200,  # Agricultura mosaico
      grepl("^311|^312|^314", as.character(Codigo)) ~ 0,  # Bosques
      grepl("^313|^315", as.character(Codigo)) ~ 200,  # Fragmentado y plantación
      grepl("^323", as.character(Codigo)) ~ 100,  # Vegetación secundaria
      grepl("^321|^322", as.character(Codigo)) ~ 0,  # Arbustivo y herbazal
      grepl("^331|^332|^335", as.character(Codigo)) ~ 0,  # Desnudo natural
      grepl("^333|^334", as.character(Codigo)) ~ 100,  # Desnudo degradado
      grepl("^4", as.character(Codigo)) ~ 0,  # Humedales
      grepl("^511|^512", as.character(Codigo)) ~ 0,  # Agua natural
      grepl("^513|^514", as.character(Codigo)) ~ 500,  # Agua no natural
      grepl("^521|^522", as.character(Codigo)) ~ 0,  # Agua natural
      grepl("^523", as.character(Codigo)) ~ 300,  # Acuicultura marina. Código 3 pero es 2
      TRUE ~ NA_real_  # Si no cumple ninguna condición
    )
  ) %>%
  mutate(
    mapbio_cat = case_when(
      grepl("^1", as.character(Codigo)) & !grepl("^131", as.character(Codigo)) ~ 24,  # Urbano y artificial
      grepl("^131", as.character(Codigo)) ~ 30,  # Minería
      grepl("^2", as.character(Codigo)) & !grepl("^2232", as.character(Codigo)) ~ 21,  # Agropecuario
      grepl("^2232", as.character(Codigo)) ~ 35,  # Palma de aceite
      grepl("^315", as.character(Codigo)) ~ 9,  # Plantación forestal
      grepl("^31112|^31122|^31212|^31222", as.character(Codigo)) ~ 6,  # Bosques Inundado
      grepl("^321", as.character(Codigo)) ~ 123,  # Arbustivo Y natural no forestal
      grepl("^322", as.character(Codigo)) ~ 3,  # arbustivo o veg secundaria # no esta mal que este secundaria aca. ????? bosque
      grepl("^32221", as.character(Codigo)) ~ 49,  # leñosa sobre arena, vegetacion esclerofila
      grepl("^3232", as.character(Codigo)) ~ 123,  # No bosque el bazal
      grepl("^331", as.character(Codigo)) ~ 23,  # Arena, playas
      grepl("^335", as.character(Codigo)) ~ 34,  # glaciar
      grepl("^332", as.character(Codigo)) ~ 29,  # Afloramiento rocoso
      grepl("^333|^334", as.character(Codigo)) ~ 25,  # Quema y degradado
      grepl("^3", as.character(Codigo)) ~ 3,  # Bosque
      grepl("^422", as.character(Codigo)) ~ 32,  # Salitral
      grepl("^4", as.character(Codigo)) ~ 11,  # Humedales
      grepl("^523", as.character(Codigo)) ~ 31,  # Acuicultura marina
      grepl("^5", as.character(Codigo)) ~ 33,  # Agua
      TRUE ~ NA_real_  # Si no cumple ninguna condición
    )
  ) %>%
  mutate(
    mapbio_cat = case_when(
      grepl("^311122", as.character(Codigo)) ~ 5,  # manglar
      grepl("^32112", as.character(Codigo)) ~ 11,  # No bosque inundable
      grepl("^32121", as.character(Codigo)) ~ 50,  # herbaceo en arena
      TRUE ~ mapbio_cat
    )
  )


st_write(corine_col, "~/GitHub/huella-humana-analisis/Resultados/corine_reclass_car.shp")

# guajira####

# instalar si hace falta
if (!requireNamespace("sf", quietly = TRUE)) install.packages("sf")

library(sf)
library(dplyr)

gdb_path <- "C:/Users/alejandra.narvaez/Downloads/GDB_InterpretacionCobertura_ConectividadGEB.gdb/GDB_InterpretacionCobertura_ConectividadGEB.gdb"

# Ver versiones (GDAL/PROJ) útiles para diagnosticar drivers
sf::sf_extSoftVersion()

# Listar todas las capas dentro del .gdb
st_layers(gdb_path)

# Leer una capa concreta (ej: "JurisdisccionCar")
# reemplaza "NombreDeLaCapa" por el nombre exacto que devolvió st_layers()
corine <- st_read(gdb_path, layer = "CoberturaTierra", quiet = FALSE)

# Vista rápida
print(corine)
plot(st_geometry(capa))

# Clasificar Corine
corine_col_p <- st_transform(corine,"EPSG:9377")

corine_col <- corine_col_p %>%
  mutate(
    pesos_com_mb = case_when(
      grepl("^1", as.character(NOMENCLAT)) ~ 500,  # Artificial humana
      grepl("^2", as.character(NOMENCLAT)) ~ 400,  # Agro
      grepl("^311|^312|^314", as.character(NOMENCLAT)) ~ 0,  # Bosques
      grepl("^313|^315", as.character(NOMENCLAT)) ~ 200,  # Fragmentado y plantación
      grepl("^32", as.character(NOMENCLAT)) ~ 0,  # Herbazales y arbustivos
      grepl("^331|^332|^335", as.character(NOMENCLAT)) ~ 0,  # Desnudo natural
      grepl("^333|^334", as.character(NOMENCLAT)) ~ 100,  # Desnudo degradado
      grepl("^4", as.character(NOMENCLAT)) ~ 0,  # Humedales
      grepl("^511|^512", as.character(NOMENCLAT)) ~ 0,  # Agua natural
      grepl("^513|^514", as.character(NOMENCLAT)) ~ 500,  # Agua no natural
      grepl("^521|^522", as.character(NOMENCLAT)) ~ 0,  # Agua natural
      grepl("^523", as.character(NOMENCLAT)) ~ 300,  # Acuicultura marina. Código 3 pero es 2
      TRUE ~ NA_real_  # Si no cumple ninguna condición
    )
  ) %>%
  mutate(
    pesos_det = case_when(
      grepl("^1", as.character(NOMENCLAT)) ~ 500,  # Artificial humana
      grepl("^21", as.character(NOMENCLAT)) ~ 300,  # Agricultura extensiva
      grepl("^22", as.character(NOMENCLAT)) ~ 400,  # Agricultura intensiva
      grepl("^23", as.character(NOMENCLAT)) ~ 300,  # Pasto
      grepl("^24", as.character(NOMENCLAT)) ~ 200,  # Agricultura mosaico
      grepl("^311|^312|^314", as.character(NOMENCLAT)) ~ 0,  # Bosques
      grepl("^313|^315", as.character(NOMENCLAT)) ~ 200,  # Fragmentado y plantación
      grepl("^323", as.character(NOMENCLAT)) ~ 100,  # Vegetación secundaria
      grepl("^321|^322", as.character(NOMENCLAT)) ~ 0,  # Arbustivo y herbazal
      grepl("^331|^332|^335", as.character(NOMENCLAT)) ~ 0,  # Desnudo natural
      grepl("^333|^334", as.character(NOMENCLAT)) ~ 100,  # Desnudo degradado
      grepl("^4", as.character(NOMENCLAT)) ~ 0,  # Humedales
      grepl("^511|^512", as.character(NOMENCLAT)) ~ 0,  # Agua natural
      grepl("^513|^514", as.character(NOMENCLAT)) ~ 500,  # Agua no natural
      grepl("^521|^522", as.character(NOMENCLAT)) ~ 0,  # Agua natural
      grepl("^523", as.character(NOMENCLAT)) ~ 300,  # Acuicultura marina. Código 3 pero es 2
      TRUE ~ NA_real_  # Si no cumple ninguna condición
    )
  ) %>%
  mutate(
    mapbio_cat = case_when(
      grepl("^1", as.character(NOMENCLAT)) & !grepl("^131", as.character(NOMENCLAT)) ~ 24,  # Urbano y artificial
      grepl("^131", as.character(NOMENCLAT)) ~ 30,  # Minería
      grepl("^2", as.character(NOMENCLAT)) & !grepl("^2232", as.character(NOMENCLAT)) ~ 21,  # Agropecuario
      grepl("^2232", as.character(NOMENCLAT)) ~ 35,  # Palma de aceite
      grepl("^315", as.character(NOMENCLAT)) ~ 9,  # Plantación forestal
      grepl("^31112|^31122|^31212|^31222", as.character(NOMENCLAT)) ~ 6,  # Bosques Inundado
      grepl("^321", as.character(NOMENCLAT)) ~ 123,  # Arbustivo Y natural no forestal
      grepl("^322", as.character(NOMENCLAT)) ~ 3,  # arbustivo o veg secundaria # no esta mal que este secundaria aca. ????? bosque
      grepl("^32221", as.character(NOMENCLAT)) ~ 49,  # leñosa sobre arena, vegetacion esclerofila
      grepl("^3232", as.character(NOMENCLAT)) ~ 123,  # No bosque el bazal
      grepl("^331", as.character(NOMENCLAT)) ~ 23,  # Arena, playas
      grepl("^335", as.character(NOMENCLAT)) ~ 34,  # glaciar
      grepl("^332", as.character(NOMENCLAT)) ~ 29,  # Afloramiento rocoso
      grepl("^333|^334", as.character(NOMENCLAT)) ~ 25,  # Quema y degradado
      grepl("^3", as.character(NOMENCLAT)) ~ 3,  # Bosque
      grepl("^422", as.character(NOMENCLAT)) ~ 32,  # Salitral
      grepl("^4", as.character(NOMENCLAT)) ~ 11,  # Humedales
      grepl("^523", as.character(NOMENCLAT)) ~ 31,  # Acuicultura marina
      grepl("^5", as.character(NOMENCLAT)) ~ 33,  # Agua
      TRUE ~ NA_real_  # Si no cumple ninguna condición
    )
  ) %>%
  mutate(
    mapbio_cat = case_when(
      grepl("^311122", as.character(NOMENCLAT)) ~ 5,  # manglar
      grepl("^32112", as.character(NOMENCLAT)) ~ 11,  # No bosque inundable
      grepl("^32121", as.character(NOMENCLAT)) ~ 50,  # herbaceo en arena
      TRUE ~ mapbio_cat
    )
  )



#revisar
u <- unique(st_drop_geometry(corine_col))
u <- unique(st_drop_geometry(corine_col[,c(8,19:21)]))
u

names(corine_col)

#st_write(corine_col, "~/GitHub/huella-humana-analisis/Resultados/corine_reclass_Mojana10k.shp")

st_write(corine_col, "~/GitHub/huella-humana-analisis/Resultados/corine_reclass_Guajira10k.shp")


