

library(terra)
library(dplyr)
library (sf)

corine <- st_read("~/GitHub/HUELLA~1/Datos/Corine/COBERT~1/COBERT~1/SHAPEL~1/E_COBE~1.SHP")

corine <- st_read("~/GitHub/huella-humana-analisis/Datos/Corine/Cobertura_de_la_tierra_100K_Periodo_2018_limite_administrativo/e_cobertura_tierra_2018v2_amb.shp")
corine <- st_read("~/GitHub/huella-humana-analisis/Datos/Corine/Cobertura_tierra_100K_periodo_2022_limite_administrativo/Cobertura_tierra_100K_periodo_2022_limite_administrativo/ECOSISTEMAS_18062025/ECOSISTEMAS_18062025.gpkg")


mapbiomas <- rast("~/GitHub/huella-humana-analisis/Datos/Mapbiomas/mapbiomas_colombia_collection2_integration_v1-classification_2020.tif")


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

# Aplicar la reclasificación
map_p <- project(mapbiomas,"EPSG:9377", res=100, method="near")

map_col <- classify(map_p, rcl = reclass_mat)
# Si quieres mantener los demás valores tal cual, puedes usar `others = NA` o `others = "copy"` según lo que necesites

# Guardar o visualizar el resultado
plot(map_col)
# writeRaster(r_class, "ruta/a/raster_reclasificado.tif


writeRaster(map_col, "Resultados/mapbiomas_pesos_2020.tif", datatype="INT1U", overwrite=T)

##############################################################################

# Clasificar Corine
corine_col_p <- st_transform(corine,"EPSG:9377")


# Suponiendo que tu tabla se llama `df`
corine_col <- corine_col_p %>%
  dplyr::select(2,3,8:13) %>% 
  mutate( # Son los pesos para comparar con Map Biomas
    pesos_com_mb = case_when(
      startsWith(as.character(nivel_3), "1") ~ 500, # Artificial humana
      startsWith(as.character(nivel_3), "2") ~ 400, # agro agri
      nivel_3 %in% c(311, 312, 314) ~ 0, # Bosque
      nivel_3 == 313 ~ 200, # Bosque fragmentado
      nivel_3 == 315 ~ 200,# Plantación forestal
      startsWith(as.character(nivel_3), "32") ~ 0, # herbazal, arbustivo o veg secundaria # no esta mal que este secundaria aca.
      nivel_3 %in% c(331,332 ,335) ~ 0,# desnudo natural 
      nivel_3 %in% c(333, 334) ~ 100, # desnudo quema o degradado
      startsWith(as.character(nivel_3), "4") ~ 0,  #humedo natural
      nivel_3 %in% c(511, 512) ~ 0, #agua natural
      nivel_3 %in% c(513, 514) ~ 500, #agua no natural
      nivel_3 %in% c(521, 522) ~ 0,#agua natural
      nivel_3 %in% c(523) ~ 300, # acuicultura marina, pero 3 para diferenciar, pero deberia ser 2
      TRUE ~ NA_real_  # Si no cumple ninguna condición
    )
  )%>%
  mutate( # Son pesos más detallados que permite la resolución temática
    pesos_det = case_when(
      startsWith(as.character(nivel_3), "1") ~ 500,# Artificial humana
      startsWith(as.character(nivel_3), "21") ~ 300, # Transitorio
      startsWith(as.character(nivel_3), "22") ~ 400,# Perenne
      startsWith(as.character(nivel_3), "23") ~ 300,# Pastos
      startsWith(as.character(nivel_3), "24") ~ 200,# Heterogéneo agrícola
      nivel_3 %in% c(311, 312, 314) ~ 0, # Bosque
      nivel_3 == 313 ~ 200, # Bosque fragmentado
      nivel_3 == 315 ~ 200,# Plantación forestal
      startsWith(as.character(nivel_3), "323") ~ 100, # Vegetación secundaria
      grepl("^321|^322", as.character(nivel_3)) ~ 0, # Herbazales/ Arbustales
      nivel_3 %in% c(331,332 ,335) ~ 0, # desnudo natural 
      nivel_3 %in% c(333, 334) ~ 100, # desnudo quema o degrdado
      startsWith(as.character(nivel_3), "4") ~ 0, #humedo natural
      nivel_3 %in% c(511, 512) ~ 0, #agua natural
      nivel_3 %in% c(513, 514) ~ 500, #agua no natural
      nivel_3 %in% c(521, 522) ~ 0,#agua natural
      nivel_3 %in% c(523) ~ 300, # acuicultura marina, pero 3 para diferenciar, pero deberia ser 2
      
      
      TRUE ~ NA_real_  # Si no cumple ninguna condición
    ) 
    )%>%  # reclasisificacion a mapbiomas
      mutate( # Son los pesos para comparar con Map Biomas
        mapbio_cat = case_when(
          grepl("^1", as.character(nivel_3)) & !grepl("^131", as.character(nivel_3)) ~ 24,# infraestructura
          grepl("^131", as.character(nivel_3)) ~ 30,# mineria
          grepl("^2", as.character(nivel_3)) & !grepl("^2232", as.character(nivel_4)) ~ 21,# agro
          grepl("^2232", as.character(nivel_4)) ~ 35,# palma de aceite

          nivel_3 %in% "315" ~ 9,# silvicultura
          nivel_5 %in% c(31112, 31122, 31212, 31222) ~ 6, # Bosque inundable
          
          startsWith(as.character(nivel_3), "321") ~ 123,#  herbazal y natural no forestal
                    startsWith(as.character(nivel_3), "322") ~ 3, # arbustivo o veg secundaria # no esta mal que este secundaria aca. ????? bosque
          nivel_5 %in% c(32221) ~ 49, # leñosa sobre arena, vegetacion esclerofila
          nivel_4 %in% c(3232) ~ 123, # no bosque herbazal
          nivel_3 %in% c(331) ~ 23,# PLayas, dunas
          nivel_3 %in% c(335) ~ 34, # glaciar
          nivel_3 %in% c(332) ~ 29, # afloramiento rocoso
          nivel_3 %in% c(333, 334) ~ 25, # desnudo quema o degradado
          startsWith(as.character(nivel_3), "3") ~ 3, # bosque
          
          # startsWith(as.character(nivel_3), "3") ~ 3, # bosque
          # startsWith(as.character(nivel_3), "315") ~ 9,# silvicultura
          # nivel_5 %in% c(31112, 31122, 31212, 31222) ~ 6, # Bosque inundable
          # nivel_6 %in% c(311122) ~ 5, # manglar
          # startsWith(as.character(nivel_3), "321") ~ 123,#  herbazal y natural no forestal
          # nivel_5 %in% c(321121, 321122) ~ 11, # no Bosque inundable
          # nivel_5 == 32121 ~ 50, # herbaceo en arena
          # startsWith(as.character(nivel_3), "322") ~ 3, # arbustivo o veg secundaria # no esta mal que este secundaria aca. ????? bosque
          # nivel_5 %in% c(32221) ~ 49, # leñosa sobre arena, vegetacion esclerofila
          # nivel_4 %in% c(3232) ~ 13, # no bosque herbazal 
          # nivel_3 %in% c(331) ~ 23,# PLayas, dunas 
          # nivel_3 %in% c(335) ~ 34, # glaciar
          # nivel_3 %in% c(332) ~ 29, # afloramiento rocoso
          # nivel_3 %in% c(333, 334) ~ 25, # desnudo quema o degradado
          nivel_3 %in% c(422) ~ 32,  #salitral
           startsWith(as.character(nivel_3), "4") ~ 11,  #humedo natural
          startsWith(as.character(nivel_3), "523") ~ 31, # acuicultura
                    startsWith(as.character(nivel_3), "5") ~ 33, # cuerpo de agua
          
          TRUE ~ NA_real_  # Si no cumple ninguna condición
        )
      ) %>% 
  mutate(
    mapbio_cat = case_when(nivel_6 %in% c(311122) ~ 5, # manglar,
              nivel_5 %in% c(32112) ~ 11, # no Bosque inundable
              nivel_5 == 32121 ~ 50, # herbaceo en arena
              TRUE ~ mapbio_cat  # Si no cumple ninguna condición
              )
    
  )
  

#revisar
u <- unique(st_drop_geometry(corine_col[,c(2:8,10)]))
u <- unique(st_drop_geometry(corine_col[,c(2:8,12)]))


corine_col <- st_zm(corine_col, drop = TRUE)
st_write(corine_col, "~/GitHub/huella-humana-analisis/Resultados/corine_reclass_2022.shp")

# Rasterizar Corin usando mapiomas.

corine_col_spat <- vect(corine_col)

pc <- rasterize(corine_col_spat,map_col, field="pesos_com_mb")
pd <- rasterize(corine_col_spat,map_col, field="pesos_det")

# Comparar mapdiomas con corine

adicion <- pc+ map_col
adicion <- as.int(adicion)


# analisis

freq_pc <- freq(pc)
freq_map_col <- freq(map_col)

plot(adicion)
 unique(adicion) 

frecuencias <- freq(adicion)

 # confusion matrix
# Supongamos que tu tabla se llama df y tiene columnas 'value' y 'count'

# Extraer referencia y mapa
frecuencias$ref <- as.integer(substr(frecuencias$value, 1, 1))   # primer dígito
frecuencias$map <- as.integer(substr(frecuencias$value, 3, 3))   # tercer dígito

# Para los valores unitarios que están en las primeras 6 filas

frecuencias$ref[1:6] <- 0
frecuencias$map[1:6] <- frecuencias$value[1:6]


# Crear matriz de confusión
conf_matrix <- with(frecuencias, tapply(count, list(Referencia = ref, Mapa = map), sum, default = 0))

# Reemplazar NA por 0
conf_matrix[is.na(conf_matrix)] <- 0

# Mostrar matriz
print(round(conf_matrix))
print(round(conf_matrix/1000))



 # Nueva reclasificacion del raster

# Cargar tu raster (ejemplo)

# Reclasificar
valores_a_cero <- c(0, 101, 202, 303, 404, 505)

# Crear nueva capa reclasificada

r_matrix <- matrix(c(valores_a_cero, rep(0, length(valores_a_cero))), ncol = 2, byrow = FALSE)
unique(adicion)

r_reclas <- classify(adicion, 
                     rcl = r_matrix)  # Esta capa tendrá solo 0 donde corresponde y NA en los demás

frecuencias_rcl <- freq(r_reclas)

unique(r_reclas)

plot(r_reclas)
plot(as.factor(r_reclas))
# Guardar resultado si quieres

writeRaster(adicion, "Resultados/adicion.tif", datatype="INT2U", overwrite=T)
writeRaster(r_reclas, "Resultados/adicion_rcll.tif", datatype="INT2U", overwrite=T)
writeRaster(pc0, "Resultados/pc0.tif", datatype="INT2U", overwrite=T)
writeRaster(mapa_pesos, "Resultados/pc0.tif", datatype="INT2U", overwrite=T)

## intento directo ####
# Comparar mapdiomas con corine

pc0 <- pc/100
diff <- pc0- map_col

diff <- as.int(pc0)- as.int(map_col)
diff <- as.int(diff)

unique(pc0)

# analisis

plot(as.factor(diff))
unique(diff) 

frecuencia_diff <- freq(diff)

writeRaster(diff, "Resultados/diff2.tif", datatype="INT1S", overwrite=T)

