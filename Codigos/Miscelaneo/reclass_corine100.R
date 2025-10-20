

library(terra)
library(dplyr)
library (sf)

corine <- st_read("~/GitHub/huella-humana-analisis/Datos/Corine/Cobertura_de_la_tierra_100K_Periodo_2018_limite_administrativo/e_cobertura_tierra_2018v2_amb.shp")
corine <- st_read("~/GitHub/huella-humana-analisis/Datos/Corine/Cobertura_tierra_100K_periodo_2022_limite_administrativo/Cobertura_tierra_100K_periodo_2022_limite_administrativo/ECOSISTEMAS_18062025/ECOSISTEMAS_18062025.gpkg")

##############################################################################

# Clasificar Corine
corine_col_p <- st_transform(corine,"EPSG:9377")


# Suponiendo que tu tabla se llama `df`
corine_col <- corine_col_p %>%
  dplyr::select(1,2,3,8:13) %>% 
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
  mutate(
    mapbio_cat = case_when(
      grepl("^1", as.character(nivel_3)) & !grepl("^131", as.character(nivel_3)) ~ 24,# infraestructura
      grepl("^131", as.character(nivel_3)) ~ 30,# mineria
      grepl("^2", as.character(nivel_3)) & !grepl("^2232", as.character(nivel_4)) ~ 21,# agro
      grepl("^2232", as.character(nivel_4)) ~ 35,# palma de aceite
      
      nivel_3 %in% "315" ~ 9,# silvicultura
      nivel_5 %in% c(31112, 31122, 31212, 31222) ~ 6, # Bosque inundable
      
      startsWith(as.character(nivel_3), "321") ~ 123,#  herbazal y natural no forestal
      startsWith(as.character(nivel_3), "322") ~ 3, # arbustivo o veg secundaria # no esta mal que este secundaria aca. ????? bosque
      nivel_5 %in% c(32221) ~ 49, # leñosa sobre arena, vegetacion esclerofila # no esta en 2018
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
u <- unique(st_drop_geometry(corine_col[,c(1,11:13)]))
View(u)

#corine_col <- st_zm(corine_col, drop = TRUE)
st_write(corine_col, "~/GitHub/huella-humana-analisis/Resultados/corine_reclass_2018.shp")


###2022 ################################

corine <- st_read("~/GitHub/huella-humana-analisis/Datos/Corine/Cobertura_tierra_100K_periodo_2022_limite_administrativo/Cobertura_tierra_100K_periodo_2022_limite_administrativo/ECOSISTEMAS_18062025/ECOSISTEMAS_18062025.gpkg")

##############################################################################

# Clasificar Corine
corine_col_p <- st_transform(corine,"EPSG:9377")


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
u <- unique(st_drop_geometry(corine_col[,c(1,9:11)]))
u <- unique(st_drop_geometry(corine_col[,c(1,23:25)]))
View(u)

corine_col <- st_zm(corine_col, drop = TRUE)
st_write(corine_col, "~/GitHub/huella-humana-analisis/Resultados/corine_reclass_2022_c.shp")


