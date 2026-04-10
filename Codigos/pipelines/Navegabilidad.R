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
#    - Están incluidos en el listado oficial de ríos navegables del documento “Superintendencia de Transporte. (2022). Caracterización Fluvial de Colombia 2022. Delegatura de Puertos.  https://visionamazonia.minambiente.gov.co/news/asi-es-el-plan-de-movilidad-sostenible-para-la-amazonia-la-iniciativa-fue-presentada-en-la-cop16/ 2022 - Superintendencia de Transporte” 
# 2. Adicionalmente, se filtran solo los tramos de los ríos que estén a una distancia igual o menor a 4 km de algún centro poblado.
# 3. Una vez identificados estos ríos navegables, se asigna un peso máximo de presión humana
#    igual a 4 en la orilla del río, que decae exponencialmente hasta 0 a los 15 km de distancia.
#
# La lógica del decaimiento se implementa con la fórmula:
#     peso = 4 * exp(-k * distancia_km)
# donde k se ajusta para que el valor tienda a 0 en 15 km.



#**********************************************************
# Carga y organizar de datos principales
#**********************************************************

# Cargar los datos de cobertura de acuerdo al tipo de dato de cobertura

if (base_cobertura == "MB") {
  
  cat("Se está generando la capa de infraestructura con base \nen Mapbiomas capa de 100 m de resolucion.\n")
  archivo_LU100cober <- file.path(dir_Intermedios,
                                  paste0("LU100cobertura_", base_cobertura, Año, ".tif"))

LU100 <- rast(archivo_LU100cober <- file.path(dir_Intermedios,
                                              paste0("LU100cobertura_", base_cobertura, Año, ".tif")))

LU100[!(LU100 %in% c(9,35,74,21,24, 25,30,75,31))] <- NA

} else if (base_cobertura == "corine") {
  cat("Se está generando la capa de infraestructura con base \nen la capa de Corine Landcover Colombia\n")
  
  corine_filter <- corine %>%
    filter(grepl("^1", as.character(codigo))) %>% st_transform(scoord) %>%  # incluyo red vial y territorios asociados
    mutate (infraestructura = 1)
  
  corine_filter <- st_cast(corine_filter, "MULTIPOLYGON")
  LU100 <- rasterize(corine_filter, r_base, field = "infraestructura")
  
  
} else{
  cat (" Definición de la capa de cobertura incorrecta\n")
}

# Cargar capas de ríos con drenaje simple y doble línea

rios_s <- st_read(file.path(dir_datos,"rios","Drenaje_Sencillo.shp"))
rios_d <- st_read(file.path(dir_datos,"rios","Drenaje_Doble.shp"))

# Elevacion digital
# Activar eventualmente si se requiere
# dem <- rast(file.path(dir_datos,"elevacion", "COL_msk_alt","COL_msk_alt.vrt")) 


#**********************************************************
# Preprocesamiento de datos
#**********************************************************
# Reproyección de capas 
rios_s <- st_transform(rios_s, scoord)
rios_d <- st_transform(rios_d, scoord)

# Activar eventualmente si se requiere
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

# Calcular distancia desde infraestructura
lu_dist <- distance(LU100)
lu_distlog <- lu_dist <= 4000


# Intersección de ríos navegables con áreas cercanas a población
lu_n_rnav <- lu_distlog + r_rios_nav_d + r_rios_nav_s

lu_n_rnav[lu_n_rnav %in% c(0,1,3,10, 13) ] <- NA  # Zonas sin coincidencia eliminadas

freq(lu_n_rnav)

#writeRaster(lu_n_rnav, file.path(dir_Intermedios, paste0("seleccion_filtrada.tiff")))

#**********************************************************
# Cálculo de distancias y pesos de presión
#**********************************************************
# Distancia desde ríos navegables seleccionados
lu_n_rnav_dis <- distance(lu_n_rnav)
plot(lu_n_rnav_dis)
# Calcular pesos con decaimiento exponencial
pesos_rnav <- 4 * exp(-0.33 * lu_n_rnav_dis / 1000) # Distancia convertida a km

pesos_rnav[pesos_rnav < 0.028] <- 0 

writeRaster(pesos_rnav, archivo_nav)

cat("Pesos navegabilidad finalizado.\n")

