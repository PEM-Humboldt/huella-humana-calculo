# Título: IHEH version ecosistemas
#
# Autor(es): Alejandra Narváez Vallejo
#
# Descripción: En este código calcula la huella con el método de ecosistemas. Esta incluye los siguiientes cambios.
## - Variables continuas como no continuas
##    - Distancia a vias según Venter et al 2016. Inclusión de vía secundarias terciarias y caminos. Inclusión de vías férreas
##    - Población según Venter et al 2016
##    - Densidad de áreas naturales,los pesos relacionados con la presion humana disminuye exponencialmente con los mayores valores del índice
##    - Uso de la tierra: Según Correa et al. (2020) y Etter et al. (2011), 


# Por hacer o corregir: 



#**********************************************************
# librerías o dependencias --------------------------------
#**********************************************************

# lectura de datos
library (sf)
library(terra)
library(tidyverse)
library(raster)


#**********************************************************
# Definir directorio(s) de trabajo -----------------------
#**********************************************************

setwd(file.path(this.path::this.path(), "..", "..","..", ".."))

dir_datos <- file.path("datos")
dir_Intermedios<- file.path ("Res_Intermedios")
dir_Resultados <- file.path("Resultados")


#**********************************************************
# Cargar los datos necesarios ----------------------------
#**********************************************************

## Año #### 
# Escriba el año de interes
Año <- 2022

# Escriba el año de los datos de población que va a usar
Año_pop <- 2020

# Raster base de referencia
r_base <- rast(file.path(dir_datos, "r_base.tif"))          # Resolución 100 m

# Uso del suelo (LU)
#ya se calculó JLU0_corine...

#  Cambiar a O o J en animal  si oncilla o jaguar

animal <-  "J"
# LU
Lu_he <- rast(file.path(dir_Intermedios,paste0( animal,"LU0_corine",Año,".tif")))


# TNT
TNT <- rast(file.path(dir_Intermedios,paste0( animal, "TNT0_corine",Año,".tif")))

# Población
Pop0 <- rast(file.path(dir_Intermedios, paste0("pop_km2_", Año_pop, ".tif")))


# Vectores de infraestructura vial

vias8 <- file.path(dir_Intermedios, paste0 ("JOosm_IGAc8_",Año,".shp")) %>%
  st_read()
vias5 <- file.path(dir_Intermedios, paste0 ("JOosm_IGAc5_",Año,".shp")) %>%
  st_read()
vias4 <- file.path(dir_Intermedios, paste0 ("JOosm_IGAc4_",Año,".shp")) %>%
  st_read()
vias2 <- file.path(dir_Intermedios, paste0 ("JOosm_IGAc2_",Año,".shp")) %>%
  st_read()

# vias ferreas

V_ferreas4 <- rast(file.path(dir_Intermedios, paste0("pesos_trenes_", 4,"_",Año, ".tiff")))
V_ferreas6 <- rast(file.path(dir_Intermedios, paste0("pesos_trenes_", 6,"_",Año, ".tiff")))


#**********************************************************
# Cargar variables necesarias ----------------------------
#**********************************************************

# Necesita que se vuelvan a correr las proyecciones porque hubo un cambio antes? TRUE Si quiere volver a guardar los rasters a pesar que ya existan

#rerun <-  TRUE
rerun <-  FALSE

#**********************************************************
# Preparar datos ----------------------------
#**********************************************************

## Pop Pd_he  ####
#**********************************************************

# Definición de los pesos Basados En la densidad poblacional Según Venter 2016

plot(Pop0)
Pd_he <- 4.999 * log10(Pop0 + 1)  # Log-transformación
Pd_he[Pd_he > 10] <- 10           # Limitar a máximo 10

plot(Pd_he)


## vias- dr_he  ####
#**********************************************************
# Asignar los pesos A las carreteras y vías férreas

# Crear una lista con las capas de vías transformadas a la misma proyección de r_base, 
# luego rasterizar cada una sobre la cuadrícula base y calcular la distancia euclidiana desde cada celda
vias_groups <- lapply(list(vias2, vias4, vias5, vias8), function(x) {
  p <- st_transform(x, crs(r_base)) %>%
    rasterize(r_base) %>%
    terra::distance()
})

# Asignar nombres representativos a cada categoría de vía
names(vias_groups) <- c("v2", "v4", "v5", "v8")

# Curva de decaimiento basada en función exponencial ajustada desde literatura
# 2.426123*exp(-1*(seq(.5,15,0.1)-1)) parecido a el de un artículo

# Definir función de clasificación basada en distancia a vías
# 'max' define el valor máximo de influencia directa
clsDisVias <- function(x, max = 4) {
  max * exp(-0.33 * (x / 1000 - 0.5))
}

# Aplicar la función de clasificación a las vías de categorías 4, 5 y 8 (mayor impacto)
Vias_4R <- lapply(vias_groups[3:4], clsDisVias)

# Asignar valores máximos de influencia directa según el tipo de vía
Vias_4R$v5[Vias_4R$v5 > 4] <- 5 
Vias_4R$v8[Vias_4R$v8 > 4] <- 8 

# ajustar valores mínimos
Vias_4R$v5[Vias_4R$v5 < 0.034] <- 0 
Vias_4R$v8[Vias_4R$v8 < 0.034] <- 0 

# Aplicar la función de clasificación a la vía de categoría 2 (menor impacto)
Vias_2R <- lapply(vias_groups[1:2], clsDisVias, max = 2)

# Asignar el valor máximo de influencia directa para la vía de categoría 2 y 4
Vias_2R$v2[Vias_2R$v2 > 2] <- 2
Vias_2R$v4[Vias_2R$v4 > 2] <- 4


# ajustar valores mínimos
Vias_2R$v2[Vias_2R$v2 < 0.017] <- 0 
Vias_2R$v4[Vias_2R$v4 < 0.017] <- 0 

# Asignar ceros a los valores NA En las vías férreas

V_ferreas4[is.na(V_ferreas4)] <- 0
V_ferreas6[is.na(V_ferreas6)] <- 0



# Combinar todas las capas y calcular el valor máximo por celda entre las capas de influencia
dr_he <- app(c(Vias_2R$v2, Vias_2R$v4, Vias_4R$v5, Vias_4R$v8, V_ferreas4, V_ferreas6 ), max)
# Se hace la corrección para que a partir de 15 km el valor sea cero


# Visdr_he0# Visualización de las capas intermedias y resultado final

plot(Vias_4R$v8, main = "Influencia vías categoría 8")
plot(Vias_4R$v5, main = "Influencia vías categoría 5")
plot(Vias_2R$v4, main = "Influencia vías categoría 4")
plot(Vias_2R$v2, main = "Influencia vías categoría 2")
plot(V_ferreas4, main = "Vía férrea inactiva")
plot(V_ferreas6, main = "Vía férrea activa")
plot(dr_he, main = "Capa combinada de influencia vial (dr_he)")
hist(dr_he, main = "Histograma de influencia vial (dr_he)")

dr_he  # Resultado final


## B_per. POrcentaje de Bosque 1km2  ####
#**********************************************************
# Se calcula Un indicativo de la fragmentación basado en densidad de píxeles Naturales En un área específica De 1 km de radio y se le asignan los pesos The huella Basado en decaimiento exponencial
 
### jaguar ###############

vecindad <- focalMat(TNT, type = "circle", d = 1000)  # Ventana de 1 km

# Sumar área transformada en vecindad

densidad_0 <- focal(TNT,
                    w = vecindad,
                    fun = sum,
                    na.rm = TRUE) * 100
plot(densidad_0)


fuzzy_sigmoid <- function(x,
                                     base = 60,
                                     techo = 5,
                                     valor_base = 0.001,
                                     valor_techo = 0.999,
                          midpoint=30, 
                          suavidad=1){
  
  #midpoint <- (base + techo) / 2
  
  slope <- log((1 / valor_base) - 1) /
    (midpoint - base)
  slope <- slope / suavidad
  
  1 / (1 + exp(-slope * (x - midpoint)))
  
}

plot(Bperc <- fuzzy_sigmoid(densidad_0)*10)

# Se hace la corrección para que Si la cobertura es Completamente natural sea 0, NO se corrige la parte alta por que no llega a 10
Bperc[Bperc <= 0.01] <- 0

plot(Bperc)


### 500 M2 ONCILLA ####

vecindad <- focalMat(TNT, type = "circle", d = 500)  # Ventana de 500 km

# Sumar área transformada en vecindad

densidad_0 <- focal(TNT,
                    w = vecindad,
                    fun = sum,
                    na.rm = TRUE) * 100
plot(densidad_0)

plot(Bperc <- (1-fuzzy_sigmoid ( densidad_0,       
                base = 0,
                          techo = 85,
                          valor_base = 0.001,
                          valor_techo = 0.999,
                          midpoint=50, 
                          suavidad=1.5))*10)

# Se hace la corrección para que Si la cobertura es Completamente natural sea 0, NO se corrige la parte alta por que no llega a 10
Bperc[Bperc <= 0.01] <- 0

## Dist ríos  ####
#**********************************************************
rios_dist <- rast(file.path(dir_Intermedios,paste0( "JOriosdist",Año,".tif")))


pesos_rios_dist <- fuzzy_sigmoid (rios_dist,
                            base = 1000,
                            techo = 16000,
                            valor_base = 0.001,
                            valor_techo = 0.999, # si aplico suavidad . este parametro no importa
                            midpoint=7000,
                            suavidad=2)*10


# significado diatancias y pesos
# 1-- 3185
# 2 --- 4594
# 3 -- 5529
# 4 -- 6300

plot(pesos_rios_dist)


# Se hace la corrección para que Si la cobertura es Completamente natural sea 0.
pesos_rios_dist[pesos_rios_dist <= 0.01] <- 0

# Se corrige la parte alta com obarrera absoluta

#pesos_dem[pesos_dem >= 9.9] <- 99



## Elevacion  ####
#**********************************************************
dem <- rast(file.path(dir_datos,"elevacion", "Servicio-159/SRTM30/SRTM_30_Col1.tif"))

plot(dem)
p_dem<- project(dem, r_base)

### jaguar ####


pesos_dem <- fuzzy_sigmoid (p_dem,
                          base = 1000,
                          techo = 3000,
                          valor_base = 0.001,
                          valor_techo = 0.999,
                          midpoint=2000)*10
  
plot(pesos_dem)


# Se hace la corrección para que Si la cobertura es Completamente natural sea 0.
pesos_dem[pesos_dem <= 0.01] <- 0

# Se corrige la parte alta com obarrera absoluta

#pesos_dem[pesos_dem >= 9.9] <- 99

## elevacion oncilla ####

fuzzy_bell_altura <- function(x,
                              base_inf = 800,
                              opt_inf = 2000,
                              opt_sup = 3000,
                              base_sup = 4200,
                              valor_base = 0.01){
  
  midpoint_izq <- (base_inf + opt_inf) / 2
  
  slope_izq <- log((1 / valor_base) - 1) /
    (midpoint_izq - base_inf)
  
  subida <- 1 / (1 + exp(-slope_izq * (x - midpoint_izq)))
  
  
  midpoint_der <- (opt_sup + base_sup) / 2
  
  slope_der <- log((1 / valor_base) - 1) /
    (base_sup - midpoint_der)
  
  bajada <- 1 / (1 + exp(slope_der * (x - midpoint_der)))
  
  
  if(inherits(x, "SpatRaster")){
    
    membresia <- terra::ifel(
      subida < bajada,
      subida,
      bajada
    )
    
  } else {
    
    membresia <- pmin(subida, bajada)
    
  }
  
  return(membresia)
}

pesos_dem <- (1-fuzzy_bell_altura (p_dem))*10

plot(pesos_dem)

# Se hace la corrección para que Si la cobertura es Completamente natural sea 0.
pesos_dem[pesos_dem <= 0.01] <- 0

# Se corrige la parte alta com obarrera absoluta

#pesos_dem[pesos_dem >= 9.9] <- 99


## Pendiente  ####
#**********************************************************

pendiente <- terrain(p_dem, v = "slope", unit = "degrees")

pesos_pend <- fuzzy_sigmoid (pendiente,
                            base = 30, # oncilla 30, jaguar 15
                            techo = 80, # no importa el valor
                            valor_base = 0.001,
                            valor_techo = 0.999,
                            midpoint=55, # oncilla 55, jaguar 35
                            suavidad = 2)*10 

plot(pesos_pend)


# Se hace la corrección para que Si la cobertura es Completamente natural sea 0.No Se corrige la parte alta  llega a 10
pesos_pend[pesos_pend <= 0.01] <- 0

#pesos_pend[pesos_pend >= 9.98] <- 10


# Cálculo de Huella ####
#**********************************************************
#*
animal="J"
dr_he <- rast ( paste0(dir_Resultados, "/JOVias7ok", Año, ".tif"))
Lu_he <- rast(file.path(dir_Intermedios,paste0( animal,"LU0_corine",Año,".tif")))
Pd_he <- rast ( paste0(dir_Resultados, "/JOPop", Año, ".tif"))
Bperc <- rast ( paste0(dir_Resultados, "/JB_per", Año, ".tif"))
pesos_rios_dist <- rast ( paste0(dir_Resultados, "/Jrios", Año, ".tif"))
pesos_pend <- rast ( paste0(dir_Resultados, "/",animal, "pend", Año, ".tif"))
pesos_dem <- rast ( paste0(dir_Resultados, "/",animal, "dem", Año, ".tif"))

rioBarrera <- rast( file.path(dir_Intermedios,paste0( "OriosBarrera",Año,".tif")))


# JAguar
resitencia <- Lu_he + Pd_he + Bperc + dr_he +pesos_rios_dist + pesos_pend + pesos_dem
resitencia <- Lu_he*.3 + Pd_he*.2 + Bperc*.1 + dr_he*.2 +pesos_rios_dist*.05 + pesos_pend*.05 + pesos_dem*.1
resitencia <- Lu_he*.18 + Pd_he*.18 + Bperc*.13 + dr_he*.18 +pesos_rios_dist*.1 + pesos_pend*.05 + pesos_dem*.18

# Oncilla 

resitencia <- Lu_he + Pd_he + Bperc + dr_he + pesos_pend + pesos_dem + rioBarrera
resitencia <- Lu_he*.3 + Pd_he*.25 + Bperc*.1 + dr_he*.2 + pesos_pend*.05 + pesos_dem*.1 + rioBarrera
resitencia <- Lu_he*.2 + Pd_he*.2 + Bperc*.15 + dr_he*.2 + pesos_pend*.05 + pesos_dem*.2 + rioBarrera

### Revisar resultado####

plot(resitencia)

## Guardar resultado crs:9377####
writeRaster(
  resitencia,
  paste0(dir_Resultados, "/", animal,"resistencia_ponderada_A", Año, ".tif"), 
  overwrite=TRUE)

#Guardar capas intermedias


writeRaster(
  Pd_he,
  paste0(dir_Resultados, "/JOPop", Año, ".tif"), 
  overwrite=TRUE)
writeRaster(
  Bperc,
  paste0(dir_Resultados, "/OB_perC", Año, ".tif"), 
  overwrite=TRUE)
writeRaster(
  dr_he,
  paste0(dir_Resultados, "/JOVias7ok", Año, ".tif"), 
  overwrite=TRUE)

writeRaster(
  pesos_rios_dist,
  paste0(dir_Resultados, "/Jrios", Año, ".tif"), 
  overwrite=TRUE)

writeRaster(
  pesos_dem,
  paste0(dir_Resultados, "/Odem", Año, ".tif"), 
  overwrite=TRUE)

writeRaster(
  pesos_pend,
  paste0(dir_Resultados, "/Opend", Año, ".tif"), # Jpend
  overwrite=TRUE)


