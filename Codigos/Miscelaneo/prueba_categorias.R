# Título: Análisis de categorías
#
# Autor(es): Alejandra Narváez Vallejo
#
# Descripción: En este código se Re ¿Que relación tienen los valores de la huella con Sus componentes Para poder definir los rangos de las categorías discretas de la misma.
# La huella como tal es la sumatoria de cu Variables Pujolor máximo es de 38 Ésta se escala de cero a 100. 
# Por esta razón el primer paso es analizar qué valor de la huella original,0-38, corresponde a la huella reescala 0_100.


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

setwd(file.path(this.path::this.path(), "..", "..",".."))

dir_datos <- file.path("datos")
dir_Intermedios<- file.path ("Res_Intermedios")
dir_Resultados <- file.path("Resultados")


#**********************************************************
# Cargar los datos necesarios ----------------------------
#**********************************************************

## Año #### 
# Escriba el año de interes
Año <- 2020

IHEH1002 <- rast( paste0(dir_Resultados, "/IHEH_IAVH1", Año, ".tif"))
Lu_he <-rast( paste0(dir_Resultados, "/LU1", Año, ".tif"))
Pd_he <- rast( paste0(dir_Resultados, "/Pop", Año, ".tif"))
if_he <- rast( paste0(dir_Resultados, "/frag", Año, ".tif"))
dr_he <- rast( paste0(dir_Resultados, "/Vias", Año, ".tif"))

#**********************************************************
# Revisar rangos  de huella ----------------------------
#**********************************************************

### Revisar resultados####

plot(IHEH1002)
#click(IHEH100)

colores <- c("blue","darkgreen","yellow","orange","red" )
plot(IHEH1002, breaks = c(0, 15, 40, 60, 100),col=c("blue","yellow","orange","red" ))# CORREA
plot(IHEH1002, breaks = c(0, 1,15,  30, 60,100),col=colores)# P1
plot(IHEH1002, breaks = c(0, 1,15,  30, 50,100),col=colores)#P2
plot(IHEH1002, breaks = c(0,1, 15, 60, 100),col=c("blue","yellow","orange","red" ))# TAPIA

plot(Pd_he)
plot(if_he)
plot(dr_he)
plot(Lu_he)

# prueba en un rango definido ####
mh <- IHEH1002>15 & IHEH1002<60

mh[mh==0] <- NA
gc()
ph1560 <- as.points(mh)

stack_IHEH <-  c(Lu_he , Pd_he , if_he , dr_he, IHEH1002)


muestra <- sample(1:length(ph1560), size = 2000, replace = FALSE)

ph1560_m <- st_as_sf(ph1560)[muestra,] 


hh <- extract(stack_IHEH, ph1560_m)
names(hh) <- c("ID","LU","Pop", "Frag", "Vías","IHEH")
save(hh, file=file.path(dir_Resultados,"15_60_data_2022.Rdata"))

par(mfrow=c(2,2))
boxplot(hh[hh$IHEH > 15 & hh$IHEH <30,c(-1, -6)], main= "15_30")
boxplot(hh[hh$IHEH >= 30 & hh$IHEH <60,c(-1, -6)], main= "30_60")
boxplot(hh[hh$IHEH > 15 & hh$IHEH <40,c(-1, -6)], main= "15_40")
boxplot(hh[hh$IHEH >= 40 & hh$IHEH <60,c(-1, -6)], main= "40_60")
boxplot(hh[hh$IHEH > 15 & hh$IHEH <30,c(-1, -6)], main= "15_30")
boxplot(hh[hh$IHEH >= 30 & hh$IHEH <50,c(-1, -6)], main= "30_50")
boxplot(hh[hh$IHEH >= 50 & hh$IHEH <60,c(-1, -6)], main= "50_60")


# prueba completa_mejor_rapido####

# Generar puntos aleatorios dentro de la extensión del raster
pts <- spatSample(IHEH1002, size = 30000, method = "random", as.points = TRUE)

col <- st_read("Datos/MGN2023_DPTO_POLITICO/MGN_ADM_DPTO_POLITICO.shp") %>% 
  st_transform(9377)

# Elegir solo los puntos dentro del territorio nacional

pts_sel <-st_as_sf(pts)[col,]


# Visualizar
plot(IHEH1002)
points(pts_sel, col = "red", pch = 20, cex = 0.6)


stack_IHEH <-  c(Lu_he , Pd_he , if_he , dr_he, IHEH1002)

# Extraer los puntos Del Stack
hh_all <- extract(stack_IHEH, pts_sel)
names(hh_all) <- c("ID","LU","Pop", "Frag", "Vías","IHEH")
save(hh_all, file=file.path(dir_Resultados,"muestra_IHEH_all_2022.Rdata"))
load(file.path(dir_Resultados,"muestra_IHEH_all_2022.Rdata"))

# Graficar las posibilidades en el histograma

par(mfrow=c(1,5))
boxplot(hh_all[hh_all$IHEH ==0,c(-1, -6)], main= "0",ylim = c(0, 10))
boxplot(hh_all[hh_all$IHEH >0 & hh_all$IHEH <15,c(-1, -6)], main= "1_15",ylim = c(0, 10))
boxplot(hh_all[hh_all$IHEH > 15 & hh_all$IHEH <30,c(-1, -6)], main= "15_30",ylim = c(0, 10))
boxplot(hh_all[hh_all$IHEH >= 30 & hh_all$IHEH <60,c(-1, -6)], main= "30_60",ylim = c(0, 10))
boxplot(hh_all[hh_all$IHEH >= 60 & hh_all$IHEH <=100,c(-1, -6)], main= "60_100",ylim = c(0, 10))

boxplot(hh_all[hh_all$IHEH ==0,c(-1, -6)], main= "0",ylim = c(0, 10))
boxplot(hh_all[hh_all$IHEH >0 & hh_all$IHEH <15,c(-1, -6)], main= "1_15",ylim = c(0, 10))
boxplot(hh_all[hh_all$IHEH > 15 & hh_all$IHEH <25,c(-1, -6)], main= "15_25",ylim = c(0, 10))
boxplot(hh_all[hh_all$IHEH >= 25 & hh_all$IHEH <60,c(-1, -6)], main= "25_60",ylim = c(0, 10))
boxplot(hh_all[hh_all$IHEH >= 25 & hh_all$IHEH <50,c(-1, -6)], main= "25_50",ylim = c(0, 10))


boxplot(hh_all[hh_all$IHEH ==0 ,c(-1, -6)], main= "0",ylim = c(0, 10))
boxplot(hh_all[hh_all$IHEH >0 & hh_all$IHEH <15,c(-1, -6)], main= "1_15",ylim = c(0, 10))
boxplot(hh_all[hh_all$IHEH > 15 & hh_all$IHEH <40,c(-1, -6)], main= "15_40",ylim = c(0, 10))
boxplot(hh_all[hh_all$IHEH >= 40 & hh_all$IHEH <60,c(-1, -6)], main= "40_60",ylim = c(0, 10))
boxplot(hh_all[hh_all$IHEH >= 60 & hh_all$IHEH <=100,c(-1, -6)], main= "60_100",ylim = c(0, 10))

boxplot(hh_all[hh_all$IHEH ==0,c(-1, -6)], main= "0",ylim = c(0, 10))
boxplot(hh_all[hh_all$IHEH >0 & hh_all$IHEH <15,c(-1, -6)], main= "1_15",ylim = c(0, 10))
boxplot(hh_all[hh_all$IHEH > 15 & hh_all$IHEH <30,c(-1, -6)], main= "15_30",ylim = c(0, 10))
boxplot(hh_all[hh_all$IHEH >= 30 & hh_all$IHEH <50,c(-1, -6)], main= "30_50",ylim = c(0, 10))
boxplot(hh_all[hh_all$IHEH >= 50 & hh_all$IHEH <=100,c(-1, -6)], main= "50_100",ylim = c(0, 10))


boxplot(hh_all[hh_all$IHEH >=0 & hh_all$IHEH <15,c(-1, -6)], main= "0_15",ylim = c(0, 10))
boxplot(hh_all[hh_all$IHEH >=15 & hh_all$IHEH <40,c(-1, -6)], main= "15_40",ylim = c(0, 10))
boxplot(hh_all[hh_all$IHEH > 40 & hh_all$IHEH <60,c(-1, -6)], main= "40_60",ylim = c(0, 10))
boxplot(hh_all[hh_all$IHEH >= 60 & hh_all$IHEH <=100,c(-1, -6)], main= "60_100",ylim = c(0, 10))



boxplot(hh_all[hh_all$IHEH > 15 & hh_all$IHEH <30,c(-1, -6)], main= "15_30",ylim = c(0, 10))
boxplot(hh_all[hh_all$IHEH >= 50 & hh_all$IHEH <60,c(-1, -6)], main= "30_50",ylim = c(0, 10))



breaks = c(0,1, 15,  60, 100)# tapia armijos 2017. eme gusta
breaks/100*38

labels <- c("Natural", "Baja", "Media", "Alta")


data.frame(IHEH_100=seq(0,100,5),
           IHEH_original=seq(0,100,5)/100*38
           )

par(
  mfrow = c(1, 5))
  
  

par(mar = c(4, 3, 2, .5),  # izquierda = 4
  mgp = c(2.0, 0.8, 0),   # ↓ acerca la etiqueta al eje
  cex.lab = 1.4
  )
boxplot(
  hh_all[hh_all$IHEH == 0, c(-1, -6)],
  main = "0",
  ylim = c(0, 10),
  ylab = "Pesos de Impacto"
 
)

par(mar = c(4, 0.5, 2, 0.1))

boxplot(
  hh_all[hh_all$IHEH > 0 & hh_all$IHEH < 15, c(-1, -6)],
  main = "1_15",
  ylim = c(0, 10),
  yaxt = "n"
)

boxplot(
  hh_all[hh_all$IHEH > 15 & hh_all$IHEH < 30, c(-1, -6)],
  main = "15_30",
  ylim = c(0, 10),
  yaxt = "n"
)

boxplot(
  hh_all[hh_all$IHEH >= 30 & hh_all$IHEH < 50, c(-1, -6)],
  main = "30_50",
  ylim = c(0, 10),
  yaxt = "n"
)

boxplot(
  hh_all[hh_all$IHEH >= 50 & hh_all$IHEH <= 100, c(-1, -6)],
  main = "50_100",
  ylim = c(0, 10),
  yaxt = "n"
)

x <-  0:35

y <- x*100/35
y
df <- data.frame(x,y)
df
