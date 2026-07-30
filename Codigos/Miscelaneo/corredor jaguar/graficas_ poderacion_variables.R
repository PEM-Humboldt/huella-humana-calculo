

## Pop Pd_he  ####
#**********************************************************

# Definición de los pesos Basados En la densidad poblacional Según Venter 2016

x= 1 :200
y= 4.999 * log10(x + 1)
y[y > 10] <- 10
Pd_he <- data.frame(x,y)
           # Limitar a máximo 10

plot(Pd_he, main="Población", xlab="Población (hab/km2)", ylab= "Pesos")


## vias- dr_he  ####
#**********************************************************


# Aquí los parametros para la asignación de pesos de la distancia a vias. Los pesos tienen una escala continua y es diferencial para tipos de vías.
# La fracción que verá en los nombres a continuación significa el rango de valores que va a tener este tipo de vías. Ejemplo 8/4 Quiere decir que este tipo de vías tendrán valores entre 8 para el impacto directo y a partir de 4 para el contacto indirecto.



clsDisVias <- function(x, max = 4) {
  max * exp(-0.33 * (x / 1000 - 0.5))
}

x= 1 :20000


par(mfrow=c(2,2))
# Pesos - 8/4. Vías vehiculares principales y secundarias
y= clsDisVias (x)
y[y> 4] <- 8 
y8 <- data.frame(x,y)
plot(y8, xlab="Distancia V.principales-secundarias(km)", ylab= "Pesos", ylim= c(0,10))


# Pesos - 5/4. Vías terciarias y rurales
y= clsDisVias (x)
y[y> 4] <- 5 
y5 <- data.frame(x,y)
plot(y5, xlab="Distancia V.terciarias y rurales(km)", ylab= "Pesos", ylim= c(0,10))



# Pesos - 4/2. Infraestructura peatonal y no clasificada
y= clsDisVias (x, max= 2)
y[y> 2] <- 4 
y4 <- data.frame(x,y)
plot(y4,xlab="Distancia V. peatonal(km)", ylab= "Pesos", ylim= c(0,10))

# Pesos - 2/2. Senderos naturales

y= clsDisVias (x, max= 2)
y[y> 2] <- 2 
y2 <- data.frame(x,y)
plot(y2, xlab="Distancia sendero natural(km)", ylab= "Pesos", ylim= c(0,10))


## B_per. POrcentaje de Bosque 1km2  ####
#**********************************************************
# Se calcula Un indicativo de la fragmentación basado en densidad de píxeles Naturales En un área específica De 1 km de radio y se le asignan los pesos The huella Basado en decaimiento exponencial

### jaguar ###############



x=1:100

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


y= fuzzy_sigmoid (x)*10
Bperc_J <- data.frame(x,y)

plot(Bperc_J,main="Jaguar", xlab=" Porcentaje de bosques (%)", ylab= "Pesos")


### 500 M2 ONCILLA ####

y= (1-fuzzy_sigmoid ( x,       
                     base = 0,
                     techo = 85,
                     valor_base = 0.001,
                     valor_techo = 0.999,
                     midpoint=50, 
                     suavidad=1.5))*10


Bperc_O <- data.frame(x,y)

plot(Bperc_O,main="Tigrillo", xlab=" Porcentaje de bosques (%)", ylab= "Pesos")



## Dist ríos  ####
#**********************************************************
x= 1:18000

y <- fuzzy_sigmoid (x,
                                  base = 1000,
                                  techo = 16000,
                                  valor_base = 0.001,
                                  valor_techo = 0.999, # si aplico suavidad . este parametro no importa
                                  midpoint=7000,
                                  suavidad=2)*10



pesos_rios_dist <- data.frame(x,y)

plot(pesos_rios_dist,main="Jaguar", xlab="Distancia ríos (km)", ylab= "Pesos")

# significado diatancias y pesos
# 1-- 3185
# 2 --- 4594
# 3 -- 5529
# 4 -- 6300

plot(pesos_rios_dist)

## Elevacion  ####
#**********************************************************

### jaguar ####

x <- 0:5000

y <- fuzzy_sigmoid (x,
                            base = 1000,
                            techo = 3000,
                            valor_base = 0.001,
                            valor_techo = 0.999,
                            midpoint=2000)*10

pesos_dem_J <- data.frame(x,y)

plot(pesos_dem_J,main="Jaguar", xlab="elevación (m)", ylab= "Pesos")

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


y <- (1-fuzzy_bell_altura (x))*10

pesos_dem_O <- data.frame(x,y)


plot(pesos_dem_O,,main="Tigrillo", xlab="elevación (m)", ylab= "Pesos")

## Pendiente  ####
#**********************************************************
# JAGUAR

x <- 1:90
y <- fuzzy_sigmoid (x,
                             base = 15, # oncilla 30, jaguar 15
                             techo = 80, # no importa el valor
                             valor_base = 0.001,
                             valor_techo = 0.999,
                             midpoint=35, # oncilla 55, jaguar 35
                             suavidad = 2)*10 


pesos_pend_J <- data.frame(x,y)
plot(pesos_pend_J,main="Jaguar", xlab=" pendiente (grados)", ylab= "Pesos")

# ONCILLA
x <- 1:90

y <- fuzzy_sigmoid (x,
                             base = 30, # oncilla 30, jaguar 15
                             techo = 80, # no importa el valor
                             valor_base = 0.001,
                             valor_techo = 0.999,
                             midpoint=55, # oncilla 55, jaguar 35
                             suavidad = 2)*10 


pesos_pend_O <- data.frame(x,y)
plot(pesos_pend_O,main="Tigrillo", xlab=" pendiente (grados)", ylab= "Pesos")



