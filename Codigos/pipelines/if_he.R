## if_he  ####
#**********************************************************
# Se calcula un indicador de fragmentación basado en la
# densidad de píxeles naturales dentro de una vecindad
# circular de 1 km de radio. A partir de esta densidad,
# se asignan pesos de huella mediante una función de
# decaimiento exponencial.

vecindad <- focalMat(TNT, type = "circle", d = 1000)  # Ventana circular de 1 km

# Calcular la suma de píxeles naturales dentro de la vecindad
densidad_0 <- focal(TNT,
                    w = vecindad,
                    fun = sum,
                    na.rm = TRUE) * 100  # Conversión de proporción a porcentaje

# Aplicar función de decaimiento exponencial para obtener el indicador de fragmentación
if_he <- 10 * exp(-0.05 * densidad_0)

# Ajuste del indicador:
# Cuando la cobertura es completamente natural (alta densidad),
# el valor del indicador debe ser 0 
if_he[if_he < 0.07] <- 0

densidad_0