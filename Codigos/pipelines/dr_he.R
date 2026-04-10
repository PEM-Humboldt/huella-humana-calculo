## vias- dr_he  ####
#**********************************************************
# Asignar los pesos a las carreteras y vías férreas
# luego rasterizar cada una de las las capas de vías sobre la cuadrícula base y calcular la distancia euclidiana desde cada celda

vias_groups <- lapply(list(vias2, vias4, vias5, vias8), function(x) {
  p <- x %>%
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

gc()
cat("creacion pesos vias terminado.\n")