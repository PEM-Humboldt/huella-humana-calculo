## Pop Pd_he  ####
#**********************************************************
# Se calculan los pesos de presión antrópica asociados a la
# densidad poblacional, siguiendo la metodología propuesta
# por Venter et al. (2016) para la Huella Humana.
#
# La transformación se realiza mediante una función logarítmica
# (base 10), que reduce la influencia de valores extremos de
# densidad poblacional y permite representar mejor la relación
# no lineal entre población y presión sobre el territorio.

Pd_he <- 3.333 * log10(Pop0 + 1)  # Transformación logarítmica

# Limitar los valores a un máximo de 10, de acuerdo con la
# estandarización de pesos definida en Venter et al. (2016)
Pd_he[Pd_he > 10] <- 10

# Referencia:
# Venter, O., Sanderson, E. W., Magrach, A., Allan, J. R., Beher, J.,
# Jones, K. R., … Watson, J. E. M. (2016).
# Global terrestrial Human Footprint maps for 1993 and 2009.
# Scientific Data, 3, 160067.
#**********************************************************