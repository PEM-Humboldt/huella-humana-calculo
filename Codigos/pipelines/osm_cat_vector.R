# Parámetros de vias OSM ####
#-------------------------------------------------- -
# Aquí los parametros para la asignación de pesos de la distancia a vias. Los pesos tienen una escala continua y es diferencial para tipos de vías.
# La fracción que verá en los nombres a continuación significa el rango de valores que va a tener este tipo de vías. Ejemplo 8/4 Quiere decir que este tipo de vías tendrán valores entre 8 para el impacto directo y a partir de 4 para el contacto indirecto.

# Pesos - 8/4. Vías vehiculares principales y secundarias
osm_class8 <- c(
  "trunk",
  "tertiary",
  "secondary",
  "primary_link",
  "secondary_link",
  "primary",
  "trunk_link",
  "tertiary_link",
  "living_street",
  "residential",
  "motorway_link",
  "motorway",
  "busway"
)

# Pesos - 5/4. Vías terciarias y rurales
osm_class5 <- c(
  "track",
  "track_grade1",
  "track_grade2",
  "track_grade3",
  "track_grade4",
  "track_grade5",
  "service",
  "bridleway",
  "cycleway"
)

# Pesos - 4/2. Infraestructura peatonal y no clasificada
osm_class4 <- c("pedestrian", "footway", "steps", "unknown", "unclassified")

# Pesos - 2/2. Senderos naturales
osm_class2 <- c("path")


#**********************************************************
# Procesamiento de capas ----------------------------
#**********************************************************

##  Vías ----------------------------------------------

# Asignar un atributo de "peso" a cada clase (fclass) de OSM según su categoría de importancia
osm0 <- osm0 %>%
  mutate(
    peso = case_when(
      fclass %in% osm_class8 ~ 8,
      fclass %in% osm_class5 ~ 5,
      fclass %in% osm_class4 ~ 4,
      fclass %in% osm_class2 ~ 2
    )
  ) %>% 
  st_transform( crs = st_crs(scoord))

# Crear tabla auxiliar con combinaciones únicas de clases (fclass) y sus pesos asignados
# Esto es útil como control de calidad o para futuras referencias
h <- unique(st_drop_geometry(osm0[c("fclass", "peso")]))


# Mostrar el mensaje de advertencia

print(h)
cat("⚠ Revisar que todas las categorías tengan un peso; de lo contrario corregir en 'parametros de vias'.\n")

repeat {
  resp <- tolower(readline("¿Deseas continuar? (s/n): "))
  if (resp %in% c("s", "n"))
    break
  cat("Respuesta no válida. Escribe 's' o 'n'.\n")
}

if (resp == "n") {
  stop("Proceso detenido por el usuario.")
}

cat("Continuando con el proceso...\n")


# Separar las geometrías del OSM por grupo de peso para calcular distancias de forma separada
osm_groups <- split(osm0, osm0$peso)

# Convertir cada grupo en un objeto 'sf' sólo con geometría, y agregar un ID genérico
osm_groups <- lapply(osm_groups, st_geometry)
osm_groups <- lapply(osm_groups, function(x) {
  st_sf(data.frame(ID = 1, geom = x))
})

# Asignar pesos a las vías del IGAC según su tipo de vía (TIPO_VIA) si es 2022 o GP_RTP  si es 2018

if (Año >=2020) {
  vias_IGAC2 <- vias_IGAC0 %>%
    mutate(peso = case_when(
      TIPO_VIA %in% c(1:4) ~ 8,
      # Vías principales
      TIPO_VIA %in% c(5:7) ~ 5,
      # Vías secundarias
      TIPO_VIA %in% 8 ~ 2        # Caminos o vías terciarias
    ))
  
  
} else if (Año == 2018) {
  vias_IGAC2 <- vias_IGAC0 %>%
    mutate(peso = case_when(
      GP_RTP %in% c(1:3) ~ 8,
      # Vías principales
      GP_RTP %in% c(4) ~ 5,
      # Vías secundarias
      GP_RTP %in% 8 ~ 2        # Caminos o vías terciarias
    ))
}


# Reproyectar las vías del IGAC al sistema de coordenadas de OSM para que coincidan espacialmente.
st_transform(vias_IGAC2, crs = st_crs(osm0))
vias_IGAC_p2 <- st_transform(vias_IGAC2, crs = st_crs(scoord))

# Separar las geometrías del IGAC por grupo de peso
IGAC_groups <- split(vias_IGAC_p2, vias_IGAC_p2$peso)

# Convertir cada grupo en objeto 'sf' solo con geometría, agregando un ID genérico
IGAC_groups <- lapply(IGAC_groups, st_geometry)
IGAC_groups <- lapply(IGAC_groups, function(x) {
  st_sf(data.frame(ID = 1, geom = x))
})

# Unir las capas OSM e IGAC por cada categoría de peso, para posterior análisis (por ejemplo, cálculo de distancias)
osm_igac8 <- rbind(IGAC_groups$`8`, osm_groups$`8`)
osm_igac5 <- rbind(IGAC_groups$`5`, osm_groups$`5`)
osm_igac2 <- rbind(IGAC_groups$`2`, osm_groups$`2`)
osm_igac4 <- rbind(IGAC_groups$`4`, osm_groups$`4`)  # En este caso solo OSM tiene categoría 4


# Guardar la capa en resultados intermedios
st_write(osm_igac8, OI_file8, append = FALSE)
st_write(osm_igac5, OI_file5, append = FALSE)
st_write(osm_igac4, OI_file4, append = FALSE)
st_write(osm_igac2, OI_file2, append = FALSE)

cat("Generación de Capa de pesos de la insfraestructua vial terminado.\n")
