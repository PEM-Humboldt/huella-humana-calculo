
# Título: Preparación General de los insumos Huella 
#
# Autor(es): Alejandra Narváez Vallejo
#
# Descripción: En este código se preparan los insumos base para correr el IHEH. Dichos insumos no cambian comunmente, ya que son los que definen los parametros generales de la misma; proyección, extensión.
# Además se preparan:
## - Capas constantes con los parametros generales como: Ecosistemas potenciales y Tiempo de Intervención.
## - Tabla de consulta para la definición de clases de biomasa y uso del suelo, y valores del IHEH1.
#Esta tabla contiene la clasificación combinada de biomasa y uso del suelo, junto con los valores correspondientes del índice IHEH1, calculado como la suma de las huellas por biomasa y por uso del suelo. La tabla servirá como una tabla de consulta ("lookup table") en el cálculo posterior del índice de huella ecológica humana. En ese proceso, se compararán los valores observados de biomasa y uso del suelo del año analizado con esta tabla para asignar automáticamente el valor de IHEH1 correspondiente, a partir de la tabla generada aquí bajo el nombre "combinaciones".

# Por hacer o  corregir: 

## - Es posible que vias cambien la forma en que los datos del IGAC den el tipo de revisar. Tener en cuenta y cambiar cuando sea necesario.


#**********************************************************
# librerías o dependencias --------------------------------
#**********************************************************

# lectura de datos 

library (sf) 
library(terra)
library(dplyr)

#**********************************************************
# Definir directorio(s) de trabajo -----------------------
#**********************************************************
# Se define el directorio de trabajo relativo a la ubicación del script
setwd(file.path(this.path::this.path(), "..", ".."))

# Directorios para datos, resultados intermedios y finales
dir_datos <- file.path("Datos")
dir_Intermedios <- file.path("Res_Intermedios")
dir_Resultados <- file.path("Resultados")

#**********************************************************
# Cargar los datos necesarios -----------------------------
#**********************************************************

# Raster base para análisis
r_base <- rast(file.path(dir_datos, "r_base.tif"))

# Tablas con leyendas y ecosistemas potenciales
Ecos_Pot <- read.csv2(file.path(dir_datos, "Ecos_Pot.csv"))
Leyenda_LU <- read.csv2(file.path(dir_datos, "Leyenda_LU.txt"))

# Funciones

# Función para verificar si el CRS coincide
crs_igual <- function(r, crs_ref) {
  tryCatch({
    crs(r) == crs_ref
  }, error = function(e) FALSE)
}

#**********************************************************
# Parametros globales ----------------------------
#**********************************************************
 
resolucion <-  100   # Resolución objetivo para el análisis
scoord <- crs(r_base) # Sistema de coordenadas del raster base. Cambiar cuando se defina la proyección

coincidencia_crs <- crs_igual(r_base, scoord)


#**********************************************************
# Preparar datos ----------------------------
#**********************************************************

##  raster base --------------------------------------------------

# Definir rutas de los archivos
archivo_r_base <- file.path(dir_datos, "r_base.tif")
archivo_r_base10 <- file.path(dir_datos, "r_base10.tif")


# Condición para crear o no los archivos
if (!file.exists(archivo_r_base) || 
    !file.exists(archivo_r_base10) || 
    !coincidencia_crs) {
  
  # Crear r_base proyectado
  r_base <- project(r_base, scoord, res = resolucion, method = "near")
  
  # Crear r_base10
  r_base10 <- disagg(r_base, fact = 10)
  
  # Guardar los archivos
  writeRaster(r_base, archivo_r_base, datatype = "INT1U", overwrite = TRUE)
  writeRaster(r_base10, archivo_r_base10, datatype = "INT1U", overwrite = TRUE)
  
} else {
  # Leer desde disco
  r_base <- rast(archivo_r_base)
  r_base10 <- rast(archivo_r_base10)
}

##  Ecositemas potenciales  --------------------------------------------------
# Definir rutas de los archivos de salida
archivo_eco_100 <- file.path(dir_datos, "Eco_100Rc1.tif")

# Procesar ecosistemas potenciales solo si no existe el archivo
if (!file.exists(archivo_eco_100)|| 
    !coincidencia_crs) {
  
  # Leer el raster de ecosistemas potenciales desde la GDB
  gdb_path <- file.path(dir_datos, "EcosistemasPotencialesDeColombia.gdb")
  raster_layer <- rast(gdb_path, "EcosistemasPotencialesDeColombia")
  
  # Ajustar resolución y proyectar al sistema del raster base
  Eco_100 <- raster_layer %>%
    project(r_base10, method = "near") %>%
    aggregate(fact = 10, fun = "modal")
  
  # Asignar niveles a la capa según la leyenda (Opcional)
  ley_pot <- Ecos_Pot %>%
    dplyr::select(ECOSPOTENC, COD) %>%
    unique()
  levels(Eco_100) <- ley_pot
  
  # Guardar raster de ecosistemas potenciales reescalado
  writeRaster(Eco_100, archivo_eco_100, datatype = "INT1U", overwrite = TRUE)
  
} else {
  # Si ya existe, solo cargarlo
  Eco_100 <- rast(archivo_eco_100)
}

##  Tiempo de intervención --------------------------------------------------
# Definir rutas de los archivos de salida

archivo_tiempo_int <- file.path(dir_Intermedios, "TiempoInt_2018_100.tif")

# Procesar tiempo de intervención solo si no existe el archivo
if (!file.exists(archivo_tiempo_int)|| 
    !coincidencia_crs) {
  
  # Cargar raster de tiempo de intervención, ajustar a resolución y guardar
  TI <- rast(file.path(dir_datos, "TiempoInt_20181.tif"))
  TI <- resample(TI, r_base, method = "near")
  writeRaster(TI, archivo_tiempo_int, datatype = "INT2U", overwrite = TRUE)
  
} else {
  # Si ya existe, solo cargarlo
  TI <- rast(archivo_tiempo_int)
}


#**********************************************************
# Preparar tabla de consulta 
#**********************************************************


# Tablas de atributos y leyendas
Ecos_Pot1 <- Ecos_Pot[, c("ECOSPOTENC", "COD", "LLAVE", "FISIO")] %>% unique()

# Clasificación de categorías de vegetación (fisionomías).importantes para definir los grupos de las categorias de cobertura de tierra y biomasa

Arbustales <- c(
  'Arbustales  abiertos y suculentas' ,
  'Arbustales  bajos muy abiertos y areas deserticas' ,
  'Arbustales xerofÃ­ticos' ,
  'Arbustales y Bosques Bajos Densos esclerÃ³filos' ,
  'Arbustales y cardonales densos'
)

Bosques <- c(
  'Bosques Altos Densos' ,
  'Bosques altos densos de manglar' ,
  'Bosques Altos Densos y pantanos' ,
  'Bosques Altos y Medios Densos' ,
  'Bosques Bajos Densos esclerÃ³filos (Caatingas amazÃ³nicas altas)' ,
  'Bosques bajos densos y Arbustales' ,
  'Bosques bajos densos y arbustales de manglar' ,
  'Bosques Bajos y Arbustales Densos' ,
  'Bosques Bajos y Arbustales Densos esclerÃ³filos (Caatingas amazÃ³nicas medias)' ,
  'Bosques bajos, herbazales y vegetaciÃ³n flotante' ,
  'Bosques Medios Densos' ,
  'Bosques Medios y Bajos Densos' ,
  'Bosques, herbazales y pantanos'
)

Herbazales <- c(
  'Herbazales' ,
  'Herbazales (campinas amazonicas)' ,
  'Herbazales abiertos y nieves' ,
  'Herbazales y arbustales densos' ,
  'Sabanas  casmÃ³fitas y herbazales' ,
  'Sabanas herbÃ¡ceas con arbustales' ,
  'Sabanas herbÃ¡ceas y arbustales'
)

Paramo <- c('S13' , 'S14' , 'S15' , 'N')

Agua <- c('Lagos, lagunas, ciÃ©nagas permanentes, y cauces de rios mayores')

Urbano <-  'Infraestructura urbana'

Mineria <-  'Minería'

Noveg <- 'Otra área sin vegetación'

GTF1 <- c ('Acuicultura',
           'Mosaico de agricultura y/o pasto' ,
           'Palma aceitera')

bi1 <- c (
  'Sabanas  casmÃ³fitas y herbazales del Pedobiomas del Zonobioma de los Bosques HÃºmedos Tropicales',
  'Sabanas herbÃ¡ceas con arbustales del Pedo/Peinobiomas del Zonobioma de Bosque Seco Tropical y BsHT',
  'Sabanas herbÃ¡ceas y arbustales del Pedobiomas del Zonobioma de los Bosques HÃºmedos Tropicales'
)


## Crear combinaciones únicas #####
#--------------------------------------------------

# Combinación de clases de uso de suelo, ecosistema y transformación (0 = natural, 1 = transformado)
combinaciones <- expand.grid(Leyenda_LU$Id, Ecos_Pot1$ECOSPOTENC, 0:1)
colnames(combinaciones) <- c("LegendN", "CODN", "TNT")

# Reemplazar valores problemáticos
combinaciones$LegendN[combinaciones$LegendN == 0] <- 51  #reclasificar para no tener el valor de 0 en dos columnas diferentes que al hacer la suma de las columnas confundiria los valores únicos


# Obtener codigos únicos para las combinaciones
# 1. multiplicar las columnas por 10 y 1000 para tener los codigos en difrentes posiciones y así con una sumatoria de las columnas lograr obtener una "IDC" de valores únicos por combinación.
# 2. Unir con las tablas de atributos de cobertura de tierra y Ecosisitemas potenciales 

combinaciones <- combinaciones %>%
  mutate(
    LegendN10 = LegendN * 10,
    CODN1000 = CODN * 1000,
    IDC = LegendN10 + CODN1000 + TNT
  ) %>%
  left_join(Leyenda_LU[2:3], by = join_by(LegendN == Id)) %>%
  left_join(Ecos_Pot1, by = join_by(CODN == ECOSPOTENC))


## Clasificación: Grado de transformación ####
#--------------------------------------------------

combinaciones <- combinaciones %>% # se deben colocar las condiciones por bloques porque si encuetra la condicion por un lado ya no reescribe los resultados
  mutate(
    Grado_tran = case_when(
      FISIO %in% Arbustales & TNT == 1  ~ "Arbustal transformado",
      FISIO %in% Bosques & TNT == 1 ~ "Bosque Transformado" ,
      FISIO %in% Herbazales &   TNT == 1 ~ "Herbazal Transformado",
      FISIO %in% Agua & TNT == 1 ~ "Cuerpos de Agua Transformados",
      FISIO %in% Arbustales & TNT == 0  ~ "Arbustal Natural",
      FISIO %in% Bosques & TNT == 0 ~ "Bosque Natural" ,
      FISIO %in% Herbazales & TNT == 0 ~ "Herbazal Natural",
      FISIO %in% Agua & TNT == 0 ~ "Cuerpos de Agua Naturales"
    )
  ) %>%
  mutate(
    Grado_tran = case_when(
      COD %in% Paramo & TNT == 1 ~ "Paramo transformado",
      COD %in% Paramo & TNT == 0 ~ "Paramo Natural",
      .default = Grado_tran
    )
  ) %>%
  mutate(
    Grado_tran = case_when(
      Grado_tran %in% c(
        'Herbazal Transformado' ,
        'Arbustal transformado' ,
        'Bosque Transformado'
      ) &
        Leyenda == 'Mosaico de agricultura y/o pasto'  ~ "Mosaico de cultivos",
      Leyenda == Urbano  ~ "Urbanizado",
      Leyenda == Mineria ~ "Minería",
      .default = Grado_tran
    )
  )



## Asignar huella del uso de la tierra (HUELLA_LU) ####
#--------------------------------------------------

# asignación de pesos LU Simplificado

combinaciones <- combinaciones %>%
  mutate(
    Huella_Lu = case_when(
      Grado_tran %in% "Arbustal transformado" & Leyenda == "Silvicultura"  ~ 3,
      Grado_tran %in% "Arbustal transformado" & Leyenda %in% GTF1  ~ 4,
      Grado_tran %in% "Arbustal transformado" & Leyenda %in% c (Noveg)  ~ 1,
      
      
      Grado_tran %in% "Bosque Transformado" & Leyenda == "Silvicultura"  ~ 3,
      Grado_tran %in% "Bosque Transformado" & Leyenda %in% GTF1  ~ 4,
      Grado_tran %in% "Bosque Transformado" & Leyenda %in% c (Noveg)  ~ 1,
      
      
      Grado_tran %in% "Cuerpos de Agua Transformados" & Leyenda %in% c(
        "Silvicultura",
        "Mosaico de agricultura y/o pasto",
        "Palma aceitera"
      )  ~ 4,
      Grado_tran %in% "Cuerpos de Agua Transformados" & Leyenda %in% c (Noveg, "Acuicultura")  ~ 2,
      
      
      Grado_tran %in% "Herbazal Transformado" & Leyenda %in% c("Silvicultura") ~ 3,
      Grado_tran %in% "Herbazal Transformado" & Leyenda %in% GTF1  ~ 4,
      Grado_tran %in% "Herbazal Transformado" & Leyenda %in% c (Noveg)  ~ 2,
      
      
      Grado_tran %in% "Paramo transformado" & Leyenda %in% c (Noveg)  ~ 3,
      Grado_tran %in% "Paramo transformado" & Leyenda %in% c(GTF1, "Silvicultura")  ~ 4
    )
  ) %>%
  
  
  mutate(
    Huella_Lu = case_when(
      Leyenda %in% c (Mineria, Urbano)  ~ 5,
      # es redundante con todas las condiciones de mineria parcadas con "# no", mejor esta sóla en un mutate a parte
      Leyenda %in% c ('Mosaico de agricultura y/o pasto')  ~ 4,
      Grado_tran %in% c(
        'Arbustal Natural',
        'Bosque Natural',
        'Cuerpos de Agua Naturales',
        'Herbazal Natural',
        'Paramo Natural'
      )  ~ 0,
      .default = Huella_Lu
      
    )
  ) 

#**********************************************************
# Asignar huella biomasa (BI_HE) --------------------
#**********************************************************
# arreglar para asegurar que todos los cambios se efectuen

combinaciones <- combinaciones %>%
  mutate(
    Bi_he = case_when(
      Grado_tran %in% "Arbustal transformado" & FISIO %in% Arbustales  ~ 2,
      Grado_tran %in% "Bosque Transformado" & FISIO %in% Bosques  ~ 3,
      Grado_tran %in% "Herbazal Transformado" & FISIO %in% Herbazales [c(2:3)]  ~ 2,
      Grado_tran %in% "Herbazal Transformado" & LLAVE %in% bi1  ~ 2,
      
      Grado_tran %in% c(
        "Cuerpos de Agua Naturales",
        "Arbustal Natural",
        "Bosque Natural",
        "Herbazal Natural",
        "Paramo Natural"
      ) ~ 0,
      
      Grado_tran %in% c(
        "Cuerpos de Agua Transformados",
        "Minería",
        "Urbanizado",
        "Paramo transformado"
      ) ~ 5
    )
  ) %>%
  
  
  mutate(Bi_he = case_when(
    Leyenda %in% c ('Mosaico de agricultura y/o pasto') & 
      (FISIO %in% Herbazales [2:3] | LLAVE %in% bi1) ~ 3,   ### es igual al de abajp??? # mejor en otro mutate abajo
    
    
    Leyenda %in% c ('Mosaico de agricultura y/o pasto') &
      ( FISIO %in% Herbazales [c(1, 4)] |
          LLAVE %in% 'Sabanas herbÃ¡ceas con arbustales del Orobiomas zonales del Zonobioma de Bosque HÃºmedo Tropical'
      ) ~ 4,
    .default = Bi_he
  )) %>%
  
  
  mutate(
    Bi_he = case_when(
      Leyenda %in% c ('Mosaico de agricultura y/o pasto') & (LLAVE %in% bi1) ~ 2, ###??? ESTE REESCRIBE LA PRIMERA CONDICION DEL ANTERIOR MUTATE
      
      
      Grado_tran %in% "Herbazal Transformado" & LLAVE %in% bi1  ~ 2,  ### es identico en la 4 cond, por que dos veces?
      Grado_tran %in% "Herbazal Transformado" &
        (   FISIO %in% Herbazales [c(1, 4)] |
              LLAVE %in%  'Sabanas herbÃ¡ceas con arbustales del Orobiomas zonales del Zonobioma de Bosque HÃºmedo Tropical'
        )  ~ 3,
      # podria tener problemas por la llave
      
      Leyenda %in% c ('Mosaico de agricultura y/o pasto') & (FISIO %in% Bosques) ~ 4,   # mejor aparte
      Leyenda %in% c ('Mosaico de agricultura y/o pasto') & (FISIO %in% Arbustales) ~ 4, # mejor aparte
      .default = Bi_he
      
    )
  )



## Calcular Índice preliminar de Huella Ecológica Humana (Bi_he + Huella_Lu) ####
#--------------------------------------------------

# Reordenar columnas para claridad
combinaciones <- combinaciones[c(6, 1:5, 7:13)]

# Sumar huellas y eliminar combinaciones sin datos válidos
combinaciones <- combinaciones %>%
  mutate(IHEH1 = Bi_he + Huella_Lu) %>%
  drop_na(IHEH1)


archivo_combinaciones <- file.path(dir_datos, "combinaciones.csv")

if (!file.exists(archivo_combinaciones)) {
  write.csv2(combinaciones, archivo_combinaciones, row.names = FALSE)
} else {
  message("El archivo 'combinaciones.csv' ya existe. No se sobrescribió.")
}









