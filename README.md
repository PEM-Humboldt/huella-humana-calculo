Cálculo Huella espacial humana
================
El Índice de Huella Espacial Humana (IHEH) permite representar espacialmente la intensidad del impacto acumulado de las actividades humanas sobre los ecosistemas terrestres. Su escala es del 0 al 100. A medida que el valor del IHEH aumenta, se indica un mayor nivel de presión o transformación ejercida por el ser humano sobre estos ecosistemas. En este sentido, valores bajos reflejan una menor intervención, mientras que los valores altos señalan áreas con una fuerte influencia antrópica.
Este repositorio tiene los paso y datos para hacer el cálculo de la huella espacial humana a nivel nacional. 
Para el territorio nacional corriendo todos los pasos el tiempo de ejecución de los códigos puede ser de 3 a 8 horas, dependiendo de la rutina elegida.
Idealmente el cálculo de la huella humana está planteado bienal.
Las capas de huella se pueden encontrar en [Geonetwork](https://geonetwork.humboldt.org.co/geonetwork/srv/spa/catalog.search#/metadata/0277ec94-63b3-41db-9349-d53434fa1251)

- fALTA:
- doi

## Organizar directorio de trabajo

Los datos para correr los códigos están almacenados
[aquí](https://drive.google.com/file/d/1PUlXwC8_2_-43vScYRxiwmlgcgHVqB0F/view?usp=drive_link).
Una vez descargada y descomprimida la carpeta, reemplaze la carpeta “Datos” en el directorio Datos del proyecto.
El directorio del proyecto está organizado de la siguiente manera.

    Codigos
    │-Huella_Diaz_Y_Adaptacion
    │    └-  00_InsumosGenerales.R
    │    └- 01_Insumosxhuella.R
    │    └- 02_HuellaHumana_Diaz.R
    │    └- 03_HuellaHumana_adaptada.R
    │    └- 04_HuellaHumana_adaptadaVias.R
    │
    │-pipelines
    │    └- vias_ferreas.R
    │    └- Descarga_y_mergePop.R
    │    └- osm_cat_vector.R
    │    └- Navegabilidad.R
    │    └- creación LU0_y_TNT_corine.R
    │    └- creación LU0_y_TNT_mp.R
    │    └- dr_he.R
    │    └- if_he.R
    │    └- lu_he.R
    │    └- pd_he.R
    │
    └- 00_InsumosGenerales.R
    └- 01_Insumosxhuella_automatizado.R
    └- 02_IHEH_calculo.R
    └- 03_DatosGeonetwork.R
    │
    │-Miscelaneo
    │    └- 000_Insumoscobertura_tablaLU.R
    │    └- analisis_fuentesCoberturas.R
    │    └- reclass_corine.R
    │    └- VC_reclacificacionCori&Mapbiomas.R
    │    └- Huella_IAVH2025_Mapbiomas
    │        └- 00_InsumosGenerales.R
    │        └- 01_Insumosxhuella.R
    │        └- 02_Pesos_navegabilidad.R
    │        └- 021_Pesos_ferreos.R
    │        └- 03_HuellaHumana_adaptadaVias.R
    │    └- Huella_IAVH2025_CorineNDFB
    │        └- 00_InsumosGenerales.R
    │        └- 01_Insumosxhuella.R
    │        └- 02_Pesos_navegabilidad.R
    │        └- 021_Pesos_ferreos.R
    │        └- 03_HuellaHumana_adaptadaVias.R
    │    └--Huella_IAVH2025_Corine
    │        └- 00_InsumosGenerales.R
    │        └- 01_Insumosxhuella.R
    │        └- 021_Pesos_ferreos.R
    │        └- 03_HuellaHumana_adaptadaVias.R.
    │    
    └-Datos
    │ │
    │ └- reemplaze aquí los datos que  descargue 
    │ 
    |
    └- Res_Intermedios
    |
    └- Resultados

## Datos

Las fuentes de datos necesarios para la elaboración de la huella son:

- [Ecosistemas potenciales de Colombia, año 2015, Escala 1:100.000]( https://geonetwork.humboldt.org.co/geonetwork/srv/spa/catalog.search#/metadata/08b22fe2-5c4a-4b7a-89a8-ed6ea5d9cbdb)
- [Leyenda Mapbiomas](https://colombia.mapbiomas.org/wp-content/uploads/sites/3/2024/11/Codigo-de-la-Leyenda-coleccion-2-1.pdf)
- [Cobertura de la Tierra Mapbiomas](https://colombia.mapbiomas.org/wp-content/uploads/sites/3/2024/11/Codigo-de-la-Leyenda-coleccion-2-1.pdf) del año requerido
- [Cobertura de la tierra 100K (Corine)](https://experience.arcgis.com/experience/568ddab184334f6b81a04d2fe9aac262/page/Datos-Abiertos-Geogr%C3%A1ficos-/) del año requerido
- Tiempo de intervención 2018 Buscarlos en la carpeta de datos [aquí](https://drive.google.com/file/d/1PUlXwC8_2_-43vScYRxiwmlgcgHVqB0F/view?usp=drive_link)
- [Vías de open Street Maps](https://download.geofabrik.de/south-america/). Descargar el archivo correspondiente al primero de enero del año siguiente al año de interés.
- [Red Vial del Igac](https://www.colombiaenmapas.gov.co/?e=-84.08030383789075,-1.38663143198846,-64.41477649414598,11.402208518426857,4686&b=igac&u=0&t=39&servicio=1468)
- Datos de población: [GHS-POP R2023A - GHS population grid  multitemporal (1975–2030) European Commission, Joint Research Centre - JRC](https://data.europa.eu/data/datasets/2ff68a52-5b5b-4a22-8f40-c41da8332cfe?locale=en)

## Códigos

En esta sección se almacenan los scripts relacionados con la construcción de la Huella Espacial Humana (IHEH). La estructura está organizada en diferentes subcarpetas según su función dentro del flujo de trabajo.

🔹 Scripts principales (nivel raíz de Codigos)

En la carpeta principal se encuentran cuatro scripts clave:

```
- 00_InsumosGenerales.R: Creación y definición del raster base (extensión, resolución y sistema de referencia espacial).
- 01_Insumosxhuella_automatizado.R: Procesamiento de los datos que describen las presiones antrópicas.
- 02_IHEH_calculo.R: Asignación de pesos e integración de las presiones para el cálculo del IHEH.
- 03_DatosGeonetwork.R: Preparacion de datos en formato compatible con GeoNetwork.
```  
Los tres primeros scripts (00 a 02) integran diferentes rutinas provenientes de la carpeta pipelines.

🔹 pipelines

Incluye las rutinas para el preprocesamiento y procesamiento de los insumos necesarios para el cálculo de la huella humana.
Estos scripts organizan el flujo de trabajo de manera modular, facilitando la reutilización de procesos comunes como:

- Preparación de insumos base
- Cálculo de pesos (ej. red vial, red ferroviaria)
- Integración de variables espaciales

Las rutinas usadas en 01_Insumosxhuella_automatizado.R son: 
 
    └- vias_ferreas.R
    └- Descarga_y_mergePop.R
    └- osm_cat_vector.R
    └- Navegabilidad.R (es un factor opcional, no esta integrado en el calculo final por ahora)
    └- creación LU0_y_TNT_corine.R
    └- creación LU0_y_TNT_mp.R

Las rutinas usadas en 02_IHEH_calculo.R son: 
        
    └- dr_he.R
    └- if_he.R
    └- lu_he.R
    └- pd_he.R

El flujo de trabajo general de las huellas es el siguiente:
<img width="1023" height="1066" alt="flujo_huella" src="https://github.com/user-attachments/assets/f9637160-f850-47ac-832f-689fcc21360b" />

🔹 Huella_Diaz_Y_Adaptacion

Contiene los códigos base de la metodología original de huella humana desarrollada por Julián Díaz, tal como fue implementada en el workflow de ArcMap. 

```
1. 00_InsumosGenerales.R: En este código se preparan los insumos base para correr el IHEH. Dichos insumos no cambian comunmente, ya que son los que definen los parametros generales de la misma; proyección, extensión.
Además se preparan:
    - Capas constantes con los parametros generales como: Ecosistemas potenciales y Tiempo de Intervención.
    - Tabla de consulta para la definición de las clases de biomasa y uso de tierra
2. 01_Insumosxhuella.R: Se preparan los insumos necesarios para correr el IHEH y se almacenan en la carpeta de resultados intermedios, Res_Intermedios
3. 02_HuellaHumana_Diaz.R: En este código calcula la huella 2018 replicando los pasos de los modelos de ArcMap de Julian Díaz. Por motivos computacionales fue necesario hacer hacer algunas modificaciones.
```
Adicionalmente, la carpeta incluye versiones adaptadas del método, en las cuales se incorporan: Diferentes tipos de vías, Variables continuas, Ajustes metodológicos (inclusión/remoción de variables). Los códigos asociados con estas adaptaciones son: 
```
5. 03_HuellaHumana_adaptada.R: En este código calcula la huella con el método de ecosistemas. Esta incluye los siguiientes cambios.
    - Variables continuas como continuas
    - Remoción de variables no esenciales para el cálculo.
6. 04_HuellaHumana_adaptadaVias: En este código se calcula la huella con el método de ecosistemas. Esta incluye los siguientes cambios:  
    - Variables continuas como continuas  
    - Remoción de variables no esenciales para el cálculo  
    - Diferenciación de vías(!!!Los pesos no son idénticos a los que se describirán en la nueva "Huella_IAVH2025_Corine"):  
        - Vías vehiculares principales y secundarias  
        - Vías terciarias y rurales  
        - Infraestructura peatonal y no clasificada  
        - Senderos naturales
```
🔹 Miscelaneo (Descripción parcial)
    
Esta carpeta contiene:

- Scripts auxiliares para análisis específicos o exploratorios (ej. prueba_categorías.R: Este código permite analizar cómo están distribuidos los valores de los diferentes factores de la huella en las categorías discretas de la misma.)
- Rutinas cortas para procesamiento de datos
- Subcarpetas con rutinas completas de cálculo de huella usando diferentes fuentes de cobertura. Se conservan como respaldo metodológico, ya que los códigos fueron reestructurados en pipelines y scripts maestros para evitar la duplicación de procesos.
  
 ```
# Carpeta: Huella_IAVH2025_Mapbiomas
      
    1. 00_InsumosGenerales.R: En este código se preparan los insumos base para correr el IHEH. Dichos insumos no cambian comunmente, ya que son los que definen los parametros generales de la misma; proyección, extensión.
Además se preparan:
        - Capas constantes con los parametros generales como: Ecosistemas potenciales y Tiempo de Intervención.
        - Tabla de consulta para la definición de las clases de biomasa y uso de tierra
    2. 01_Insumosxhuella.R: Se preparan los insumos necesarios para correr el IHEH y se almacenan en la carpeta de resultados intermedios, Res_Intermedios
    3. 02_Pesos_navegabilidad.R: calcula las presiones antrópicas potenciales asociadas a la navegabilidad de ríos
    4. 021_Pesos_ferreos.R:  Calcula las presiones antrópicas potenciales asociadas a las vías férreas.
    5. 04_HuellaHumana_adaptadaVias: En este código se calcula la huella con el método de ecosistemas. Esta incluye los siguientes cambios:  
        - Variables continuas como continuas  
        - Remoción de variables no esenciales para el cálculo  
        - Diferenciación de vías:  
            - Vías vehiculares principales y secundarias  
            - Vías terciarias y rurales  
            - Infraestructura peatonal y no clasificada  
            - Senderos naturales
```
      
              
```
# Carpeta: Huella_IAVH2025_Corine.
  
    1. 00_InsumosGenerales.R: En este código se preparan los insumos base para correr el IHEH. Dichos insumos no cambian comunmente, ya que son los que definen los parametros generales de la misma; proyección, extensión.
Además se preparan:
        - Capas bases constantes con los parametros generales 
    2. 01_Insumosxhuella.R: Se preparan los insumos necesarios para correr el IHEH y se almacenan en la carpeta de resultados intermedios, Res_Intermedios.
        - División de tipos de vías en formato vector
        - Descarga de datos poblacionales creación de mosaico y cálculo de densidad
        - Creación de raster de pesos de cobertura de la tierra
        - Creación de raster transformado -no transformado
    4. 021_Pesos_ferreos.R:  Calcula las presiones antrópicas potenciales asociadas a las vías férreas.
    5. 04_HuellaHumana_adaptadaVias: En este código se calcula la huella con el método de ecosistemas. Esta incluye los siguientes cambios:  
        - Variables continuas como continuas  
        - Remoción de variables no esenciales para el cálculo  
        - Inclusión de infraestructura férrea
        - Diferenciación de vías:  
            - Vías vehiculares principales y secundarias  
            - Vías terciarias y rurales  
            - Infraestructura peatonal y no clasificada  
            - Senderos naturales
           
```



## Versiones de los paquetes usados

En el momento de la elaboración de los códigos las version de R y de los paquetes necesarios para los códigos fue. 

R version 4.4.1 (2024-06-14 ucrt)
- dplyr_1.1.4  
- terra_1.7-83 
- sf_1.0-19
- raster_3.6-30   
- sp_2.1-4        
- lubridate_1.9.3 
- forcats_1.0.0   
- stringr_1.5.1   
- purrr_1.0.2
- readr_2.1.5     
- tidyr_1.3.1     
- tibble_3.2.1    
- ggplot2_3.5.1   
- tidyverse_2.0.0
- tidyr_1.3.1

