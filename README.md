Cálculo Huella espacial humana
================
El Índice de Huella Espacial Humana (IHEH) permite representar espacialmente la intensidad del impacto acumulado de las actividades humanas sobre los ecosistemas terrestres. A medida que el valor del IHEH aumenta, se indica un mayor nivel de presión o transformación ejercida por el ser humano sobre estos ecosistemas. En este sentido, valores bajos reflejan una menor intervención, mientras que los valores altos señalan áreas con una fuerte influencia antrópica.
Este repositorio tiene los paso y datos para hacer el cálculo de la huella espacial humana a nivel nacional. 
- fALTA:
- Regularidad
- descripcion codigos, y correcion rutas
- Recursos necesarios y tiempo de correr código
- Referencias- Enlace a Huella Heon Network
- release
- licencia
- doi




## Organizar directorio de trabajo

Los datos para correr los códigos están almacenados
[aquí](https://drive.google.com/file/d/1YQjFb3u8uJ7UmWHlNncM_UXtJ_gJcOmz/view?usp=drive_link).
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
    │-Huella_IAVH2025_Mapbiomas
    │    └- 00_InsumosGenerales.R
    │    └- 01_Insumosxhuella.R
    │    └- 02_Pesos_navegabilidad.R
    │    └- 021_Pesos_ferreos.R
    │    └- 03_HuellaHumana_adaptadaVias.R
    │
    │-Huella_IAVH2025_Corine
    │    └- 00_InsumosGenerales.R
    │    └- 01_Insumosxhuella.R
    │    └- 021_Pesos_ferreos.R
    │    └- 03_HuellaHumana_adaptadaVias.R
    │-Miscelaneo
    │    └- 000_Insumoscobertura_tablaLU.R
    │    └- analisis_fuentesCoberturas.R
    │    └- reclass_corine.R
    │    └- VC_reclacificacionCori&Mapbiomas.R
    │    
    └-Datos
    │ │
    │ └- replaze aquí los datos que  descargue 
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
- Tiempo de intervención 2018 Buscarlos en la carpeta de datos [aquí](https://drive.google.com/file/d/1YQjFb3u8uJ7UmWHlNncM_UXtJ_gJcOmz/view?usp=drive_link)
- [Vías de open Street Maps](https://download.geofabrik.de/south-america/). Descargar el archivo correspondiente al primero de enero del año siguiente al año de interés.
- [Red Vial del Igac](https://www.colombiaenmapas.gov.co/?e=-84.08030383789075,-1.38663143198846,-64.41477649414598,11.402208518426857,4686&b=igac&u=0&t=39&servicio=1468)
- [Datos de población](https://jeodpp.jrc.ec.europa.eu)

## Códigos
En esta sección se guardan los códigos relacionados con  la construcción de la huella espacial humana y sus versiones: 

### Huella_Diaz_Y_Adaptacion
1. 00_InsumosGenerales.R: En este código se preparan los insumos base para correr el IHEH. Dichos insumos no cambian comunmente, ya que son los que definen los parametros generales de la misma; proyección, extensión.
Además se preparan:
    - Capas constantes con los parametros generales como: Ecosistemas potenciales y Tiempo de Intervención.
    - Tabla de consulta para la definición de las clases de biomasa y uso de tierra
2. 01_Insumosxhuella.R: Se preparan los insumos necesarios para correr el IHEH y se almacenan en la carpeta de resultados intermedios, Res_Intermedios
3. 02_HuellaHumana_Diaz.R: En este código calcula la huella 2018 replicando los pasos de los modelos de ArcMap de Julian Díaz. Por motivos computacionales fue necesario hacer hacer algunas modificaciones.
4. 03_HuellaHumana_adaptada.R: En este código calcula la huella con el método de ecosistemas. Esta incluye los siguiientes cambios.
    - Variables continuas como continuas
    - Remoción de variables no esenciales para el cálculo.
5. 04_HuellaHumana_adaptadaVias: En este código se calcula la huella con el método de ecosistemas. Esta incluye los siguientes cambios:  
    - Variables continuas como continuas  
    - Remoción de variables no esenciales para el cálculo  
    - Diferenciación de vías:  
        - Vías vehiculares principales y secundarias  
        - Vías terciarias y rurales  
        - Infraestructura peatonal y no clasificada  
        - Senderos naturales

### Huella_IAVH2025_Mapbiomas .... en construcción!!!!!!!!!!!
1. 00_InsumosGenerales.R: En este código se preparan los insumos base para correr el IHEH. Dichos insumos no cambian comunmente, ya que son los que definen los parametros generales de la misma; proyección, extensión.
Además se preparan:
    - Capas constantes con los parametros generales como: Ecosistemas potenciales y Tiempo de Intervención.
    - Tabla de consulta para la definición de las clases de biomasa y uso de tierra
2. 01_Insumosxhuella.R: Se preparan los insumos necesarios para correr el IHEH y se almacenan en la carpeta de resultados intermedios, Res_Intermedios
3. 02_Pesos_navegabilidad.R:
4. 021_Pesos_ferreos.R: 
5. 04_HuellaHumana_adaptadaVias: En este código se calcula la huella con el método de ecosistemas. Esta incluye los siguientes cambios:  
    - Variables continuas como continuas  
    - Remoción de variables no esenciales para el cálculo  
    - Diferenciación de vías:  
        - Vías vehiculares principales y secundarias  
        - Vías terciarias y rurales  
        - Infraestructura peatonal y no clasificada  
        - Senderos naturales
### Huella_IAVH2025_Corine.
1. 00_InsumosGenerales.R: En este código se preparan los insumos base para correr el IHEH. Dichos insumos no cambian comunmente, ya que son los que definen los parametros generales de la misma; proyección, extensión.
Además se preparan:
    - Capas constantes con los parametros generales como: Ecosistemas potenciales y Tiempo de Intervención.
    - Tabla de consulta para la definición de las clases de biomasa y uso de tierra
2. 01_Insumosxhuella.R: Se preparan los insumos necesarios para correr el IHEH y se almacenan en la carpeta de resultados intermedios, Res_Intermedios
4. 021_Pesos_ferreos.R: 
5. 04_HuellaHumana_adaptadaVias: En este código se calcula la huella con el método de ecosistemas. Esta incluye los siguientes cambios:  
    - Variables continuas como continuas  
    - Remoción de variables no esenciales para el cálculo  
    - Diferenciación de vías:  
        - Vías vehiculares principales y secundarias  
        - Vías terciarias y rurales  
        - Infraestructura peatonal y no clasificada  
        - Senderos naturales
4. prueba_categorías.R:
 
### Miscelaneo
1. 000_Insumoscobertura_tablaLU
2. analisis_fuentesCoberturas
3. reclass_corine100
4. VC_reclasificaionCori&Mapbiomas
   
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

