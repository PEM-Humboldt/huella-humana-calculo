.Cálculo Huella espacial humana
================

Este repositorio tiene los paso y datos para hacer el cálculo de la huella espacial humana a nivel nacional. 


## Organizar directorio de trabajo

Los datos para correr los códigos están almacenados
[aquí](https://drive.google.com/file/d/1YQjFb3u8uJ7UmWHlNncM_UXtJ_gJcOmz/view?usp=drive_link).
Una vez descargada y descomprimida la carpeta, reemplaze la carpeta “Datos” en el directorio Datos del proyecto.
El directorio del proyecto está organizado de la siguiente manera.

    Codigos
    │-  00_InsumosGenerales.R
    │-  01_Insumosxhuella.R
    │-  02_HuellaHumana_Diaz.R
    │-  03_HuellaHumana_adaptada.R
    │-  04_Comparar entre versiones.R
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

