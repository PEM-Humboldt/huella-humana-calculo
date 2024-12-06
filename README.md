Cálculo Huella espacial humana
================
En esta sección se guardan los códigos relacionados con  la construcción de la huelala espacial humana: 

1. 00_InsumosGenerales.R: En este código se preparan los insumos base para correr el IHEH. Dichos insumos no cambian comunmente, ya que son los que definen los parametros generales de la misma; proyección, extensión.
Además se preparan:
 - Capas constantes con los parametros generales como: Ecosistemas potenciales y Tiempo de Intervención.
 - Tabla de consulta para la definición de las clases de biomasa y uso de tierra
2. 01_Insumosxhuella.R:Se preparan los insumos necesarios para correr el IHEH :
 - Población: Descarga (URl en código),  corte a zona de estudio , reproyección, cálculo de densidad. No es necesario correlo si se va a usar la poblacióin del IHEH anterior (la población se calcula cada 5 años). 
 - Vias: 2018: IGAc de Julian y descarga 2019 de osm por que tiene fecha enero 2019 #####
 - Vias: 2022: IGAc (https://www.colombiaenmapas.gov.co/?e=-82.43784778320864,-0.17644239911865092,-71.23179309571162,9.90326984502256,4686&b=igac&u=0&t=23&servicio=205) y descarga 2023 de osm por que tiene fecha enero 2023, Para el cálculo de años posteriores seguir esquema del 2022
3. 02_HuellaHumana_Diaz.R:En este código calcula la huella 2018 replicando los pasos de los modelos de ArcMap de Julian Díaz. Por motivos computacionales fue necesario hacer hacer algunas modificaciones.
4. 03_HuellaHumana_adaptada.R:En este código calcula la huella con el método de ecosistemas. Esta incluye los siguiientes cambios.
  - Variables continuas como continuas
  - Remoción de variables no escenciales para el cálculo.
5. 04_Comparar entre versiones: Código incompleto para comparar entre versiones


## Organizar directorio de trabajo

Los datos para correr los códigos están almacenados
[aquí](https://drive.google.com/file/d/1YQjFb3u8uJ7UmWHlNncM_UXtJ_gJcOmz/view?usp=drive_link)
Una vez descargada y descomprimida la carpeta, reemplaze la carpeta “Datos” en el directorio Datos del proyecto.
El directorio del proyecto está organizado de esta manera que facilita la ejecución del
código:

    Codigos
    │-  00_InsumosGenerales.R
    │-  01_Insumosxhuella.R
    │-  02_HuellaHumana_Diaz.R
    │-  03_HuellaHumana_adaptada.R
    │-  04_Comparar entre versiones.R
    │    
    └-Datos
    │ │
    │ └- replaze aquí los datos que  descargué 
    │ 
    |
    └- Res_Intermedios
    |
    └- Resultados

