Proyecto Amazonas FPLV
================
En esta sección se guardan los códigos relacionados con el proyecto fondo para vida, que incluyen: 

1. análisis multitemporal de huella humana (1970, 1990, 2000, 2015, 2018, 2019) : Huella_nucleos_base, Huella_nucleos_c_u, Huella_todos_nucleos
2. análisis de la integridad de bosque año 2019: Integridad_nucleos_base, Integridad_núcleos_c_u, Integridad_todos_nucleos
3. Gráficas tipo alluvium (Sanki), para mostrar las dinámicas de trasformación del paisaje a través del tiempo: alluvium_GUIDOS
  
En los temas 1 y 2 se calculan estadísticas zonales (promedio, mediana, desviación estándar) de los valores de huella y estadisticos zonales para obtener la frecuencias de categorias de intensidad de  IHEH e integridad de bosque. 

Los resultados se guardan en dos data frames:
Stat_values: Contiene estadísticas zonales (promedio, mediana, desviación estándar) para cada entidad de análisis y año .
Stat_reclass: Contiene la frecuencia y porcentaje de categorías de reclasificación para cada análisis y año.

En la última sección de los códigos las tablas se organizan para su exportación en formatos .csv y html para tener tablas interactivas que faciliten la exploración. Seguidamente se preparan y exportan gráficas de los datos que muestren la evolución de la IHEH a través de los años y el estado de la integridad para permita comparar  entre unidades de análisis.


## Organizar directorio de trabajo

Las entradas de ejemplo de este ejercicio están almacenadas en
[aquí](xxx).
Una vez descargadas y descomprimida, reemplaze la carpeta “Originales” en el directorio Datos del proyecto.
El directorio del proyecto está organizado de esta manera que facilita la ejecución del
código:

    Códigos
    │- Huella_nucleos_base 
    │- Huella_nucleos_c_u
    │- Huella_todos_nucleos
    │- Integridad_nucleos_base
    │- Integridad_núcleos_c_u
    │- Integridad_todos_nucleos
    │- alluvium_GUIDOS
    │    
    └-Datos
    │ │
    │ └- Originales: replaze aquí los datos que bajo
    │ │   │
    │ │   
    │ └- Intermedios
    │     │     
    |
    └- Resultados

