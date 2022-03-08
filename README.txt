Hola! 

Soy Camila Marín, el presente trabajo se enmarca en mi práctica profesional, donde realice 
un analisis de datos del archivo OTU_Tab_nomitoch_nochloro_no18S_SOLO_BRANSFIELD, este archivo
no se encuentra actualmente disponible para circular públicamente, la fecha en la que se subirá
a este repositorio es indeterminada.
Los datos fueron recolectados entre el 8 de enero y el 24 de febrero de 2019 en 
la campaña “Multinational Large-Scale Krill Synoptic Survey, 2019”. 

El orden del trabajo fue el siguiente:

1_Script_GET_READY_with_TIDYVERSE_SOLO_BRANSFIELD.R -> Este Script fue realizado por mi 
supervisora de práctica. Es aquí donde se leen los datos, se seleccionan los datos a 
utilizar y se obtiene la matriz de OTUs rarefaccionada.

2_Ejercicio_diversidad.R -> En este script se realizan los calculos de ALPHA DIVERSIDAD y 
BETA DIVERSIDAD. Se obtiene la tabla "Tabla_datos_diversidad_alpha_ejercicio_1.txt" que 
contiene los índices de diversidad a utilizar con sus muestras respectivas.

3_orden_manual.txt -> Aquí se explican los pasos manuales que realicé para obtener la tabla
utilizada en el script "graficos.R", esta tabla contiene los datos recopilados de los índices 
S.obs, Shannon y Simpson con sus muestras respectivas y sus clasificaciones.

4_graficos.R -> En este script se realizaron graficos para los índices S.obs, Shannon y
Simpson calculados entre las distintas estaciones, profundidades y fracciones. Estos fueron
gráficos con valores agregados por factor (gráficos de caja) y con valores disgregados 
(gráficos de puntos). 

5_Kruskalwallis.R -> En este script se obtuvieron las diferencias estadísticamente 
significativas entre los boxplots (que representan las estaciones, las profundidades y las 
fracciones de tamaño en los 3 índices utilizados), para esto se ocupó el test de 
Kruskal-Wallis.


Lista de los archivos utilizados en el trabajo: 
-1_Script_GET_READY_with_TIDYVERSE_SOLO_BRANSFIELD.R
-2_Ejercicio_diversidad.R
-3_orden_manual.txt
-4_graficos.R
-5_Kruskalwallis.R
-KH_metadata_estaciones_v3_SOLO_BRANSFIELD
-OTUsot
-README
-Tabla_datos_diversidad_alpha
-tabla_datos_ordenados


Archivo creado en febrero 2022
última versión: 08-03-2022
