########################################################
##### Script test de Kruskal-Wallis #####
########################################################

#----
#Nombre del Script: Kruskalwallis.R
#by Camila Marín. Mayo 2021
#Contacto: camila.marin.a@mail.pucv.cl
#----

#Este Script se divide por las secciones de estaciones, profundidad, filtro, 
#ecorregión, masa de agua y cercania Isla Decepción.
#Cada sección calcula la significancia estadistica para los índices S.obs, Shannon 
#y Simpson

#El ÍNDICE del contenido de este Script:
#### (1) Estaciones. línea 25
#### (2) Profundidad. línea 41
#### (3) Filtros. línea 55
#### (4) Ecorregión línea 75
#### (5) Masa de agua. línea 91
#### (6) Cercania Isla Decepción. línea 105


#------------------ se viene

#Cargamos librerias a usar
library(agricolae)
library(readxl)
tabla <- read_excel("~/OCE/Practica/Datos/Listos/tabla_datos_ordenados.xlsx")

#### (1) Estaciones
#S.obs
compara<-kruskal(tabla$S.obs,tabla$estacion,group=TRUE, main="tabla")
compara
plot(compara)
#Shannon
compara2<-kruskal(tabla$Shannon,tabla$estacion,group=TRUE, main="tabla")
compara2
plot(compara2)
#Simpson
compara3<-kruskal(tabla$Simpson,tabla$estacion,group=TRUE, main="tabla")
compara3
plot(compara3)

### El punto en el plot es el valor promedio

#### (2) Profundidad
#S.obs
compara<-kruskal(tabla$S.obs,tabla$prof,group=TRUE, main="tabla")
compara
plot(compara)
#Shannon
compara2<-kruskal(tabla$Shannon,tabla$prof,group=TRUE, main="tabla")
compara2
plot(compara2)
#Simpson
compara3<-kruskal(tabla$Simpson,tabla$prof,group=TRUE, main="tabla")
compara3
plot(compara3)

#### (3) Filtros
#S.obs
compara<-kruskal(tabla$S.obs,tabla$filtro,group=TRUE, main="tabla")
compara
plot(compara)
#Shannon
compara2<-kruskal(tabla$Shannon,tabla$filtro,group=TRUE, main="tabla")
compara2
plot(compara2)
#Simpson
compara3<-kruskal(tabla$Simpson,tabla$filtro,group=TRUE, main="tabla")
compara3
plot(compara3)

##########

#### (4) Ecorregión
#S.obs
compara<-kruskal(tabla$S.obs,tabla$eco,group=TRUE, main="tabla")
compara
plot(compara)
#Shannon
compara2<-kruskal(tabla$Shannon,tabla$eco,group=TRUE, main="tabla")
compara2
plot(compara2)
#Simpson
compara3<-kruskal(tabla$Simpson,tabla$eco,group=TRUE, main="tabla")
compara3
plot(compara3)

### El punto en el plot es el valor promedio

#### (5) Masa de agua.
#S.obs
compara<-kruskal(tabla$S.obs,tabla$influ,group=TRUE, main="tabla")
compara
plot(compara)
#Shannon
compara2<-kruskal(tabla$Shannon,tabla$influ,group=TRUE, main="tabla")
compara2
plot(compara2)
#Simpson
compara3<-kruskal(tabla$Simpson,tabla$influ,group=TRUE, main="tabla")
compara3
plot(compara3)

#### (6) Cercania Isla Decepción.
#S.obs
compara<-kruskal(tabla$S.obs,tabla$decepcion,group=TRUE, main="tabla")
compara
plot(compara)
#Shannon
compara2<-kruskal(tabla$Shannon,tabla$decepcion,group=TRUE, main="tabla")
compara2
plot(compara2)

#Simpson
compara3<-kruskal(tabla$Simpson,tabla$decepcion,group=TRUE, main="tabla")
compara3
plot(compara3)
