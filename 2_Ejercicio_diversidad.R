########################################################
##### Script de inicio, �ndices de diversidad #####
########################################################

#----
#Nombre del Script: Ejercicio_diversidad.R
#by Camila Mar�n. Marzo 2021
#Contacto: camila.marin.a@mail.pucv.cl
#Comentarios: Tomo como referencia el tutorial realizado por mi profesora 
#supervisora Mireia Mestre
#----

#Este Script contiene los c�lculos de diversidad realizados para las muestras de la campa�a.
#Si bien en este script calculamos 6 �ndices, en los pasos posteriores nos centraremos en 3:
#S.obs, Shannon y Simpson.


#El �NDICE del contenido de este Script:
#### (1) LIMPIAMOS SESI�N Y CARGAMOS LIBRERIAS. l�nea 28
#### (2) CALCULAMOS ALPHA DIVERSIDAD. l�nea 43
#### (3) CALCULAMOS BETA DIVERSIDAD. l�nea 107



#------------------ se viene

#### (1) limpiamos la sesi�n:
ls()
rm(list=ls())
ls()

#fijamos valor random:
set.seed(1234)

#Cargamos librerias a usar
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(vegan)

#### (2) Calculamos ALPHA DIVERSIDAD
#Diversidad ALPHA (diversidad asociada a una comunidad/sitio/muestra en concreto)

#Vemos la riqueza (=n�mero de especies) de cada muestra con la función estimateR
richness<-estimateR(OTUsot)
richness
#diversidad alpha (=riqueza=n�mero de especies) es, b�sicamente, el 
#n�mero de especies total en una muestra (S.obs)

#Otras maneras de calcular alpha diversidad: La diversidad puede tener en cuenta
#no s�lo el n�mero de especies, sino tambi�n su abundancia (eveness). 
#Indice de Shannon (H) (considera richness and evenness). Te da un valor que es
#para comparar entre muestras. No es el numero de especies
Shannon<-diversity(OTUsot,index="shannon", MARGIN=1, base=exp(1))
Shannon
#Indice de Simpson (D) (considera richness and evenness). Te da un valor que es
#para comparar entre muestras. No es el numero de especies. Aumenta  con el 
#eveness
Simpson <-diversity(OTUsot,"simpson")
Simpson
#Todos estos valores los vamos guardando como una tabla:
tab_richness<-as.data.frame(richness)
tab_richness2<-t(tab_richness)
tab_shannon<-as.data.frame(Shannon)
tab_simpson<-as.data.frame(Simpson)
#Juntamos todos los datos en una misma tabla:
tab_richness_shannon<- merge(tab_richness2, tab_shannon, by.x="row.names", by.y="row.names",sort="FALSE")
tab_richness_shannon_simpson<- merge(tab_richness_shannon, tab_simpson, by.x="Row.names", by.y="row.names",sort="FALSE")
#y ahora lo podemos sacar a nuestra lista de archivos, como un ".txt":
write.table(tab_richness_shannon_simpson, file="Tabla_datos_diversidad_alpha.txt", col.names=NA,row.names=TRUE, sep="\t", quote=FALSE)

#Calulamos la Curva rarefacci�n con Vegan
raremax <- min(rowSums(OTUsot))
Srare <- rarefy(OTUsot, raremax)
S <- specnumber(OTUsot)
rarecurve(OTUsot, step = 20, sample = raremax, cex = 0.5)

#Calulamos la curva de rarefacci�n con Vegan, ahora con la OTU table sin 
#rarefraccionar, y comparamos con el polt anterior
#raremax <- min(rowSums(OTUt))
#Srare <- rarefy(OTUt, raremax)   
#S <- specnumber(OTUt)
#rarecurve(OTUt, step = 20, sample = raremax, cex = 0.5)


#Podemos calcular la Curva de acumulaci�n de especies (Species accumulation
#curves. Por ejemplo usando el método de Ugland:
CC_CURVugland<-specaccum(OTUsot,method="exact", permutations=100)
##Warning message: In cor(x > 0) : the standard deviation is zero
CC_CURVugland
plot(CC_CURVugland)
plot(CC_CURVugland, xlab = "Num muestras", ylab = "Num especies")

#La Curva rank-abundance
#En la curva rank abundance, ordenamos las especies desde la m�s abundante a la 
#menos abundante. En microbiolog�a se da una curva caracter�stica: es una curva
#logar�tmica, donde hay unos pocos muy abundantes, y muchos muy poco abundantes.
#Los poco abundantes tienden a 0 y conforman la llamada "rare biosphere".
#R tiene una funci�n para dibujar estas curvas: con radfit
mod<-radfit(OTUsot)
plot(mod)
modsum1<-radfit((OTUsot[1,]))
plot(modsum1)

#### (3) Calculamos BETA DIVERSIDAD
#Calculamos la matriz de distancias Bray-Curtis (Bray-Curtis dissimilarity 
#matrix) con la funci�n vegdist:
OTUsot.bray<-vegdist(OTUsot, method="bray")
OTUsot.bray
head(OTUsot.bray)
table(OTUsot.bray)

#Visualizamos las distancias Bray-Curtis con el an�lisis NMDS (non-metric 
#multidimensional scaling)
OTUsot.bray.nmds<-monoMDS(OTUsot.bray)
OTUsot.bray.nmds
par(cex=0.7) #size of the plot letter
plot(OTUsot.bray.nmds)
#Las muestras mientras m�s cercanas m�s parecidas

#Calculamos la matriz de distancias UPGMA (Unweighted Pair Group Method with 
#Arithmetic Mean) con la funci�n hclust. Esta funci�n ya deja los datos
#preparados para su posterior visualización en modo cluster:
OTUsot.bray.hclust<-hclust(OTUsot,"average")
OTUsot.bray.hclust
#Visualizamos las distancias UPGMA con un hierarchical clustering
plot(OTUsot.bray.hclust)
#En el cluster, muestras m�s cercanas son m�s parecidas.

#Si con el nmds y el cluster vemos c�mo de parecidas-diferentes son las
#muestras seg�n sus comunidades, tambi�n podemos ver c�mo de parecidas-
#diferentes son las comunidades seg�n su distribuci�n en las muestras (es decir,
#su co-ocurrencia o co-variaci�n entre muestras). Esto lo podemos ver con el 
#Detrended Correspondende Analysis (DCA) (similar al PCA pero quiz�s m�s robusto)
DCA<-decorana(OTUsot)
plot(DCA,display="sites") #Vemos las diferencias entre muestras
DCA
plot(DCA,display="species") #Vemos las diferencias entre otus
DCA