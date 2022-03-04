########################################################
##### Script DE INICIO MANEJO OTU TABLE BRANSFIELD #####
########################################################

#----
#Nombre del Script: Script_GET_READY_with_TIDYVERSE_SOLO_BRANSFIELD.R
#by Mireia Mestre. Febrero 2021
#Contacto: mireia.mestre.martin@gmail.com
#Comentarios: Tomo como referencia el tutorial de Marta Masdeu que hizo en el 
#curso del IDAEA. Curso impartido por Daniel Lundin.
#----

#Este Script va a ser el SCRIPT INICIAL de nuestro proyecto.
#Dentro de nuestro Proyecto de R, nos organizaremos de manera que tendremos:
#-un script inicial para cargar y preparar datos 
#-varios scripts para analizar datos.
#Tendremos un script por cada tipo de análisis (por ejemplo, Scripts para 
#boxplots diversidad, Script para nMDS...). 
#Cada vez que ejecutemos los scripts de análisis de datos, 
#ejecutaremos antes este script de inicio, donde limpiamos la sesión de R y
#cargamos y preparamos las tablas de datos.

#Principalmente, lo que hacemos en este Script es importar los datos y ordenar 
#data.frames según el package Tidyverse. 

#Vamos a crear data.frames de dos tipos:
#Primero vamos a construir data.frames que son útiles para manejar con el 
#package Tidyverse. El manejo de datos con Tidyverse es cada vez más común.
#Al final del script vamos a construir data.frames tradicionales, pues se siguen
#utilizando mucho. 


#El ÍNDICE del contenido de este Script:
#### (1) Read data
#### (2) SELECCIONAMOS QUÉ DATOS QUEREMOS
#### (3) ELIMINAR MUESTRAS POCO ABUNDANTES Y RAREFACCIONAR
#### (4) DEFINIMOS TABLAS DATOS PARA PRÓXIMOS ANÁLISIS



#------------------ se viene

#limpiamos la sesión:
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

#Anotamos qué versión tenemos de R:
version
#version.string R version 4.0.3 (2020-10-10)
#nickname       Bunny-Wunnies Freak Out  

#Vemos dónde estamos:
getwd()
#Aquí vemos los archivos que tenemos en nuestro directorio:
dir()
#Para este script, comprobamos que tenemos los archivos:
#La OTU TAble:
#OTU_Tab_nomitoch_nochloro_no18S_SOLO_BRANSFIELD.txt
#La tabla ambiental:
#KH_metadata_estaciones_v3_SOLO_BRANSFIELD.txt"
#La taxonomía:
#taxonomy.txt 



#### (1) Read data
#First, we need to read the data, our case three tab separated tables

#### (a) Leemos la OTU table
counts <- read_tsv(
  "OTU_Tab_nomitoch_nochloro_no18S_SOLO_BRANSFIELD.txt",
  col_types = cols(
    .default = col_integer(),
    OTU = col_character()
  ))
#Vemos que la table tiene 7465 obs. of 79 variables 
#Lo que es lo mismo: 7465 filas y 79 columnas
#Lo que es lo mismo: 7465 OTUs y 79 muestras

#Leemos de nuevo la OTU table y ahora hacemos un "gather" (con package tidyr) 
#para tener la tabla en formato largo.
#El formato largo nos será útil más en adelante.
#La tabla larga tendrá sólo tres columnas: "OTU"; "sample"; "count".
counts <- read_tsv(
  "OTU_Tab_nomitoch_nochloro_no18S_SOLO_BRANSFIELD.txt",
  col_types = cols(
    .default = col_integer(),
    OTU = col_character()
  ))%>%
  # Make the table long. Como la tabla que hemos leído tiene 79 columnas, 
  #apuntamos el valor 79 en la función "gather"
  gather(sample, count, 2:84)  %>%
  # Filter so we only have rows with count > 0. Aquí quitamos las filas que 
  #tienen valor 0 (es decir aquellos OTUs que no aparecen en ciertas muestras). 
  filter(count > 0) #%>%
# Filter out observations from samples smaller than 2000 counts in total. ESTO NO VA
#group_by(sample) %>% filter(sum(count) >= 2000) %>% ungroup()

#El resultado de organizar con gather es una tabla:
#10685 obs. of 3 variables 



#### (b) Leemos la matriz metadatos ambientales
samples <- read_tsv(
  "KH_metadata_estaciones_v3_SOLO_BRANSFIELD.txt",
  col_types = cols(
    .default = col_double(),
    SAMPLE_v1 = col_character(),
    SAMPLE_TYPE = col_character(),
    STATION = col_character(),
    AREA = col_character(),
    DEPTH = col_character(),
    SIZE_FRACTION = col_character(),
    SIZE_FRACTION_num = col_character()
  )
)

#### (c) Leemos la matriz de taxonomía
taxa <- read_tsv(
  'taxonomy.txt',
  col_types = cols(.default = col_character())
) 

#Esta parte es para cuando la taxonomía no viene separada por columnas. Nosotros 
#ya la tenemos separada en columnas, y entonces este paso no es necesario. Lo 
#dejo anotado por si algún dia puede ser de utilidad%>%
# Get rid of the D_0__ etc at the start of each taxon level
#mutate(Taxon = gsub('D_[0-9]__', '', Taxon)) %>%
# Separate the joined taxonomy into known pieces
#separate(Taxon, sep = ';', c('domain', 'phylum', 'class', 'order', 'family', 'genus'), fill = 'right', extra = 'drop')




#### (2) SELECCIONAMOS QUÉ DATOS QUEREMOS

#ESTE PASO SÓLO CUANDO SE QUIERA HACER UN SUBMUESTREO.
#(se pueden seleccionar, por ejemplo, ciertas estaciones o profundidades)

#En el ejemplo seleccionamos de nuevo las muestras BRANSFIELD, así que es totalmente
#redundante.. Lo dejo anotado igual, por si estas líneas son útiles en el
#futuro. 

#Vamos a seleccionar muestras sólo del transecto BRANSFIELD se llamará counts_BRANSFIELD
#Para ello, primero juntamos las matrices de counts y samples
counts_and_samples <- counts %>%
  inner_join(samples, by = c('sample' = 'SAMPLE_v1')) 
#Ahora seleccionamos las muestras del TRANSECTO BRANSFIELD
counts_and_samples_only_BRANSFIELD<-filter(counts_and_samples, AREA=="BRANSFIELD")
#Ahora seleccionamos las columans OTU, sample y count, para así quedarnos con 
#una matriz tipo counts con sólo muestras de BRANSFIELD
counts_BRANSFIELD<-select(counts_and_samples_only_BRANSFIELD, OTU, sample, count)#Este 
#sí funciona



#### (3) ELIMINAR MUESTRAS POCO ABUNDANTES Y RAREFRACCIONAR

#### (a) Queremos saber cuántos reads hay por muestra. 
#Para eso, dibujaremos un plot donde salga Y=sample X= counts per sample:
counts_BRANSFIELD %>% 
  # Calculate a summed count per sample
  group_by(sample) %>% summarise(count = sum(count)) %>% ungroup() %>%
  # Use ggplot to plot a point plot with samples along the x axis and
  # count on the y.
  ggplot(aes(x = sample, y = count)) +
  geom_point() +
  xlab("samples") +
  ylab("num sequences") +
  # Flip x and y axes
  coord_flip() +
  labs(title="num sequences per sample",subtitle="KH BRANSFIELD")


#Repetimos el plot, ahora con las muestras ordenadas de mayor a menor
counts_BRANSFIELD %>% group_by(sample) %>% summarise(count = sum(count)) %>% arrange (desc(count)) %>% ungroup()%>%
  arrange(count) %>%    # First sort by val. This sort the dataframe but NOT the factor levels
  mutate(sample=factor(sample, levels=sample)) %>%   # This trick update the factor levels
  ggplot( aes(x=sample, y=count)) +
  geom_segment( aes(xend=sample, yend=0)) +
  geom_point( size=1, color="black") +
  theme_bw() +
  xlab("")+
  coord_flip() +
  labs(title="num sequences per sample",subtitle="KH BRANSFIELD")

#Podemos además saber min, máx y promedio con:
min(counts_BRANSFIELD$count)
max(counts_BRANSFIELD$count)
mean(counts_BRANSFIELD$count)


#### (b) Delete too small libraries

#Aquí definimos el número mínimo de secuencias que va a tener cada muestra, y 
#este valor después nos servirá para el paso de rarefraccionar. 
#Este valor mínimo es un compromiso entre el número de muestras que mantengo
#y número de secuencias por muestra (es decir, entre eliminar pocas muestras y 
#tener muchas secuencias por muestra). 
#El plot anterior nos ayuda a definir este límite. También nos ayudaría dibujar 
#la rarefraction curve (no está incluido en este script).

#Probamos este valor:
MIN_LIB_SIZE = 1000  #AQUI Cambiar este valor para ir probando. Probar, por 
#ejemplo, 1000, 2000, 3000, 5000 secuencias, según el número de secuencias que 
#tengamos (ver abajo).

counts_BRANSFIELD_v2 <- counts_BRANSFIELD %>% group_by(sample) %>% mutate(sum_count = sum(count)) %>% ungroup() %>%
  filter(sum_count > MIN_LIB_SIZE) %>% select(-sum_count)

#Aqui miniscript para calcular num muestras antes y num muestras después de 
#marcar límite inferior
counts_BRANSFIELDx_inic<-spread(data = counts_BRANSFIELD, key = sample, value = count) 
#Son 79 muestras antes de marcar límite
counts_BRANSFIELDx_final<-spread(data = counts_BRANSFIELD_v2, key = sample, value = count) 
#Son 71 muestras. 

#Otra manera más sencilla de verlo:
counts_BRANSFIELDx_inic_v2<-counts_BRANSFIELD %>% group_by(sample) %>% summarise(count = sum(count)) 
counts_BRANSFIELDx_final_v2<-counts_BRANSFIELD_v2 %>% group_by(sample) %>% summarise(count = sum(count)) 

str(counts_BRANSFIELDx_final_v2)
min(counts_BRANSFIELDx_final_v2$count)

#OJO que aunque le indiquemos MIN_LIB_SIZE = 1000, el valor mínimo de 
#secuencias que tenemos (y al que después rarefraccionamos) en realidad es 
#el valor mínimo de secuencias de aquella muestra que tiene almenos 1000 
#secuencias. Esto es, en este caso, 1158 secuencias. 

#BRANSFIELD tiene 79 muestras, si marcamos el min a:
#5000 seqs, nos quedamos con  55 muestras = 69%
#4000 seqs, nos quedamos con  60 muestras = 75%
#3500 seqs, nos quedamos con  61 muestras = 77%
#3000 seqs, nos quedamos con  62 muestras = 78%
#2500 seqs, nos quedamos con  63 muestras = 79%
#2000 seqs, nos quedamos con  64 muestras = 81%
#1000 seqs, nos quedamos con  70 muestras = 89% de las muestras

counts_BRANSFIELD_vX <- counts_BRANSFIELD %>% group_by(sample) 

#NOTA:tambien hay que tener en cuenta CUÁLES son las muestras que se eliminan 
#(si son muestras totalmente prescindibles las podemos eliminar sin problema, 
#pero si son imporantes, conviene mantenerlas). 
#Cómo de importantes son ciertas muestras se sabe muchas veces a posteriori, 
#cuando ya hemos hecho muchos plots. 
#Por eso, es recomendable empezar los análisis con el mayor número de muestras
#posible, y una vez tengamos muchos plots y conozcamos bien nuestras muestras, 
#podremos valorar qué muestras podemos eliminar para subir el número de reads 
#por muestra.
#Entonces, por ahora, sigamos los análisis con 1000 reads por muestra.


#### (c) Rarefy

#Se rarefacciona al mínimo que hemos definido en el apartado anterior
min_lib_size <- counts_BRANSFIELD_v2 %>%
  group_by(sample) %>% summarise(count = sum(count)) %>%
  summarise(mincount = min(count)) %>% ungroup() %>%
  pull(mincount)


r <- counts_BRANSFIELD_v2 %>%
  select(OTU, sample, count) %>%
  spread(OTU, count, fill = 0) %>%
  data.frame() %>% tibble::column_to_rownames('sample') %>%
  rrarefy(min_lib_size) %>%
  data.frame() %>% tibble::rownames_to_column('sample') %>%
  gather(OTU, rcount, 2:ncol(.)) %>%
  filter(rcount > 0)

#Aqui obtenemos la tabla de counts, ahora con la columna de valores 
#rarefaccionados (rcount)
counts_BRANSFIELD_v3 <- counts_BRANSFIELD_v2 %>%
  left_join(r, by = c('sample', 'OTU')) %>%
  replace_na(list('rcount' = 0))

#Aqui podemos comprobar en un plot que todas las muestras están rarefaccionadas
counts_BRANSFIELD_v3 %>% 
  # Calculate a summed count per sample
  group_by(sample) %>% summarise(rcount = sum(rcount)) %>% ungroup() %>%
  # Use ggplot to plot a point plot with samples along the x axis and
  # count on the y.
  ggplot(aes(x = sample, y = rcount)) +
  geom_point() +
  xlab("samples") +
  ylab("num sequences") +
  # Flip x and y axes
  coord_flip() +
  labs(title="num sequences per sample",subtitle="KH BRANSFIELD RAREFY")

#OJO recordemos de nuevo que, aunque le indiquemos MIN_LIB_SIZE = 1000, 
#el valor mínimo de secuencias (al que rarefraccionamos), es el valor mínimo de 
#secuencias de aquella muestra que tiene almenos 1000 secuencias.
#Esto es, en este caso, 1189 secuencias, y lo vemos si:
min(counts_BRANSFIELDx_final_v2$count)



#### (4) DEFINIMOS TABLAS DATOS PARA PRÓXIMOS ANÁLISIS

#Matriz ambiental
ENV<-samples

#Matriz taxonomía 
TAX<-taxa

#Matriz OTUs rarefaccionada para Tidyverse:
counts_BRANSFIELD_v4<-select(counts_BRANSFIELD_v3, OTU, sample,rcount)
OTUs<-counts_BRANSFIELD_v4

#Matriz OTUs unida a matriz ambiental para Tidyverse
#Para ello, primero juntamos las matrices de counts y samples
OTUs_and_ENV <- OTUs %>%
  inner_join(ENV, by = c('sample' = 'SAMPLE_v1')) 

#Matriz OTUs unida a matriz ambiental unida a matriz taxa para Tidyverse
OTUs_and_ENV_and_TAXA <- OTUs_and_ENV %>%
  inner_join(TAX, by = c('OTU' = 'OTU')) 


#Matriz OTUs rarefaccionada para uso tradicional (old style):
counts_BRANSFIELD_v5<-spread(data = counts_BRANSFIELD_v4, key = sample, value = rcount)
counts_BRANSFIELD_v5[is.na(counts_BRANSFIELD_v5)] <- 0
OTUso<-counts_BRANSFIELD_v5
OTUso<-as.data.frame(OTUso)
#View(OTUst)
rownames(OTUso)<-OTUso$OTU
#View(OTUso)
#Ahora sacamos los, Row.names duplicated:
OTUso<-OTUso[,-1] 
#La OTUst no debe tener ninguna fila o columna con sólo valores 0.
dim(OTUso)
OTUsot<-t(OTUso)
otu2<-OTUsot[,colSums(OTUsot)>0]
dim(otu2)
OTUso<-t(otu2)


#Matriz ambiental para uso tradicional (old style)
ENVo<-samples
ENVo<-as.data.frame (ENVo)
#View(ENVo)
rownames(ENVo)<-ENVo$SAMPLE_v1
#View(ENVo)
ENVo<-ENVo[,-1] 


#Matriz taxonomía para uso tradicional (old style)
TAXo<-taxa
TAXo<-as.data.frame (TAXo)
#View(TAXo)
rownames(TAXo)<-TAXo$OTU
#View(TAXo)
TAXo<-TAXo[,-1] 


#--->RESUMIENDO, con este Script hemos obtenido:
#*************************************************************************
ENV #Matriz ambiental para Tidyverse
TAX #Matriz taxonomía para Tidyverse
OTUs #Matriz OTUs rarefaccionada para Tidyverse
OTUs_and_ENV #Matriz OTUs unida a matriz ambiental para Tidyverse
OTUs_and_ENV_and_TAXA #Matriz OTUs unida a matriz ambiental unida a matriz taxa para Tidyverse
OTUso #Matriz OTUs rarefaccionada para uso tradicional (old style)
ENVo #Matriz ambiental para uso tradicional (old style)
TAXo #Matriz taxonomía para uso tradicional (old style)
#*************************************************************************





#### PASO EXTRA ####
##LO SIGUIENTE ES NECESARIO SÓLO SI EN EL PASO (2) HEMOS SELECCIONADO UNOS 
#DATOS EN CONCRETO
##Si en el paso 2 hemos seleccionado unas muestras en concreto, la ENVo debería 
#incluir sólo esas muestras. A estas alturas del Script, lo que tenemos es que
#la ENVo incluye todas las muestras. Entonces, OTUso y ENVo no encajan bien. 
#Para que OTUs y ENV encajen bien:

#Primero unimos ENVo y OTUso y luego los separamos, así sabemos que las 
#dimensiones de cada uno están bien.
OTUsot<-t(OTUso)
ENVo_OTUso<- merge(ENVo, OTUsot, by.x="row.names", by.y="row.names",sort="FALSE")
rownames(ENVo_OTUso)<-ENVo_OTUso$Row.names
#Sacamos los Row.names duplicated:
ENVo_OTUso<-ENVo_OTUso[,-1]
ENVo_OTUso<-ENVo_OTUso

#Nos quedamos sólo con la tabla de OTUs. IMPORTANTE: El formato de la tabla de 
#OTUS: los otus en columna y muestras en filas
OTUso2<-ENVo_OTUso[,-(1:6)]

#Nos quedamos sólo con ENVo
ENVo2<-ENVo_OTUso[,(1:6)]



#### PASO EXTRA ####
####EJEMPLO AMPLIACIÓN (por si es útil)
#Si observamos que, por ejemplo, podemos clasificar las estaciones en dos tipos
#Por ejemplo, estaciones norte y estaciones sur, podemos crear una columna 
#nueva así (cambia el num de estación según tus muestras): 
#Creamos nueva columna:
ENVo2$STATION2<-ENVo2$STATION
#La queremos numérica
ENVo2$STATION2<-as.numeric(ENVo2$STATION2)
ENVo2[ENVo2$STATION=="P33",]$STATION2<-"SOUTH"
ENVo2[ENVo2$STATION=="P34",]$STATION2<-"SOUTH"
ENVo2[ENVo2$STATION=="P35",]$STATION2<-"NORTH"
ENVo2[ENVo2$STATION=="P36",]$STATION2<-"NORTH"
ENVo2[ENVo2$STATION=="P37",]$STATION2<-"NORTH"
ENVo2[ENVo2$STATION=="P38",]$STATION2<-"NORTH"
ENVo2[ENVo2$STATION=="P39",]$STATION2<-"NORTH"
ENVo2[ENVo2$STATION=="P40",]$STATION2<-"NORTH"
ENVo2[ENVo2$STATION=="P51",]$STATION2<-"SOUTH"
#Y ahora queremos como factor
ENVo2$STATION2<-as.factor(ENVo2$STATION2)

