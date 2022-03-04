########################################################
##### Script graficos de caja y gráficos de puntos. #####
########################################################

#----
#Nombre del Script: graficos.R
#by Camila Marín. Abril 2021
#Contacto: camila.marin.a@mail.pucv.cl
#----

#Este Script comienza con un filtrado de datos, esto se realiza por filtro, profundidad
#por ecorregión, por la influencia de la masa de agua y la cercanía a la Isla Decepcion. 
#Este script se separá por los 3 índices de diversidad. Primero se realizan los gráficos 
#de caja y luego los gráficos de puntos de un mismo índice, terminado se pasa al otro índice.
#Este script tiene más de 60 gráficos, por lo que no se recomienda correr completo de una 
#pasada

#El ÍNDICE del contenido de este Script:
#### (1) FILTRADO INICIAL. línea 31
#### (2) GRÁFICO DE CAJA PARA S.obs. línea 73
#### (3) GRÁFICO DE CAJA PARA Shannon línea 345
#### (4) GRÁFICO DE CAJA PARA Simpson línea 624

#------------------ se viene

#Cargamos librerias a usar
library(ggplot2)
library(dplyr)
library(readxl)
library(pgirmess)
library(stats)
library(vegan)

#Leemos datos que recopilan los indices realizados en Ejercicio diversidad
 
tabla_box <- read_excel("~/OCE/Practica/Datos/tabla_datos_ordenados")

#### (1) Filtramos datos a usar

### por filtros ###
#0.2 pico
filtro1<-filter(tabla_box,filtro=="F1")
#3 nano
filtro2<-filter(tabla_box,filtro=="F2")
#20 micro
filtro3<-filter(tabla_box,filtro=="F3")

###por profundidades ###
# 5 metros
prof5<-filter(tabla_box,profundidad==5)
# 30 metros
prof30<-filter(tabla_box,profundidad==30)
# 100 metros
prof100<-filter(tabla_box,profundidad==100)
# 200 metros
prof200<-filter(tabla_box,profundidad==200)
# >200 metros
tabla_box$profundidad<-as.numeric(tabla_box$profundidad) #transformamos para reconocer el numero
# deep
profdeep<-filter(tabla_box,profundidad>201)


## transformamos a caracter filtro y profundidad
tabla_box$filtro<-as.character(tabla_box$filtro)
tabla_box$profundidad<-as.character(tabla_box$profundidad)

### por sitios ###
# ecorregion
eco<-select(tabla_box,eco,S.obs,Shannon,Simpson)

# Masa de agua
influ<-select(tabla_box,influ,S.obs,Shannon,Simpson)

# Isla decepción
decepcion<-select(tabla_box,decepcion,S.obs,Shannon,Simpson)

                            ######## GRAFICOS DE CAJAS ###########
#### (2) S.obs
# REALIZAMOS BOXPLOT CON LOS 3 FILTROS
theme_set(theme_bw())
f <- ggplot(tabla_box, aes(filtro, S.obs))
f + geom_boxplot() + 
  geom_dotplot(binaxis='y', 
               stackdir='center', 
               dotsize = .50, 
               fill="red") +
  theme(axis.text.x = element_text(angle=0, vjust=0.6)) + 
  labs(title="Diversidad filtros", 
       subtitle="Box plot + Dot plot",
       x="Filtro",
       y="S.obs")+scale_y_continuous(limits = c(30,270))

# REALIZAMOS BOXPLOT PARA FILTRO 1 EN TODAS LAS ESTACIONES
a <- ggplot(filtro1, aes(estacion, S.obs))
a + geom_boxplot() + 
  geom_dotplot(binaxis='y', 
               stackdir='center', 
               dotsize = .50, 
               fill="red") +
  theme(axis.text.x = element_text(angle=0, vjust=0.6)) + 
  labs(title="Diversidad filtro 1", 
       subtitle="Box plot + Dot plot",
       x="Estaciones",
       y="S.obs") +scale_y_continuous(limits = c(50,220))

#filtro2
b <- ggplot(filtro2, aes(estacion, S.obs))
b + geom_boxplot() + 
  geom_dotplot(binaxis='y', 
               stackdir='center', 
               dotsize = .50, 
               fill="red") +
  theme(axis.text.x = element_text(angle=0, vjust=0.6)) + 
  labs(title="Diversidad filtro 2", 
       subtitle="Box plot + Dot plot",
       x="Estaciones",
       y="S.obs")+scale_y_continuous(limits = c(30,270))


#filtro3
c <- ggplot(filtro3, aes(estacion, S.obs))
c + geom_boxplot() + 
  geom_dotplot(binaxis='y', 
               stackdir='center', 
               dotsize = .50, 
               fill="red") +
  theme(axis.text.x = element_text(angle=0, vjust=0.6)) + 
  labs(title="Diversidad filtro 3", 
       subtitle="Box plot + Dot plot",
       x="Estaciones",
       y="S.obs") +scale_y_continuous(limits = c(50,220))



######### REALIZAMOS GRAFICO DE TODAS LAS PROFUNDIDADES
p <- ggplot(tabla_box, aes(prof, S.obs))
p + geom_boxplot() + 
  geom_dotplot(binaxis='y', 
               stackdir='center', 
               dotsize = .50, 
               fill="red") +
  theme(axis.text.x = element_text(angle=0, vjust=0.6)) + 
  labs(title="Diversidad profundidades", 
       subtitle="Box plot + Dot plot",
       x="profundidad (m)",
       y="S.obs") + scale_x_discrete (limits= c("5","20","30","100","200","deep"))

#profundidad 5
d <- ggplot(prof5, aes(estacion, S.obs))
d + geom_boxplot() + 
  geom_dotplot(binaxis='y', 
               stackdir='center', 
               dotsize = .50, 
               fill="red") +
  theme(axis.text.x = element_text(angle=0, vjust=0.6)) + 
  labs(title="Diversidad profundidad 5 metros", 
       subtitle="Box plot + Dot plot",
       x="Estaciones",
       y="S.obs")+scale_y_continuous(limits = c(30,270))

#profundidad 30
e <- ggplot(prof30, aes(estacion, S.obs))
e + geom_boxplot() + 
  geom_dotplot(binaxis='y', 
               stackdir='center', 
               dotsize = .50, 
               fill="red") +
  theme(axis.text.x = element_text(angle=0, vjust=0.6)) + 
  labs(title="Diversidad profundidad 30 metros", 
       subtitle="Box plot + Dot plot",
       x="Estaciones",
       y="S.obs")+scale_y_continuous(limits = c(30,270))

#profundidad 100
i <- ggplot(prof100, aes(estacion, S.obs))
i + geom_boxplot() + 
  geom_dotplot(binaxis='y', 
               stackdir='center', 
               dotsize = .50, 
               fill="red") +
  theme(axis.text.x = element_text(angle=0, vjust=0.6)) + 
  labs(title="Diversidad profundidad 100 metros", 
       subtitle="Box plot + Dot plot",
       x="Estaciones",
       y="S.obs")+scale_y_continuous(limits = c(30,270))


#profundidad 200
g <- ggplot(prof200, aes(estacion, S.obs))
g + geom_boxplot() + 
  geom_dotplot(binaxis='y', 
               stackdir='center', 
               dotsize = .50, 
               fill="red") +
  theme(axis.text.x = element_text(angle=0, vjust=0.6)) + 
  labs(title="Diversidad profundidad 200 metros", 
       subtitle="Box plot + Dot plot",
       x="Estaciones",
       y="S.obs")+scale_y_continuous(limits = c(30,270))

#estaciones
j <- ggplot(tabla_box, aes(estacion, S.obs))
j + geom_boxplot() + 
  geom_dotplot(binaxis='y', 
               stackdir='center', 
               dotsize = .50, 
               fill="red") +
  theme(axis.text.x = element_text(angle=0, vjust=0.6)) + 
  labs(title="Diversidad estaciones", 
       subtitle="Box plot + Dot plot",
       x="Estaciones",
       y="S.obs")+scale_y_continuous(limits = c(30,270))

# P02
P02<-filter(tabla_box,estacion=="P02")
k <- ggplot(P02, aes(profundidad, S.obs,shape=filtro))+ geom_dotplot(binaxis='y', 
               stackdir='center', 
               dotsize = .50, 
               fill="red") +
  theme(axis.text.x = element_text(angle=0, vjust=0.6)) + 
  labs(title="Estación P02", 
       subtitle="Diversidad por profundidades y filtros",
       x="profundidad (m)",
       y="S.obs")
k + geom_point(aes(colour = filtro), size = 6) + geom_point(colour = "black", size = 2) +scale_x_discrete (limits= c("5","30","100"))+scale_y_continuous(limits = c(0,300))


# P01
P01<-filter(tabla_box,estacion=="P01")
l <- ggplot(P01, aes(profundidad, S.obs,shape=filtro)) + geom_dotplot(binaxis='y', 
               stackdir='center', 
               dotsize = .50, 
               fill="red") +
  theme(axis.text.x = element_text(angle=0, vjust=0.6)) + 
  labs(title="Estación P01", 
       subtitle="Diversidad por profundidades y filtros",
       x="profundidad (m)",
       y="S.obs")
l + geom_point(aes(colour = filtro), size = 6) + geom_point(colour = "black", size = 2) +scale_x_discrete (limits= c("5","30","100"))+scale_y_continuous(limits = c(0,300))

# P04
P04<-filter(tabla_box,estacion=="P04")
m <- ggplot(P04, aes(profundidad, S.obs,shape=filtro))+ geom_dotplot(binaxis='y', 
               stackdir='center', 
               dotsize = .50, 
               fill="red") +
  theme(axis.text.x = element_text(angle=0, vjust=0.6)) + 
  labs(title="Estación P04", 
       subtitle="Diversidad por profundidades y filtros",
       x="profundidad (m)",
       y="S.obs")
m + geom_point(aes(colour = filtro), size = 6) + geom_point(colour = "black", size = 2) +scale_x_discrete (limits= c("5","30"))+scale_y_continuous(limits = c(0,300))

# P56
P56<-filter(tabla_box,estacion=="P56")
n <- ggplot(P56, aes(profundidad, S.obs,shape=filtro)) + geom_dotplot(binaxis='y', 
               stackdir='center', 
               dotsize = .50, 
               fill="red") +
  theme(axis.text.x = element_text(angle=0, vjust=0.6)) + 
  labs(title="Estación P56", 
       subtitle="Diversidad por profundidades y filtros",
       x="profundidad (m)",
       y="S.obs")
n + geom_point(aes(colour = filtro), size = 6) + geom_point(colour = "black", size = 2) +scale_x_discrete (limits= c("5","30","200","600","1000"))+scale_y_continuous(limits = c(0,300))

# Mx
Mx<-filter(tabla_box,estacion=="Mx")
o <- ggplot(Mx, aes(profundidad, S.obs,shape=filtro)) + geom_dotplot(binaxis='y', 
               stackdir='center', 
               dotsize = .50, 
               fill="red") +
  theme(axis.text.x = element_text(angle=0, vjust=0.6)) + 
  labs(title="Estación Mx", 
       subtitle="Diversidad por profundidades y filtros",
       x="profundidad (m)",
       y="S.obs")
o + geom_point(aes(colour = filtro), size = 6) + geom_point(colour = "black", size = 2) +scale_x_discrete (limits= c("5","30","100","200","830"))+scale_y_continuous(limits = c(0,300))


# M5
M5<-filter(tabla_box,estacion=="M5")
q <- ggplot(M5, aes(profundidad, S.obs,shape=filtro)) + geom_dotplot(binaxis='y', 
               stackdir='center', 
               dotsize = .50, 
               fill="red") +
  theme(axis.text.x = element_text(angle=0, vjust=0.6)) + 
  labs(title="Estación M5", 
       subtitle="Diversidad por profundidades y filtros",
       x="profundidad (m)",
       y="S.obs")
q + geom_point(aes(colour = filtro), size = 6) + geom_point(colour = "black", size = 2) +scale_x_discrete (limits= c("5","20","100","200","300","500"))+scale_y_continuous(limits = c(0,300))


# M3
M3<-filter(tabla_box,estacion=="M3")
r <- ggplot(M3, aes(profundidad, S.obs,shape=filtro))+ geom_dotplot(binaxis='y', 
               stackdir='center', 
               dotsize = .50, 
               fill="red") +
  theme(axis.text.x = element_text(angle=0, vjust=0.6)) + 
  labs(title="Estación M3", 
       subtitle="Diversidad por profundidades y filtros",
       x="profundidad (m)",
       y="S.obs")
r+geom_point(aes(colour = filtro),size = 6)+geom_point(colour = "black", size = 2)+scale_x_discrete (limits= c("5","30","100","200","300"))+scale_y_continuous(limits = c(0,300))


################# Sitios
## ecorregion
x <- ggplot(na.omit(eco), aes(eco, S.obs))
x + geom_boxplot() + 
  geom_dotplot(binaxis='y', 
               stackdir='center', 
               dotsize = .50, 
               fill="red") +
  theme(axis.text.x = element_text(angle=0, vjust=0.6)) + 
  labs(title="Diversidad ecorregiones", 
       subtitle="Box plot + Dot plot",
       x="Ecorregión",
       y="S.obs")+scale_y_continuous(limits = c(30,270))

# Influencia aguas
x2 <- ggplot(na.omit(influ), aes(influ, S.obs))
x2 + geom_boxplot() + 
  geom_dotplot(binaxis='y', 
               stackdir='center', 
               dotsize = .50, 
               fill="red") +
  theme(axis.text.x = element_text(angle=0, vjust=0.6)) + 
  labs(title="Diversidad influencia masa de agua", 
       subtitle="Box plot + Dot plot",
       x="Influencia",
       y="S.obs")+scale_y_continuous(limits = c(30,270))

# cercania isla decepción
x3 <- ggplot(na.omit(decepcion), aes(decepcion, S.obs))
x3 + geom_boxplot() + 
  geom_dotplot(binaxis='y', 
               stackdir='center', 
               dotsize = .50, 
               fill="red") +
  theme(axis.text.x = element_text(angle=0, vjust=0.6)) + 
  labs(title="Diversidad cercania Isla Decepción", 
       subtitle="Box plot + Dot plot",
       x="Isla Decepción",
       y="S.obs")+scale_y_continuous(limits = c(30,270))

#### (3) Shannon
# FILTROS
theme_set(theme_bw())
f1 <- ggplot(tabla_box, aes(filtro, Shannon))
f1 + geom_boxplot() + 
  geom_dotplot(binaxis='y', 
               stackdir='center', 
               dotsize = .50, 
               fill="red") +
  theme(axis.text.x = element_text(angle=0, vjust=0.6)) + 
  labs(title="Diversidad filtros", 
       subtitle="Box plot + Dot plot",
       x="Filtro",
       y="Shannon") +scale_y_continuous(limits = c(2.5,4.95))

#filtro1
a1 <- ggplot(filtro1, aes(estacion, Shannon))
a1 + geom_boxplot() + 
  geom_dotplot(binaxis='y', 
               stackdir='center', 
               dotsize = .50, 
               fill="red") +
  theme(axis.text.x = element_text(angle=0, vjust=0.6)) + 
  labs(title="Diversidad filtro 1", 
       subtitle="Box plot + Dot plot",
       x="Estaciones",
       y="Shannon")+scale_y_continuous(limits = c(2.5,4.95))

#filtro2
b1 <- ggplot(filtro2, aes(estacion, Shannon))
b1 + geom_boxplot() + 
  geom_dotplot(binaxis='y', 
               stackdir='center', 
               dotsize = .50, 
               fill="red") +
  theme(axis.text.x = element_text(angle=0, vjust=0.6)) + 
  labs(title="Diversidad filtro 2", 
       subtitle="Box plot + Dot plot",
       x="Estaciones",
       y="Shannon")+scale_y_continuous(limits = c(2.5,4.95))

#filtro3
c1 <- ggplot(filtro3, aes(estacion, Shannon))
c1 + geom_boxplot() + 
  geom_dotplot(binaxis='y', 
               stackdir='center', 
               dotsize = .50, 
               fill="red") +
  theme(axis.text.x = element_text(angle=0, vjust=0.6)) + 
  labs(title="Diversidad filtro 3", 
       subtitle="Box plot + Dot plot",
       x="Estaciones",
       y="Shannon")+scale_y_continuous(limits = c(2.5,4.95))


######### PROFUNDIDADES
p1 <- ggplot(tabla_box, aes(prof, Shannon))
p1 + geom_boxplot() + 
  geom_dotplot(binaxis='y', 
               stackdir='center', 
               dotsize = .50, 
               fill="red") +
  theme(axis.text.x = element_text(angle=0, vjust=0.6)) + 
  labs(title="Diversidad profundidades", 
       subtitle="Box plot + Dot plot",
       x="profundidad (m)",
       y="Shannon")+ scale_x_discrete (limits= c("5","20","30","100","200","deep"))+scale_y_continuous(limits = c(2.5,4.95))


#profundidad 5
d1 <- ggplot(prof5, aes(estacion, Shannon))
d1 + geom_boxplot() + 
  geom_dotplot(binaxis='y', 
               stackdir='center', 
               dotsize = .50, 
               fill="red") +
  theme(axis.text.x = element_text(angle=0, vjust=0.6)) + 
  labs(title="Diversidad profundidad 5 metros", 
       subtitle="Box plot + Dot plot",
       x="Estaciones",
       y="Shannon")+scale_y_continuous(limits = c(2.5,4.95))

#profundidad 30
e1 <- ggplot(prof30, aes(estacion, Shannon))
e1 + geom_boxplot() + 
  geom_dotplot(binaxis='y', 
               stackdir='center', 
               dotsize = .50, 
               fill="red") +
  theme(axis.text.x = element_text(angle=0, vjust=0.6)) + 
  labs(title="Diversidad profundidad 30 metros", 
       subtitle="Box plot + Dot plot",
       x="Estaciones",
       y="Shannon")+scale_y_continuous(limits = c(2.5,4.95))

#profundidad 100
i1 <- ggplot(prof100, aes(estacion, Shannon))
i1 + geom_boxplot() + 
  geom_dotplot(binaxis='y', 
               stackdir='center', 
               dotsize = .50, 
               fill="red") +
  theme(axis.text.x = element_text(angle=0, vjust=0.6)) + 
  labs(title="Diversidad profundidad 100 metros", 
       subtitle="Box plot + Dot plot",
       x="Estaciones",
       y="Shannon")+scale_y_continuous(limits = c(2.5,4.95))


#profundidad 200
g1 <- ggplot(prof200, aes(estacion, Shannon))
g1 + geom_boxplot() + 
  geom_dotplot(binaxis='y', 
               stackdir='center', 
               dotsize = .50, 
               fill="red") +
  theme(axis.text.x = element_text(angle=0, vjust=0.6)) + 
  labs(title="Diversidad profundidad 200 metros", 
       subtitle="Box plot + Dot plot",
       x="Estaciones",
       y="Shannon")+scale_y_continuous(limits = c(2.5,4.95))


#profundidad deep
h1 <- ggplot(profdeep, aes(estacion, Shannon))
h1 + geom_boxplot() + 
  geom_dotplot(binaxis='y', 
               stackdir='center', 
               dotsize = .50, 
               fill="red") +
  theme(axis.text.x = element_text(angle=0, vjust=0.6)) + 
  labs(title="Diversidad profundidad deep", 
       subtitle="Box plot + Dot plot",
       x="Estaciones",
       y="Shannon")+scale_y_continuous(limits = c(2.5,4.95))



#estaciones
j1 <- ggplot(tabla_box, aes(estacion, Shannon))
j1 + geom_boxplot() + 
  geom_dotplot(binaxis='y', 
               stackdir='center', 
               dotsize = .50, 
               fill="red") +
  theme(axis.text.x = element_text(angle=0, vjust=0.6)) + 
  labs(title="Diversidad estaciones", 
       subtitle="Box plot + Dot plot",
       x="Estaciones",
       y="Shannon")+scale_y_continuous(limits = c(2.5,5))

# P02
k1 <- ggplot(P02, aes(profundidad, Shannon,shape=filtro))+ geom_dotplot(binaxis='y', 
                                                                     stackdir='center', 
                                                                     dotsize = .50, 
                                                                     fill="red") +
  theme(axis.text.x = element_text(angle=0, vjust=0.6)) + 
  labs(title="Estación P02", 
       subtitle="Diversidad por profundidades y filtros",
       x="profundidad (m)",
       y="Shannon")
k1 + geom_point(aes(colour = filtro), size = 6) + geom_point(colour = "black", size = 2) +scale_x_discrete (limits= c("5","30","100"))+scale_y_continuous(limits = c(2.5,4.95))


# P01
l1 <- ggplot(P01, aes(profundidad, Shannon,shape=filtro)) + geom_dotplot(binaxis='y', 
                                                                      stackdir='center', 
                                                                      dotsize = .50, 
                                                                      fill="red") +
  theme(axis.text.x = element_text(angle=0, vjust=0.6)) + 
  labs(title="Estación P01", 
       subtitle="Diversidad por profundidades y filtros",
       x="profundidad (m)",
       y="Shannon")
l1 + geom_point(aes(colour = filtro), size = 6) + geom_point(colour = "black", size = 2) +scale_x_discrete (limits= c("5","30","100"))+scale_y_continuous(limits = c(2.5,4.95))

# P04
m1 <- ggplot(P04, aes(profundidad, Shannon,shape=filtro))+ geom_dotplot(binaxis='y', 
                                                                     stackdir='center', 
                                                                     dotsize = .50, 
                                                                     fill="red") +
  theme(axis.text.x = element_text(angle=0, vjust=0.6)) + 
  labs(title="Estación P04", 
       subtitle="Diversidad por profundidades y filtros",
       x="profundidad (m)",
       y="Shannon")
m1 + geom_point(aes(colour = filtro), size = 6) + geom_point(colour = "black", size = 2) +scale_x_discrete (limits= c("5","30"))+scale_y_continuous(limits = c(2.5,4.95))

# P56
P56<-filter(tabla_box,estacion=="P56")
n1 <- ggplot(P56, aes(profundidad, Shannon,shape=filtro)) + geom_dotplot(binaxis='y', 
                                                                      stackdir='center', 
                                                                      dotsize = .50, 
                                                                      fill="red") +
  theme(axis.text.x = element_text(angle=0, vjust=0.6)) + 
  labs(title="Estación P56", 
       subtitle="Diversidad por profundidades y filtros",
       x="profundidad (m)",
       y="Shannon")
n1 + geom_point(aes(colour = filtro), size = 6) + geom_point(colour = "black", size = 2) +scale_x_discrete (limits= c("5","30","200","600","1000"))+scale_y_continuous(limits = c(2.5,4.95))

# Mx
o1 <- ggplot(Mx, aes(profundidad, Shannon,shape=filtro)) + geom_dotplot(binaxis='y', 
                                                                     stackdir='center', 
                                                                     dotsize = .50, 
                                                                     fill="red") +
  theme(axis.text.x = element_text(angle=0, vjust=0.6)) + 
  labs(title="Estación Mx", 
       subtitle="Diversidad por profundidades y filtros",
       x="profundidad (m)",
       y="Shannon")
o1 + geom_point(aes(colour = filtro), size = 6) + geom_point(colour = "black", size = 2) +scale_x_discrete (limits= c("5","30","100","200","830"))+scale_y_continuous(limits = c(2.5,4.95))


# M5
q1 <- ggplot(M5, aes(profundidad, Shannon,shape=filtro)) + geom_dotplot(binaxis='y', 
                                                                     stackdir='center', 
                                                                     dotsize = .50, 
                                                                     fill="red") +
  theme(axis.text.x = element_text(angle=0, vjust=0.6)) + 
  labs(title="Estación M5", 
       subtitle="Diversidad por profundidades y filtros",
       x="profundidad (m)",
       y="Shannon")
q1 + geom_point(aes(colour = filtro), size = 6) + geom_point(colour = "black", size = 2) +scale_x_discrete (limits= c("5","20","100","200","300","500"))+scale_y_continuous(limits = c(2.5,4.95))


# M3
r1 <- ggplot(M3, aes(profundidad, Shannon,shape=filtro))+ geom_dotplot(binaxis='y', 
                                                                    stackdir='center', 
                                                                    dotsize = .50, 
                                                                    fill="red") +
  theme(axis.text.x = element_text(angle=0, vjust=0.6)) + 
  labs(title="Estación M3", 
       subtitle="Diversidad por profundidades y filtros",
       x="profundidad (m)",
       y="Shannon")
r1+geom_point(aes(colour = filtro),size = 6)+geom_point(colour = "black", size = 2)+scale_x_discrete (limits= c("5","30","100","200","300"))+scale_y_continuous(limits = c(2.5,4.95))

################# Sitios
## ecorregion
xx <- ggplot(na.omit(eco), aes(eco, Shannon))
xx + geom_boxplot() + 
  geom_dotplot(binaxis='y', 
               stackdir='center', 
               dotsize = .50, 
               fill="red") +
  theme(axis.text.x = element_text(angle=0, vjust=0.6)) + 
  labs(title="Diversidad ecorregiones", 
       subtitle="Box plot + Dot plot",
       x="Ecorregión",
       y="Shannon")+scale_y_continuous(limits = c(2.5,4.95))

# Influencia aguas
x12 <- ggplot(na.omit(influ), aes(influ, Shannon))
x12 + geom_boxplot() + 
  geom_dotplot(binaxis='y', 
               stackdir='center', 
               dotsize = .50, 
               fill="red") +
  theme(axis.text.x = element_text(angle=0, vjust=0.6)) + 
  labs(title="Diversidad influencia masa de agua", 
       subtitle="Box plot + Dot plot",
       x="Influencia",
       y="Shannon")+scale_y_continuous(limits = c(2.5,4.95))

# cercania isla decepción
x13 <- ggplot(na.omit(decepcion), aes(decepcion, Shannon))
x13 + geom_boxplot() + 
  geom_dotplot(binaxis='y', 
               stackdir='center', 
               dotsize = .50, 
               fill="red") +
  theme(axis.text.x = element_text(angle=0, vjust=0.6)) + 
  labs(title="Diversidad cercania Isla Decepción", 
       subtitle="Box plot + Dot plot",
       x="Isla Decepción",
       y="Shannon")+scale_y_continuous(limits = c(2.5,4.95))

#### (4) Simpson
# FILTROS
theme_set(theme_bw())
f2 <- ggplot(tabla_box, aes(filtro, Simpson))
f2 + geom_boxplot() + 
  geom_dotplot(binaxis='y', 
               stackdir='center', 
               dotsize = .50, 
               fill="red") +
  theme(axis.text.x = element_text(angle=0, vjust=0.6)) + 
  labs(title="Diversidad filtros", 
       subtitle="Box plot + Dot plot",
       x="Filtro",
       y="Simpson")+scale_y_continuous(limits = c(0.83,0.999))

#filtro1
a2 <- ggplot(filtro1, aes(estacion, Simpson))
a2 + geom_boxplot() + 
  geom_dotplot(binaxis='y', 
               stackdir='center', 
               dotsize = .50, 
               fill="red") +
  theme(axis.text.x = element_text(angle=0, vjust=0.6)) + 
  labs(title="Diversidad filtro 1", 
       subtitle="Box plot + Dot plot",
       x="Estaciones",
       y="Simpson")+scale_y_continuous(limits = c(0.83,0.999))

#filtro2
b2 <- ggplot(filtro2, aes(estacion, Simpson))
b2 + geom_boxplot() + 
  geom_dotplot(binaxis='y', 
               stackdir='center', 
               dotsize = .50, 
               fill="red") +
  theme(axis.text.x = element_text(angle=0, vjust=0.6)) + 
  labs(title="Diversidad filtro 2", 
       subtitle="Box plot + Dot plot",
       x="Estaciones",
       y="Simpson")+scale_y_continuous(limits = c(0.83,0.999))

#filtro3
c2 <- ggplot(filtro3, aes(estacion, Simpson))
c2 + geom_boxplot() + 
  geom_dotplot(binaxis='y', 
               stackdir='center', 
               dotsize = .50, 
               fill="red") +
  theme(axis.text.x = element_text(angle=0, vjust=0.6)) + 
  labs(title="Diversidad filtro 3", 
       subtitle="Box plot + Dot plot",
       x="Estaciones",
       y="Simpson")+scale_y_continuous(limits = c(0.83,0.999))


######### PROFUNDIDADES
p2 <- ggplot(tabla_box, aes(prof, Simpson))
p2 + geom_boxplot() + 
  geom_dotplot(binaxis='y', 
               stackdir='center', 
               dotsize = .50, 
               fill="red") +
  theme(axis.text.x = element_text(angle=0, vjust=0.6)) + 
  labs(title="Diversidad profundidades", 
       subtitle="Box plot + Dot plot",
       x="profundidad (m)",
       y="Simpson")+ scale_x_discrete (limits= c("5","20","30","100","200","deep"))+scale_y_continuous(limits = c(0.83,0.999))


#profundidad 5
d2 <- ggplot(prof5, aes(estacion, Simpson))
d2 + geom_boxplot() + 
  geom_dotplot(binaxis='y', 
               stackdir='center', 
               dotsize = .50, 
               fill="red") +
  theme(axis.text.x = element_text(angle=0, vjust=0.6)) + 
  labs(title="Diversidad profundidad 5 metros", 
       subtitle="Box plot + Dot plot",
       x="Estaciones",
       y="Simpson")+scale_y_continuous(limits = c(0.83,0.999))

#profundidad 30
e2 <- ggplot(prof30, aes(estacion, Simpson))
e2 + geom_boxplot() + 
  geom_dotplot(binaxis='y', 
               stackdir='center', 
               dotsize = .50, 
               fill="red") +
  theme(axis.text.x = element_text(angle=0, vjust=0.6)) + 
  labs(title="Diversidad profundidad 30 metros", 
       subtitle="Box plot + Dot plot",
       x="Estaciones",
       y="Simpson")+scale_y_continuous(limits = c(0.83,0.999))

#profundidad 100
i2 <- ggplot(prof100, aes(estacion, Simpson))
i2 + geom_boxplot() + 
  geom_dotplot(binaxis='y', 
               stackdir='center', 
               dotsize = .50, 
               fill="red") +
  theme(axis.text.x = element_text(angle=0, vjust=0.6)) + 
  labs(title="Diversidad profundidad 100 metros", 
       subtitle="Box plot + Dot plot",
       x="Estaciones",
       y="Simpson")+scale_y_continuous(limits = c(0.83,0.999))


#profundidad 200
g2 <- ggplot(prof200, aes(estacion, Simpson))
g2 + geom_boxplot() + 
  geom_dotplot(binaxis='y', 
               stackdir='center', 
               dotsize = .50, 
               fill="red") +
  theme(axis.text.x = element_text(angle=0, vjust=0.6)) + 
  labs(title="Diversidad profundidad 200 metros", 
       subtitle="Box plot + Dot plot",
       x="Estaciones",
       y="Simpson")+scale_y_continuous(limits = c(0.83,0.999))


#profundidad deep
h2 <- ggplot(profdeep, aes(estacion, Simpson))
h2 + geom_boxplot() + 
  geom_dotplot(binaxis='y', 
               stackdir='center', 
               dotsize = .50, 
               fill="red") +
  theme(axis.text.x = element_text(angle=0, vjust=0.6)) + 
  labs(title="Diversidad profundidad deep", 
       subtitle="Box plot + Dot plot",
       x="Estaciones",
       y="Simpson")+scale_y_continuous(limits = c(0.83,0.999))



#estaciones
j2 <- ggplot(tabla_box, aes(estacion, Simpson))
j2 + geom_boxplot() + 
  geom_dotplot(binaxis='y', 
               stackdir='center', 
               dotsize = .50, 
               fill="red") +
  theme(axis.text.x = element_text(angle=0, vjust=0.6)) + 
  labs(title="Diversidad estaciones", 
       subtitle="Box plot + Dot plot",
       x="Estaciones",
       y="Simpson")+scale_y_continuous(limits = c(0.83,0.999))

# P02
k2 <- ggplot(P02, aes(profundidad, Simpson,shape=filtro))+ geom_dotplot(binaxis='y', 
                                                                        stackdir='center', 
                                                                        dotsize = .50, 
                                                                        fill="red") +
  theme(axis.text.x = element_text(angle=0, vjust=0.6)) + 
  labs(title="Estación P02", 
       subtitle="Diversidad por profundidades y filtros",
       x="profundidad (m)",
       y="Simpson")
k2 + geom_point(aes(colour = filtro), size = 6) + geom_point(colour = "black", size = 2) +scale_x_discrete (limits= c("5","30","100"))+scale_y_continuous(limits = c(0.83,0.999))


# P01
l2 <- ggplot(P01, aes(profundidad, Simpson,shape=filtro)) + geom_dotplot(binaxis='y', 
                                                                         stackdir='center', 
                                                                         dotsize = .50, 
                                                                         fill="red") +
  theme(axis.text.x = element_text(angle=0, vjust=0.6)) + 
  labs(title="Estación P01", 
       subtitle="Diversidad por profundidades y filtros",
       x="profundidad (m)",
       y="Simpson")
l2 + geom_point(aes(colour = filtro), size = 6) + geom_point(colour = "black", size = 2) +scale_x_discrete (limits= c("5","30","100"))+scale_y_continuous(limits = c(0.83,0.999))

# P04
m2 <- ggplot(P04, aes(profundidad, Simpson,shape=filtro))+ geom_dotplot(binaxis='y', 
                                                                        stackdir='center', 
                                                                        dotsize = .50, 
                                                                        fill="red") +
  theme(axis.text.x = element_text(angle=0, vjust=0.6)) + 
  labs(title="Estación P04", 
       subtitle="Diversidad por profundidades y filtros",
       x="profundidad (m)",
       y="Simpson")
m2 + geom_point(aes(colour = filtro), size = 6) + geom_point(colour = "black", size = 2) +scale_x_discrete (limits= c("5","30"))+scale_y_continuous(limits = c(0.83,0.999))

# P56
P56<-filter(tabla_box,estacion=="P56")
n2 <- ggplot(P56, aes(profundidad, Simpson,shape=filtro)) + geom_dotplot(binaxis='y', 
                                                                         stackdir='center', 
                                                                         dotsize = .50, 
                                                                         fill="red") +
  theme(axis.text.x = element_text(angle=0, vjust=0.6)) + 
  labs(title="Estación P56", 
       subtitle="Diversidad por profundidades y filtros",
       x="profundidad (m)",
       y="Simpson")
n2 + geom_point(aes(colour = filtro), size = 6) + geom_point(colour = "black", size = 2) +scale_x_discrete (limits= c("5","30","200","600","1000"))+scale_y_continuous(limits = c(0.83,0.999))

# Mx
o2 <- ggplot(Mx, aes(profundidad, Simpson,shape=filtro)) + geom_dotplot(binaxis='y', 
                                                                        stackdir='center', 
                                                                        dotsize = .50, 
                                                                        fill="red") +
  theme(axis.text.x = element_text(angle=0, vjust=0.6)) + 
  labs(title="Estación Mx", 
       subtitle="Diversidad por profundidades y filtros",
       x="profundidad (m)",
       y="Simpson")
o2 + geom_point(aes(colour = filtro), size = 6) + geom_point(colour = "black", size = 2) +scale_x_discrete (limits= c("5","30","100","200","830"))+scale_y_continuous(limits = c(0.83,0.999))


# M5
q2 <- ggplot(M5, aes(profundidad, Simpson,shape=filtro)) + geom_dotplot(binaxis='y', 
                                                                        stackdir='center', 
                                                                        dotsize = .50, 
                                                                        fill="red") +
  theme(axis.text.x = element_text(angle=0, vjust=0.6)) + 
  labs(title="Estación M5", 
       subtitle="Diversidad por profundidades y filtros",
       x="profundidad (m)",
       y="Simpson")
q2 + geom_point(aes(colour = filtro), size = 6) + geom_point(colour = "black", size = 2) +scale_x_discrete (limits= c("5","20","100","200","300","500"))+scale_y_continuous(limits = c(0.83,0.999))


# M3
r2 <- ggplot(M3, aes(profundidad, Simpson,shape=filtro))+ geom_dotplot(binaxis='y', 
                                                                       stackdir='center', 
                                                                       dotsize = .50, 
                                                                       fill="red") +
  theme(axis.text.x = element_text(angle=0, vjust=0.6)) + 
  labs(title="Estación M3", 
       subtitle="Diversidad por profundidades y filtros",
       x="profundidad (m)",
       y="Simpson")
r2+geom_point(aes(colour = filtro),size = 6)+geom_point(colour = "black", size = 2)+scale_x_discrete (limits= c("5","30","100","200","300"))+scale_y_continuous(limits = c(0.83,0.999))


################# Sitios
## ecorregion
xxx <- ggplot(na.omit(eco), aes(eco, Simpson))
xxx + geom_boxplot() + 
  geom_dotplot(binaxis='y', 
               stackdir='center', 
               dotsize = .50, 
               fill="red") +
  theme(axis.text.x = element_text(angle=0, vjust=0.6)) + 
  labs(title="Diversidad ecorregiones", 
       subtitle="Box plot + Dot plot",
       x="Ecorregión",
       y="Simpson")+scale_y_continuous(limits = c(0.83,0.999))

# Influencia aguas
x22 <- ggplot(na.omit(influ), aes(influ, Simpson))
x22 + geom_boxplot() + 
  geom_dotplot(binaxis='y', 
               stackdir='center', 
               dotsize = .50, 
               fill="red") +
  theme(axis.text.x = element_text(angle=0, vjust=0.6)) + 
  labs(title="Diversidad influencia masa de agua", 
       subtitle="Box plot + Dot plot",
       x="Influencia",
       y="Simpson")+scale_y_continuous(limits = c(0.83,0.999))

# cercania isla decepción
x23 <- ggplot(na.omit(decepcion), aes(decepcion, Simpson))
x23 + geom_boxplot() + 
  geom_dotplot(binaxis='y', 
               stackdir='center', 
               dotsize = .50, 
               fill="red") +
  theme(axis.text.x = element_text(angle=0, vjust=0.6)) + 
  labs(title="Diversidad cercania Isla Decepción", 
       subtitle="Box plot + Dot plot",
       x="Isla Decepción",
       y="Simpson")+scale_y_continuous(limits = c(0.83,0.999))

