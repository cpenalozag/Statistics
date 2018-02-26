library(psych)
library(ggplot2)
library(magrittr)
library(stringr)
library(lubridate)
library(scales)
library(testthat)
library(readxl)
library(RColorBrewer)
library(tidyverse)
library(dplyr)

# Punto 4
# Descriptive Statitics
datos <- read.csv("Desktop/Universidad/6to Semestre/Proba  II/Tareas/T1/Punto 4.csv", header=TRUE, sep=",")
print(head(datos))
DS<-describeBy(datos[4],group=datos[3],mat=TRUE,digits = 2)
print(DS)

# Box plot
x=datos[3]
y=datos[4]
fill <- "#4271AE"
lines <- "#1F3552"
plot1 <- ggplot(datos, aes(x = x, y = y)) +
  geom_boxplot(colour = lines,fill=fill,size=1) +
  scale_y_continuous(name = "Life Expectancy (years)",
                     breaks = seq(50, 90, 5),
                     limits=c(50, 90)) +
  scale_x_discrete(name = "Income group") +
  ggtitle("Life Expectancy (years) Vs Income group") +
  theme_bw() +
  theme(panel.grid.major = element_line(colour = "#d3d3d3"),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(size = 18, family = "Tahoma", face ="bold"),
        text=element_text(family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x = element_text(colour="black", size = 13),
        axis.text.y = element_text(colour="black", size = 11),
        axis.line = element_line(size=0.5, colour = "black"))

plot1 

# Sum of squares
SSA<- sum(DS[,4]*(DS[,5]-mean(y[,1]))^2)
SSA

SST<- var(y[,1])*(length(y[,1])-1)
SST

SSE<-SST-SSA
SSE

# Estadístico F
MSA<-(SSA/(4-1))
MSE<-(SSE/(202-4))

F_<- MSA/MSE
F_

qf(0.95,3,198)

# Anova
anov<- aov(y[,1]~x[,1])
summary(anov)
anov


# Means
h <- DS[1,5] # Mean higher
b <- DS[2,5] # Mean lower
mb <- DS[3,5] # Mean lower mid
um <- DS[4,5] # Mean upper middle

# N data
n_h <- DS[1,4] # Mean higher
n_b <- DS[2,4] # Amount lower
n_mb <- DS[3,4] # Amount lower mid
n_um <- DS[4,4] # Mean upper middle

# Estadístico de Prueba
t<- (mb-b)/sqrt(MSE*(1/n_mb+1/n_b))
t  

#Región de rechazo
rr <- qt(0.975,198+3-2)
rr
# Intervalos de confianza
# Bajo
ICb1<- b - rr*sqrt(MSE/n_b)
ICb1
ICb2<- b + rr*sqrt(MSE/n_b)
ICb2

# Medio bajo
ICmb1<- mb - rr*sqrt(MSE/n_mb)
ICmb1
ICmb2<- mb + rr*sqrt(MSE/n_mb)
ICmb2

# Medio alto
ICum1<- um - rr*sqrt(MSE/n_um)
ICum1
ICum2<- um + rr*sqrt(MSE/n_um)
ICum2

# Alto
ICh1<- h - rr*sqrt(MSE/h)
ICh1
ICh2<- h + rr*sqrt(MSE/h)
ICh2

# Punto 5
datos2 <- read.csv("Desktop/Universidad/6to Semestre/Proba  II/Tareas/T1/Punto 5.csv", header=TRUE, sep=",")
head(datos2)
datos2[,1]
groups <- list (datos2[,1],datos2[,3])
list
DS2<-describeBy(datos2[4],group=groups,mat=TRUE,digits = 3)
DS2
y = datos2[,4]
y
franja<-as.factor(datos2[,1])
franja
ciudad<-as.factor(datos2[,3])
ciudad

# Box plot dos factores
plot2 <- ggplot(datos2, aes(x =franja, y = y,fill=ciudad )) +
  geom_boxplot()+
  scale_y_continuous(name = "Consumo de Combustible",
                     breaks = seq(50, 100, 5),
                     limits=c(50, 100))+
  theme_bw()+
  theme(panel.grid.major = element_line(colour = "#d3d3d3"),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(size = 18, family = "Tahoma", face ="bold"),
        text=element_text(family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x = element_text(colour="black", size = 13),
        axis.text.y = element_text(colour="black", size = 11),
        axis.line = element_line(size=0.5, colour = "black"))

plot2


# Anova 2 factores
fit2<-aov(y~franja+ciudad)
summary(fit2)

# Gráfico interacción
plot3<- ggplot(datos2, aes(x =franja, y = y,colour = ciudad, 
                          group = ciudad)) + geom_point() + geom_line()+theme_bw()+
  scale_y_continuous(name = "Consumo de Combustible",
                     breaks = seq(50, 100, 5),
                     limits=c(50, 100))+
  theme(panel.grid.major = element_line(colour = "#d3d3d3"),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(size = 18, family = "Tahoma", face ="bold"),
        text=element_text(family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x = element_text(colour="black", size = 13),
        axis.text.y = element_text(colour="black", size = 11),
        axis.line = element_line(size=0.5, colour = "black"))

plot3

# Anova dos factores con interacción
fit3<-aov(y~franja*ciudad)
summary(fit3)


# c

datos3 <- datos2
head(datos3)
datos3[,4]=datos3[,4]*1000*3.78/100

fit4<-aov(datos3[,4]~franja*ciudad)
summary(fit4)
