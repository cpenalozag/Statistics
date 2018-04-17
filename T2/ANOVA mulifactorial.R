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
library(onewaytests)
library(lawstat)
library(stats)

# Punto 1c
# Descriptive Statistics
paletas <- read.csv("Desktop/Universidad/6to Semestre/Proba  II/Tareas/T2/Paletas.csv", header=TRUE, sep=",")
print(head(paletas))
DSp<-describeBy(paletas[2],group=paletas[1], mat=TRUE, digits= 2)
print(DSp)

# Box plot
xp=paletas[,1]
Peso=paletas[,2]
levene.test(Peso,xp)

fill <- "#4271AE"
lines <- "#1F3552"
plot1 <- ggplot(paletas, aes(x = xp, y = yp)) +
  geom_boxplot(colour = lines,fill=fill,size=1) +
  scale_y_continuous(name = "Peso",
                     breaks = seq(180, 400, 10),
                     limits=c(180,400)) +
  scale_x_discrete(name = "Proveedor") +
  ggtitle("Proveedor Vs Peso") +
  theme_bw() +
  theme(panel.grid.major = element_line(colour = "#d3d3d3"),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(size = 18, family = "Tahoma", face ="bold"),
        text=element_text(family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x = element_text(colour="black", size = 12),
        axis.text.y = element_text(colour="black", size = 12),
        axis.line = element_line(size=0.5, colour = "black"))

plot1


# Punto 4
# Descriptive Statistics
datosC <- read.csv("Desktop/Universidad/6to Semestre/Proba  II/Tareas/T2/Censo.csv", header=TRUE, sep=",")
print(head(datosC))
groupingVs <- list(datosC[,1],datosC[,2])
DS<-describeBy(datosC[3],datosC[,1], mat=TRUE, digits= 2)
print(DS)

# Box plot
Ingreso = datosC[,3]
Ingreso
Cantidad<-as.factor(datosC[,1])
Cantidad
Barrio<-as.factor(datosC[,2])
Barrio

# Box plot dos factores
plot <- ggplot(datos, aes(x =Cantidad, y = Ingreso,fill=Barrio )) +
  geom_boxplot()+
  scale_y_continuous(name = "Ingreso ($)",
                     breaks = seq(15, 30, 1),
                     limits=c(15, 30))+
  scale_x_discrete(name = "Cantidad personas")+
  theme_bw()+
  theme(panel.grid.major = element_line(colour = "#d3d3d3"),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(size = 18, family = "Tahoma", face ="bold"),
        text=element_text(family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x = element_text(colour="black", size = 12),
        axis.text.y = element_text(colour="black", size = 12),
        axis.line = element_line(size=0.5, colour = "black"))

plot



# Anova dos factores con interacci??n
fit<-aov(Ingreso~Cantidad*Barrio)
summary(fit)
print(datosC)
print(mean(datosC$Ingreso[datosC$Barrio=="Rosales"&datosC$Cantidad=="Cinco"]))
print(mean(datosC$Ingreso[datosC$Barrio=="Chico"&datosC$Cantidad=="Tres"]))

#Medias y n??mero de datos
mu1<-mean(datosC$Ingreso[datosC$Barrio=="Rosales"&datosC$Cantidad=="Cinco"])
mu2<-mean(datosC$Ingreso[datosC$Barrio=="Chico"&datosC$Cantidad=="Tres"])
n1<-length(datosC$Ingreso[datosC$Barrio=="Rosales"&datosC$Cantidad=="Cinco"])
n2<-length(datosC$Ingreso[datosC$Barrio=="Chico"&datosC$Cantidad=="Tres"])
MSE<-anova(fit)[["Mean Sq"]][4] 
MSE
gle<-df.residual(fit)
gle
#Estad??tico de Prueba
t<-(mu1-mu2)/sqrt(MSE*(1/n1+1/n2))
t
qt(0.95,gle)

#Rosales vs Usaquen
#Medias y n??mero de datos
mu1<-mean(datosC$Ingreso[datosC$Barrio=="Rosales"])
mu2<-mean(datosC$Ingreso[datosC$Barrio=="Usaquen"])
n1<-length(datosC$Ingreso[datosC$Barrio=="Rosales"])
n2<-length(datosC$Ingreso[datosC$Barrio=="Chico"])
MSE<-anova(fit)[["Mean Sq"]][4] 
gle<-df.residual(fit)
#Estad??tico de Prueba
t<-(mu1-mu2-2.5)/sqrt(MSE*(1/n1+1/n2))
t
qt(0.95,gle)

fitT<-aov(Ingreso~Cantidad)
TukeyHSD(fitT)

# 3 Factor anova
datos <- read.csv("Desktop/Universidad/6to Semestre/Proba  II/Tareas/T2/Censo2.csv", header=TRUE, sep=",")
print(datos[,1])
groupingVs <- list(datos[,1],datos[,2],datos[,3])
DS<-describeBy(datos[4],group=groupingVs, mat=TRUE, digits= 2)
print(DS)
y = datos[,4]
print(head(datos[3]))
Cantidad = as.factor(datos[,1])
print(Cantidad)
Barrio = as.factor(datos[,2])

MasDe3Banos = as.factor(datos[,3])
print(MasDe3Banos)
##Correr modelo Anova con interacci??n Triple 
fit<-aov(y~Cantidad*Barrio*MasDe3Banos)
summary(fit)

# Punto 6
# Descriptive Statitics
datos2 <- read.csv("Desktop/Universidad/6to Semestre/Proba  II/Tareas/T2/Ingles.csv", header=TRUE, sep=",")
print(head(datos2[,1]))
DS<-describeBy(datos2[2],group=datos2[1],mat=TRUE,digits = 2)
print(DS)


# Anova

anov<- aov(datos2[,2]~datos2[,1])
summary(anov)
anov
datos[,2 ]

# Normality test
#Raw data
dep<-c(datos2[,2])
print(head(datos2[,2]))
print(head(datos2[,1]))
fact<-factor(datos2[,1])
factf<-log(fact)
shapiro.test(dep)
shapiro.test(fact)
#Fixed data

# Independence test
tbl <- table(fact,dep)
tbl
chisq.test(tbl)
f=c("Once A", "Once B", "Once C", "Once D", "Once E")
print(f)
fa=factor(f)
print(fa)
# Homocedasticity test
df = data.frame(fact,dep)
print(df)
bf.test(dep~fa,data=df, verbose=TRUE)
#With natural logarithm
depf = log(dep)
print(depf)
dff = data.frame(depf,fact)
bf.test(depf~fact,data=dff)
levene.test(dep,fact)
levene.test(depf,fact)
plot(fact,dep)
plot(fact,depf)
