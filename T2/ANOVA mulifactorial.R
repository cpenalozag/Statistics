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

# Punto 1c
# Descriptive Statistics
paletas <- read.csv("Desktop/Universidad/6to Semestre/Proba  II/Tareas/T2/Paletas.csv", header=TRUE, sep=",")
print(head(paletas))
DSp<-describeBy(paletas[2],group=paletas[1], mat=TRUE, digits= 2)
print(DSp)

# Box plot
xp=paletas[,1]
yp=paletas[,2]

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
datos <- read.csv("Desktop/Censo.csv", header=TRUE, sep=",")
print(head(datos))
groupingVs <- list(datos[,1],datos[,2])
DS<-describeBy(datos[3],group=groupingVs, mat=TRUE, digits= 2)
print(DS)

# Box plot
Ingreso = datos[,3]
Ingreso
Cantidad<-as.factor(datos[,1])
Cantidad
Barrio<-as.factor(datos[,2])
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
dep<-c(datos2[,2])
fact<-c(datos2[,1])
shapiro.test(dep)
shapiro.test(fact)

# Independence test
tbl <- table(fact,dep)
tbl
chisq.test(tbl)
