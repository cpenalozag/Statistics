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
datos <- read.csv("~/Desktop/Tarea 1/Punto 4.csv", header=TRUE, sep=",")
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


SSA<- sum(DS[,4]*(DS[,5]-mean(y[,1]))^2)
SSA

SST<- var(y)*(length(y[,1])-1)
SST
