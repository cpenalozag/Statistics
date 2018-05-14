library(readxl)
library(car)
library(lmtest)
library(EnvStats)
library(caret)
library(ddalpha)
library(kernlab)
library(e1071)
library(orcutt)

# Data for financial entity problem
data1 <- read_excel("Desktop/Universidad/6to Semestre/Proba  II/Tareas/T5/Data.xlsx", sheet="Punto 3")
head(data1)
model1<-lm(Gasto~.-Empresa,data=data1)
summary(model1)

# Se calcula el VIF
vif(model1)

# Se grafica el modelo
plot(model1)

#frame de las variables X
matriz<-data.frame(data1[,3:6])
#Matriz de correlaciones 
cor(matriz)

#Prueba de Breusch Pagan
bptest(model1)

# En función de Gasto Financiero
bptest(Gasto~`Gasto Financiero`,data=data1)

# En función de Ingresos
bptest(Gasto~Ingresos,data=data1)

# En función de Calificacion Financiera
bptest(Gasto~`Calificacion Financiera`,data=data1)

# En función de Valor Empresa
bptest(Gasto~`Valor Empresa`,data=data1)

# En función de Puntaje Contaminación
bptest(Gasto~`Puntaje Contaminacion`,data=data1)

# En función de Numero de pais
bptest(Gasto~`Numero de paises`,data=data1)

gastoFinBCMod <- caret::BoxCoxTrans(data1$`Gasto Financiero`)
puntajeContBCMod <- caret::BoxCoxTrans(data1$`Puntaje Contaminacion`)
numPaisesBCMod <- caret::BoxCoxTrans(data1$`Numero de paises`)

gastoBCMod <- caret::BoxCoxTrans(data1$Gasto)

data1corrected <- cbind(data1, `Gasto Financiero n`=predict(gastoFinBCMod, data1$`Gasto Financiero`)) # append the transformed variable
data1corrected <- cbind(data1corrected,  `Puntaje Contaminacion n`=predict(puntajeContBCMod, data1$`Puntaje Contaminacion`)) # append the transformed variable
data1corrected <- cbind(data1corrected, `Numero de paises n`=predict(numPaisesBCMod, data1$`Numero de paises`)) # append the transformed variable

data1corrected <- cbind(data1, `Gasto nuevo`=predict(gastoBCMod, data1$Gasto)) # append the transformed variable

correctedModel<-lm(`Gasto nuevo`~.-Empresa-Gasto,data=data1corrected)
summary(correctedModel)
bptest(correctedModel)

#Correción 
data1correccion<-data1/((data1$`Gasto Financiero`*data1$`Puntaje Contaminacion`*data1$`Numero de paises`)^1/2)
data1correccion<-data1correccion/((data1$`Puntaje Contaminacion`)^1/2)
data1correccion<-data1correccion/((data1$`Numero de paises`)^1/2)

reg<-lm(Gasto~.-Empresa,data=data1correccion) 
summary(reg)
bptest(reg)


# PUNTO 6

# Data for flower shop
data2 <- read_excel("Desktop/Universidad/6to Semestre/Proba  II/Tareas/T5/Data.xlsx", sheet="Punto 4")
head(data2)

#Modelo flores
model2 <- lm(`Docenas de Flores`~.,data=data2)
summary(model2)

# Se calcula el VIF
vif(model2)

# Se grafica el modelo
plot(model2)

# Breusch-Pagan Test
bptest(model2)

# Frame de las variables
matriz<-data.frame(data2[,1:5])
# Matriz de correlaciones 
cor(matriz)

#Regresión sin dias cerrados
model2red <- lm(`Docenas de Flores`~.-`Dias Cerrados`,data=data2)
summary(model2red)
vif(model2red)

#Comparación de residuales
res<-matrix(residuals(model2))
res1<-matrix(c(res[2:21,],0))
#Gráfico
plot(res,res1)

#Prueba de Durbin-WAtson
dwtest(model2)

# PUNTO 7


# Data about earth movement
data3 <- read_excel("Desktop/Universidad/6to Semestre/Proba  II/Tareas/T5/Data.xlsx", sheet="Punto 5")
head(data3)

model3 <- lm(Temperatura~.,data=data3)
summary(model3)

# Calcular vif
vif (model3)

# Graficar modelo
plot(model3)

# Test de Breusch-Pagan
bptest(model3)

#Comparación de residuales
res<-matrix(residuals(model3))
res1<-matrix(c(res[2:365,],0))
#Gráfico
plot(res,res1)

# Prueba de Durbin-WAtson
dwtest(model3)

# Corrección autocorrelación
Correccion<-cochrane.orcutt(model3)
summary(Correccion)

#verificar
dwtest(Correccion)
