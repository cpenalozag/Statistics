library(readxl)

# Import database
Data <- read_excel("Desktop/Universidad/6to Semestre/Proba  II/Tareas/T4/Modelos.xlsx")
head(Data)

# Declare as factors
Data$Dificultad<-factor(Data$Dificultad) #Declare variable as factor
Data$Historico<-factor(Data$Historico) #Declare variable as factor

#Model 1: Base dificultad: Media
Data$Dificultad<-relevel(Data$Dificultad,ref="Media") #Declare base category
model<-lm(Ventas~Historico*`Numero de piezas`+Precio*Dificultad+`Presupuesto de publicidad`*Dificultad,data=Data)
summary(model)
plot(model)

#Model 2:Reduced model
Data$Historico<-relevel(Data$Historico,ref="No") #Declare base category
reducedModel<-lm(Ventas~Historico*`Numero de piezas`+Precio*Dificultad,data=Data)
summary(reducedModel)

library(car)
# Partial f for d)
linearHypothesis(reducedModel, c("HistoricoSi=0", "HistoricoSi:`Numero de piezas`=0"), test="F")

# Partial f for e)
linearHypothesis(reducedModel, c("DificultadBaja=0", "Precio:DificultadBaja=0"), test="F")

# Partial f for f)
Data$Historico<-relevel(Data$Historico,ref="No") #Declare base category
Data$Dificultad<-relevel(Data$Dificultad,ref="Baja") #Declare base category
linearHypothesis(reducedModel, c("HistoricoSi=0", "HistoricoSi:`Numero de piezas`=0", "DificultadExtrema=0", "Precio:DificultadExtrema=0"), test="F")



# Model Comparison
anova(model,reducedModel)

#Model 3:Reduced model low difficulty
Data$Dificultad<-relevel(Data$Dificultad,ref="Baja") #Declare base category
reducedModel<-lm(Ventas~Historico*`Numero de piezas`+Precio*Dificultad,data=Data)
summary(reducedModel)

#Opción1
op1<-matrix(c(0,1,190,3.05,0,0,190,0,0),1)
#Opción2 
op2<-matrix(c(0,0,220,2.20,0,1,0,0,2.20),1)
#Comparación
K<-op1-op2
K
K2<-op2-op1
K2
library(multcomp)
#EP
t<-glht(reducedModel,linfct=K)
summary(t)
#EP 2
t2<-glht(reducedModel,linfct=K2)
summary(t2)
