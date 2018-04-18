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


# Model Comparison
anova(model,reducedModel)

#Model 3:Reduced model low difficulty
Data$Dificultad<-relevel(Data$Dificultad,ref="Baja") #Declare base category
reducedModel<-lm(Ventas~Historico*`Numero de piezas`+Precio*Dificultad,data=Data)
summary(reducedModel)

