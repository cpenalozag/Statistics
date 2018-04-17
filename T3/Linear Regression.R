library(readxl)
datos <- read_excel("Desktop/Universidad/6to Semestre/Proba  II/Tareas/T3/Tarea3.xlsx")
head(datos)
a<-rep(1,30)#Generar columna de 1's
X<-data.matrix(cbind(a,datos[,-c(1,6)])) #Matriz X
Y<-data.matrix(datos[,6]) #Matriz Y 
XTX<-t(X)%*%X #Operación matricial 
XTX#XtX
XTY<-t(X)%*%Y
XTY#XyY

betas<- solve(XTX)%*%XTY
betas #Vector estimado de los betas

SSR<-t(betas)%*%XTY-XTX[1,1]*mean(Y)^2
SSR[1,1]
SSE<-t(Y)%*%Y-t(betas)%*%XTY
SSE[1,1]
SST<-t(Y)%*%Y-XTX[1,1]*mean(Y)^2
SST[1,1]
R2<-SSR/SST
R2[1,1]

#Cálculo del MSE
MSE<-SSE/(XTX[1,1]-length(betas)) #SSE dividido N-(k+1)
MSE

#Matriz de varianzas y covarianzas 
matcov<-MSE[1,1]*solve(XTX)
matcov

#Calcular Regresión lineal
fit<-lm(`Numero Ciclistas`~.-`Zona`,data=datos) #Estimación de regresión lineal
summary(fit) # Coeficientes del modelo lineal

#Prueba de significancia Global
MSR<-SSR/4 # SSR dividido el número de variables del modelo 
MSR

F_<-MSR/MSE
F_

#Región de rechazo
qf(0.95,5,25)

#Pruebas de Significancia individual
qt(0.975,25)

#Modelo Completo
summary(fit)
