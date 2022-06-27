library(xts)
library(aTSA)
library(ggplot2)
library(fable)
library(fpp2)
library(forecast)
library(tsibble)
library(tsbox)
library(plyr)
library(reshape)
library(RCurl)


data("AirPassengers")

#Graficarmos para ver nuestros datos
plot(AirPassengers, type="o",xlab="Tiempo", ylab="Valores", main="Pasajeros")

#Graficamos la autocorrelacion
acf(AirPassengers,lag=35,col="2",main="",
    xlab="Lag",ylab="Valores")

#Graficamos la autocorrelacion parcial
pacf(AirPassengers,lag=35,col="2",main="",
    xlab="Lag",ylab="Valores")

#comprobamos visualmente que no tenemos estacionariedad

##Realizamos diferenciacion

#Graficarmos para ver nuestros datos
plot(diff(AirPassengers), type="o",xlab="Tiempo", ylab="Valores", main="Pasajeros")

#Graficamos la autocorrelacion
acf(diff(AirPassengers),lag=35,col="2",main="",
    xlab="Lag",ylab="Valores")

#Graficamos la autocorrelacion parcial
pacf(diff(AirPassengers),lag=35,col="2",main="",
     xlab="Lag",ylab="Valores")

#Se ve in incremento de Variancia lo cual se podria interpretar como tendencia

##Realizamos diferenciacion con logaritmo natural de la serie 

#Graficarmos para ver nuestros datos
plot(diff(log(AirPassengers)), type="o",xlab="Tiempo", ylab="Valores", main="Pasajeros")

#Graficamos la autocorrelacion
acf(diff(log(AirPassengers)),lag=35,col="2",main="",
    xlab="Lag",ylab="Valores")

#Graficamos la autocorrelacion parcial
pacf(diff(log(AirPassengers)),lag=35,col="2",main="",
     xlab="Lag",ylab="Valores")

#Test para ver la no estacionariedad
#Dickey Fuller nos da la probabilidad de ser no estacionaria
#Si es mayor que 0.05 la serie es no estacionaria(Tiene tendencia)

adf.test(diff(log(AirPassengers)))#0.01 es estacionaria

aerial=diff(log(AirPassengers))
ARIMAmodel <- auto.arima(aerial)
ARIMAmodel
#regular,estacional
#ARIMA(0,0,1)(0,1,1)[12]
#componente auto regresivos, aplicar diff y media movil

tsdiag(ARIMAmodel)

#comportamiento de los residuos
#autocorrelacion de los residuales
#Prueba Ljung-Box



#Observador vs los Esperados del modelo

plot(ARIMAmodel$x,col="red")
lines(fitted(ARIMAmodel),col="blue")
#datos con deficit

#prdecir
forecast(ARIMAmodel,h = 12)
autoplot(forecast::forecast(ARIMAmodel,h = 12))
modelocast <- as.data.frame(forecast(ARIMAmodel,h = 12))
exp(modelocast$`Point Forecast`)



























