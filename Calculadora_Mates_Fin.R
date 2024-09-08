library(pacman)
pacman::p_load(FinancialMath,dplyr,magrittr,knitr)


#3. Berenice pide prestados 20,000 para comprar un coche. La agencia de autos le ofrece 
#   dos alternativas de planes de financiamiento, ambas constan de pagos mensuales por 4 
#   años comenzando un mes después de la compra del coche.

#i) 0% de interés durante el primer año, seguido de una tasa de interés nominal 
#   del 6 % convertible mensual por los siguientes 3 años.

#Definimos Variables
L<-20000 #Valor de la deuda
n_1<-12 #Pagos mensuales del 1er año
n_2<-36 #Pagos mensuales de los 3 años
j<-0.005 #Tasa de interés nominal del 6 % convertible mensual
v<-(1+j)^-1
anj<- ((1-(1+j)^(-n_2))/ j) #Valor de una anualidad vencida nivelada

#sabemos que cuando i=0 entonces anj es n, entonces R es

R <- ((L)/(12+anj))


#Hacemos la tabla del primer año

PERIODO = 1:n_1
tabla_3_11 <- data.frame(PERIODO = 1:n_1,
                         PAGO = R,
                         INTERES = 0,
                         PRINCIPAL = R,
                         SALDO_INSOLUTO = 0)
#Llenamos la primer fila
tabla_3_11$SALDO_INSOLUTO[1] <- L - tabla_3_11$PRINCIPAL[1]

#Llenamos la columna de Saldo Insoluto
for(i in 2:n_1){
  tabla_3_11$SALDO_INSOLUTO[i] <- tabla_3_11$SALDO_INSOLUTO[i-1] - tabla_3_11$PRINCIPAL[i]
}

#Imprimos la tabla (para ver el pago mensual y el saldo pendiente del primer año)
tabla_3_11 %>% kable()


#Ahora hacemos la tabla del 2do año
PERIODO_2<-1:36 #36 meses, los 3 años restantes
tabla_3_12 <- data.frame(PERIODO_2 = 1:36,
                         PAGO = R,
                         INTERES = R*(1-v^(n_2- PERIODO_2 + 1)),
                         PRINCIPAL = R*v^(n_2- PERIODO_2 + 1),
                         SALDO_INSOLUTO = (R/j)*(1-v^(n_2-PERIODO_2)))


#ii)Una tasa de interés del 3 % convertible mensual para el primer año, seguido 
#   de una tasa de interés del 5 % convertible mensual por los siguientes 3 años.

#Definimos Variables para el primer año (despúes del primer año se ajusta la tabla  de 
#amortización pero ahora solo nos pide el primer año)
L<-20000 #Valor de la deuda
n<-48 #Pagos mensuales 
j<-0.0025 #Tasa de interés nominal del 3 % convertible mensual
v<-(1+j)^-1 
anj<- ((1-(1+j)^(-n))/ j) #Valor de una anualidad vencida nivelada para el primer año

#Los pagos durante el primer año son de R

R<-(20000/anj)

#Construimos la tabla para el primer año
PERIODO<-1:12
tabla_3_21 <- data.frame(PERIODO = 1:12,
                         PAGO = R,
                         INTERES = R*(1-v^(n- PERIODO + 1)),
                         PRINCIPAL = R*v^(n- PERIODO + 1),
                         SALDO_INSOLUTO = (R/j)*(1-v^(n-PERIODO)))
#Imprimos la tabla (para ver el pago mensual y el saldo pendiente del primer año)
tabla_3_21 %>% kable()