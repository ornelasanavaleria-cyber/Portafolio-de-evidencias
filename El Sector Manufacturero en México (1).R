#Nombre: ORNELAS CRUCES ANA VALERIA 


#Directorio 
setwd("D:/")

#a.	Análisis de media y mediana de las tasas de crecimiento

# Activar librerias
library(stats)
library(readxl)
library(forecast)
library(timeSeries)
library(tseries)
library(mFilter)

# Lectura de base de datos
PDatos <- read_xls("PDatos.xls", sheet="PDatos")

# Visualizar objeto con base de datos
View(PDatos)

# Activar en memoria la base de datos
attach(PDatos)

summary(PDatos)

# Identificar variables de serie de tiempo
IED_Manu <- ts(IED, frequency = 4, start=c(1999,1))
Export_Manu <- ts(Export, frequency = 4, start=c(1999,1))

# Tasas de crecimiento tri - mensual de la IED y Export: aplicando un diferencial al logaritmo  
TM_IED_Manu <- diff(log(IED_Manu),1)*100 #Tasa de Crecimiento tri - mensual de la IED 
TM_Export_Manu <- diff(log(Export_Manu),1)*100 #Tasa de Crecimiento tri - mensual de las Exportaciones

summary(TM_IED_Manu)
summary(TM_Export_Manu)

# Tasas de crecimiento anual de la IED y Export: *colocando un 4, porque ambas series son trimestrales 
TA_IED_Manu <- diff(log(IED_Manu),4)*100 #Tasa de Crecimiento anual de la IED 
TA_Export_Manu <- diff(log(Export_Manu),4)*100 #Tasa de Crecimiento tri - mensual de las Exportaciones 

summary(TA_IED_Manu)
summary(TA_Export_Manu)

#Gráfica de Tasa de Crecimiento anual de las Exportaciones: 
plot(TA_Export_Manu, type="line", main="Gráfica 1: Tasa de Crecimiento Anual de las Exportaciones de manufacturas de México a EU, 1999 - 2022",
     col="red", ylim=c(-47.580,49.994), ylab="Tasa Anual en tanto por ciento",
     xlab="Periodo 1999-2022") 

#Gráfica de Tasa de Crecimiento tri - mensual de las Exportaciones: 
plot(TM_Export_Manu, type="line", main="Gráfica 2: Tasa de Crecimiento tri - mensual de las Exportaciones de manufacturas de México a EU, 1999 - 2022",
     col="blue", ylim=c(-47.580,49.994), ylab="Tasa Mensual en tanto por ciento",
     xlab="Periodo 1999-2022") 

#Gráfica de Tasa de Crecimiento anual y tri - mensual de las Exportaciones: 
plot(TA_Export_Manu, type="line", main="Gráfica 3: Tasa de Crecimiento Anual// tri- mensual de las Exportaciones de manufacturas de México a EU, 1999 - 2022",
     col="red", ylim=c(-47.580,49.994), ylab="Tasa Tri - Mensual en tanto por ciento",
     xlab="Periodo 1999-2022") 
lines(TM_Export_Manu, col="blue", lty=2)

# Grafica de Tasa de crecimiento anual de la IED: 
plot(TA_IED_Manu, type="line", main="Gráfica 4: Tasa de Crecimiento Anual de la IED de EU en el sector manufacturero de México, 1999 - 2022",
     col="red", ylim=c(-280.150,287.816), ylab="Tasa Anual en tanto por ciento",
     xlab="Periodo 1999-2022") 

# Grafica de Tasa de crecimiento tri - mensual de la IED: 
plot(TM_IED_Manu, type="line", main="Gráfica 5: Tasa de Crecimiento tri - mensual de la IED de EU en el sector manufacturero de México, 1999 - 2022",
     col="blue", ylim=c(-280.150,287.816), ylab="Tasa Tri - Mensual en tanto por ciento",
     xlab="Periodo 1999-2022") 

#Gráfica de Tasa de Crecimiento anual y tri - mensual de las Exportaciones: 
plot(TA_IED_Manu, type="line", main="Gráfica 6: Tasa de Crecimiento Anual// tri- mensual de las Exportaciones de manufacturas de México a EU, 1999 - 2022",
     col="red", ylim=c(-280.150,287.816), ylab="Tasa Tri - Mensual en tanto por ciento",
     xlab="Periodo 1999-2022") 
lines(TM_IED_Manu, col="blue", lty=2)

plot(TA_IED_Manu, type="line", main="Gráfica 7: Tasa de Crecimiento Anual// tri- mensual de las Exportaciones de manufacturas de México a EU, 1999 - 2022",
     col="red", ylim=c(-280.150,287.816), ylab="Tasa Tri - Mensual en tanto por ciento",
     xlab="Periodo 1999-2022") 
lines(TM_IED_Manu, col="blue", lty=2)
abline(v=2020, col="green")
abline(v=2021, col="green")

#Gráfica comparativa anual IED y Exportaciones: 
plot(TA_IED_Manu, type="line", main="Gráfica 8: Tasa de Crecimiento Anual de la IED y Exportaciones, 1999 - 2022",
     col="red", ylim=c(-280.150,287.816), ylab="Tasa Tri - Mensual en tanto por ciento",
     xlab="    IED = RED        EXPORTACIONES = BLUE   ") 
lines(TA_Export_Manu, col="blue")
abline(v=2020, col="green")
abline(v=2021, col="green")
abline(h=-100, col="green")

# b. Analisis de Tendencias 

# Transformacion de indicadores
lnIED_Manu <-log(IED_Manu)
lnExport_Manu <-log(Export_Manu)

# Variable de tiempo y al cuadrado
ten <- time(PDatos$Año)
ten2 <- ten^2
View(ten2)

# Grafica de indicadores 
plot(IED_Manu, type="l",main = "Gráfica 9: La IED vinculada con el tiempo", col="red") 
plot(Export_Manu, type="l",main = "Gráfica 10: Las Exportaciones vinculadas con el tiempo", col="blue") 

plot(lnIED_Manu, type="l",main = "Gráfica 11: Logaritmo natural de la IED: con Tendencia", col="red") 
plot(lnExport_Manu, type="l",main = "Gráfica 12: Logaritmo natural de las Exportaciones: con Tendencia", col="blue") 

# Tendencia lineal
mod_lin <- lm(IED_Manu ~ ten)
summary(mod_lin)
mod_lin2 <- lm(Export_Manu ~ ten)
summary(mod_lin2)

# Tendencia cuadratica
mod_cua <- lm(IED_Manu ~ ten+ten2)
summary(mod_cua)
mod_cua2 <- lm(Export_Manu ~ ten+ten2)
summary(mod_cua2)

# Tendencia exponencial
mod_exp <- lm(log(IED_Manu) ~ ten)
summary(mod_exp)
mod_exp2 <- lm(log(Export_Manu) ~ ten)
summary(mod_exp2)

# Prediccion de tendencias T
ten_lin <- ts(predict(mod_lin), frequency=4, start=c(1999,1))
ten_cua <- ts(predict(mod_cua), frequency=4, start=c(1999,1))
ten_exp <- ts(exp(predict(mod_exp)), frequency=4, start=c(1999,1))

ten_lin2 <- ts(predict(mod_lin2), frequency=4, start=c(1999,1))
ten_cua2 <- ts(predict(mod_cua2), frequency=4, start=c(1999,1))
ten_exp2 <- ts(exp(predict(mod_exp2)), frequency=4, start=c(1999,1))

plot(IED_Manu, main = "Gráfica 12: Tendencias de la Inversión Extranjera Directa")
lines(ten_lin, col = "black")
lines(ten_cua, col = "blue")
lines(ten_exp, col = "red")

plot(Export_Manu, main = "Gráfica 13: Tendencias de las Exportaciones")
lines(ten_lin2, col = "black")
lines(ten_cua2, col = "blue")
lines(ten_exp2, col = "red")

#Las variables sin Tendencia

plot(IED_Manu-ten_lin, main = "Gráfica 14: IED sin Tendencia Lineal, Cuádratica y Exponencial")
lines(IED_Manu-ten_cua, col = "blue")
lines(IED_Manu/ten_exp, col = "red")
plot(IED_Manu/ten_exp, main = "Gráfica 15: IED sin Tendencias")

plot(Export_Manu-ten_lin2, main = "Gráfica 16: Exportaciones sin Tendencia Lineal, Cuádratica y Exponencial")
lines(Export_Manu-ten_cua2, col = "blue")
lines(Export_Manu/ten_exp2, col = "red")
plot(Export_Manu/ten_exp2, main = "Gráfica 17: Exportaciones sin Tendencias")

# Residuales de los modelos #el residual representa la variable sin tendencia 
u_lin <- ts(resid(mod_lin), frequency=4, start=c(1999,1))  
u_cua <- ts(resid(mod_cua), frequency=4, start=c(1999,1))
u_exp <- ts(resid(mod_exp), frequency=4, start=c(1999,1)) 

u_lin2 <- ts(resid(mod_lin2), frequency=4, start=c(1999,1))  
u_cua2 <- ts(resid(mod_cua2), frequency=4, start=c(1999,1))
u_exp2 <- ts(resid(mod_exp2), frequency=4, start=c(1999,1)) 

# Graficas individuales de variable sin tendencias 
plot(u_lin, main = "Gráfica 18: IED sin tendencia lineal", type="l")
plot(u_cua, main = "Gráfica 19: IED sin tendencia cuadratica", type="l")
plot(u_exp, main = "Gráfica 20: IED sin tendencia exponencial", type="l") 

plot(u_lin2, main = "Gráfica 21: Exportaciones sin tendencia lineal", type="l")
plot(u_cua2, main = "Gráfica 22: Exportaciones sin tendencia cuadratica", type="l")
plot(u_exp2, main = "Gráfica 23: Exportaciones tendencia exponencial", type="l")

# Normalizar las series sin tendencias
u_lin_nor <- (u_lin-mean(u_lin))/sd(u_lin)  
u_cua_nor <- (u_cua-mean(u_cua))/sd(u_cua)  
u_exp_nor <- (u_exp-mean(u_exp))/sd(u_exp)

u_lin_nor2 <- (u_lin2-mean(u_lin2))/sd(u_lin2)  
u_cua_nor2 <- (u_cua2-mean(u_cua2))/sd(u_cua2)  
u_exp_nor2 <- (u_exp2-mean(u_exp2))/sd(u_exp2)

# Grafica con la compracion de variables sin tendencias normalizadas
plot(u_lin_nor, main = "Gráfica 24: IED sin tendencias normalizadas")
lines(u_cua_nor, col ="green")
lines(u_exp_nor, col ="red")

plot(u_lin_nor2, main = "Gráfica 25: Exportaciones sin tendencias normalizadas")
lines(u_cua_nor2, col ="green")
lines(u_exp_nor2, col ="red")

#Gráficas por el método de diferencias 
plot(diff(IED_Manu,1), main = "Gráfica 26: IED sin tendencias por el método de diferencias")  
lines(diff(IED_Manu,2), col ="green") 
lines(diff(log(IED_Manu),1), col ="red") 
plot(diff(log(IED_Manu),1), main = "Gráfica 27: IED sin tendencias con método de diferencias") 

plot(diff(Export_Manu,1), main = "Gráfica 28: Exportaciones sin tendencias por el método de diferencias")  
lines(diff(Export_Manu,2), col ="green") 
lines(diff(log(Export_Manu),1), col ="red") 
plot(diff(log(Export_Manu),1), main = "Gráfica 29: IED sin tendencias con método de diferencias")

#c. Factor Estacional (ojo sin la cosntante)

# identificar como series de tiempo y ordenar
IED_M <- ts(PDatos[with(PDatos, order(PDatos$Año)),]$IED, frequency=4, start=c(1999,1))
Export_M <- ts(PDatos[with(PDatos, order(PDatos$Año)),]$Export, frequency=4, start=c(1999,1))

# Graficar el comportamiento de la series
plot(IED_M, main="Gráfica 30: Inversión Extranjera Directa de EU al sector manufacturero de México, 1999 - 2022")
plot(Export_M, main= "Gráfica 31: Exportaciones manufactureras de México a EU, 1999 - 2022")

# Construir una matriz con dummies estacionales
dum_men_IED <- seasonaldummy(IED_M)
View(dum_men_IED)
dum_men_var_IED <- as.data.frame(dum_men_IED) #se convierte un objeto en objeto de datos 
View(dum_men_var_IED)
dum_men_var_IED$Q4 <- ifelse(PDatos[with(PDatos, order(PDatos$Año)),]$Año == "Q4", 1, 0)   
View(dum_men_var_IED)

dum_men_Exp <- seasonaldummy(Export_M)
View(dum_men_Exp)
dum_men_var_Exp <- as.data.frame(dum_men_Exp) #se convierte un objeto en objeto de datos 
View(dum_men_var_Exp)
dum_men_var_Exp$Q4 <- ifelse(PDatos[with(PDatos, order(PDatos$Año)),]$Año == "Q4", 1, 0)   
View(dum_men_var_Exp)

# ============= Modelo para el IED ==========================
#Estimar modelo estacional para el nivel de la variable (IED) 
#con dummies y sin la constante (-1 en la especificación)
mod_IED_sin  <- lm(IED_M ~ -1+dum_men_IED) 
summary(mod_IED_sin) 
mod_Exp_sin  <- lm(Export_M ~ -1+dum_men_IED) 
summary(mod_Exp_sin)

# graficar el comportamiento sin tendencias lineal
plot(diff(IED_M,1), main= "Gráfica 32: IED sin tendencia lineal, 1999 - 2022")  
plot(diff(Export_M,1), main= "Gráfica 33: Exportaciones sin tendencia lineal, 1999 - 2022")

# Estimar modelo estacional para variable sin tendencia lineal
mod_IED_1  <- lm(diff(IED_M,1) ~ -1+ Q1+ Q2+ Q3+ Q4,
                 data=dum_men_var_IED[2:94, 1:4]) 
summary(mod_IED_1) 
mod_Exp_1  <- lm(diff(Export_M,1) ~ -1+ Q1+ Q2+ Q3+ Q4,
                 data=dum_men_var_Exp[2:94, 1:4]) 
summary(mod_Exp_1) 

# Estimar modelo estacional para variable sin tendencia lineal
#         (diff(ive)  con dummies signficativas y sin constante 
mod_IED_2  <- lm(diff(IED_M,1) ~ -1+ Q1+ Q3,
                 data=dum_men_var_IED[2:94, 1:4]) 
summary(mod_IED_2)
mod_Exp_2  <- lm(diff(Export_M,1) ~ -1+ Q1+ Q2+ Q3,
                 data=dum_men_var_Exp[2:94, 1:4]) 
summary(mod_Exp_2)

# graficar variable sin tendencia cuadratica
plot(diff(IED_M,2), main= "Gráfica 34: IED sin tendencia cuadrática, 1999 - 2022")
plot(diff(Export_M,2), main= "Gráfica 35: Exportaciones sin tendencia cuadrática, 1999 - 2022")

# Estimar modelo estacional para variable sin tendencia cuadratica
#         (diff(iae,2)  con dummies
mod_IED_3  <- lm(diff(IED_M,2) ~ -1+ Q1+ Q2+ Q3+ Q4,
                 data=dum_men_var_IED[3:94, 1:4])   
summary(mod_IED_3)
mod_Exp_3  <- lm(diff(Export_M,2) ~ -1+ Q1+ Q2+ Q3+ Q4,
                 data=dum_men_var_Exp[3:94, 1:4]) 
summary(mod_Exp_3)

# Estimar modelo estacional para variable sin tendencia cuadratica
#         (diff(ive,2)  con dummies signficativas y sin constante
mod_IED_4  <- lm(diff(IED_M,2) ~ -1+ Q1+ Q3,
                 data=dum_men_var_IED[3:94, 1:4])   
summary(mod_IED_4) 

# graficar variable sin tendencia exponencial
plot(diff(log(IED_M),1), main= "Gráfica 36: IED sin tendencia exponencial, 1999 - 2022")
plot(diff(log(Export_M),1), main= "Gráfica 37: Exportaciones sin tendencia exponencial, 1999 - 2022")

# Estimar modelo estacional para variable sin tendencia exponencial
#         (diff(log(ive))  con dummies y sin constante
mod_IED_5  <- lm(diff(log(IED_M)) ~ -1+Q1 + Q2 + Q3+ Q4, 
                 data=dum_men_var_IED[2:94, 1:4])
summary(mod_IED_5)
mod_Exp_5 <- lm(diff(log(Export_M)) ~ -1+Q1+Q2+Q3+Q4,
                data = dum_men_var_Exp[2:94, 1:4])
summary((mod_Exp_5))

# Estimar modelo estacional para variable sin tendencia exponencial
#         (diff(log(ive))  con dummies signficativas y sin constante
mod_IED_6  <- lm(diff(log(IED_M)) ~ -1+ Q2 + Q3, 
                 data=dum_men_var_IED[2:94, 1:4])
summary(mod_IED_6) 

# ======== Eliminar factores estacionales (desestacionalizar)
# Eliminar factor estacional de variable sin tendencia lineal
factor_esta_IED <- fitted(mod_IED_2) #se va a guardar el resultado de resolver la ecuación dandonos el factor estacional para la variable sin tendencia lineal
IED_desL  <- diff(IED_M)-factor_esta_IED #se le quita a la variable el factor estacional para tener una variable sin tendencia lineal y desestacionalizada 
plot(diff(IED_M), main="Gráfica 38: IED sin tendencia lineal con (black) y sin factor estacional (red)") 
lines(IED_desL, col ="red") #se incluye la variable sin tendencia lineal y desestacionalizada

# Eliminar factor estacional de variable sin tendencia cuadratica 
factor_esta <- fitted(mod_IED_4)
IED_desC  <- diff(IED_M,2)-factor_esta
plot(diff(IED_M,2), main="Gráfica 39: IED sin tendencia cuadratica con (black) y sin factor estacional (purple)")
lines(IED_desC, col ="purple")

# Eliminar factor estacional de variable sin tendencia exponencial
factor_esta <- fitted(mod_IED_6)
IED_desE  <- diff(log(IED_M))-factor_esta
plot(diff(log(IED_M)),main="Gráfica 40: IED sin tendencia exponecial con (black) y sin factor estacional (cyan)")
lines(IED_desE, col ="cyan")

#d. Ciclos con medias móviles 

# Ordenar la series e identificar para mineria
IED_m <- ts(PDatos[with(PDatos, order(PDatos$Año)),]$IED, frequency=4, start=c(1999,1))
Export_m <- ts(PDatos[with(PDatos, order(PDatos$Año)),]$Export, frequency=4, start=c(1999,1))
plot(IED_m, main="Gráfica 41: Inversión Extranjera Directa de EU al sector manufacturero de México, 1999 - 2022")
plot(Export_m, main= "Gráfica 42: Exportaciones manufactureras de México a EU, 1999 - 2022")

# ============= Modelo para la IED y las Exportaciones =========================
# Construir una matriz con dummies estacionales
dum_men_IED_m <- seasonaldummy(IED_m)
dum_men_var_IED_m <- as.data.frame(dum_men_IED_m)
dum_men_var_IED_m$Q4 <- ifelse(PDatos[with(PDatos, order(PDatos$Año)),]$Año == "Q4", 1, 0)   
dum_men_Export_m <- seasonaldummy(Export_m)
dum_men_var_Export_m <- as.data.frame(dum_men_Export_m)
dum_men_var_Export_m$Q4 <- ifelse(PDatos[with(PDatos, order(PDatos$Año)),]$Año == "Q4", 1, 0)
View(dum_men_var_IED_m)
View(dum_men_var_Export_m)

# Estimar modelo estacional para variable sin tendencia exponencial
#         (diff(log(igae))  con dummies
mod_IED_m_1  <- lm(diff(log(IED_m)) ~ -1+Q1+Q2+Q3+Q4,
                  data=dum_men_var_IED_m[2:94, 1:4]) 
summary(mod_IED_m_1)

mod_IED_m_2  <- lm(diff(log(IED_m)) ~ -1+Q2+Q3,
                  data=dum_men_var_IED_m[2:94, 1:4])
summary(mod_IED_m_2)

mod_Export_m_1  <- lm(diff(log(Export_m)) ~ -1+Q1+Q2+Q3+Q4,
                   data=dum_men_var_Export_m[2:94, 1:4])  
summary(mod_Export_m_1)

mod_Export_m_2  <- lm(diff(log(Export_m)) ~ -1+Q3,
                   data=dum_men_var_Export_m[2:94, 1:4])
summary(mod_Export_m_2)

# ======== Eliminar factores estacionales (desestacionalizar)
# Eliminar factor estacional de variable sin tendencia exponencial
factor_esta_IED_mE <- fitted(mod_IED_m_2)
IED_m_desE  <- diff(log(IED_m))-factor_esta
plot(diff(log(IED_m)),main="Gráfica 43: IED sin tendencia exponencial con (black) y sin (red) factores estacionales")
lines(IED_m_desE, col ="red")

factor_esta <- fitted(mod_Export_m_2)
Export_m_desE  <- diff(log(Export_m))-factor_esta
plot(diff(log(Export_m)),main="Gráfica 44: Exportaciones sin tendencia exponencial con (black) y sin (purple) factores estacionales")
lines(Export_m_desE, col ="purple")

# Media movil no centrada de un lado de series sin tendencia exponencial
ma3_nocen_IED_m <- filter(IED_m_desE, sides=1, rep(1,3)/3)  
ma5_nocen_IED_m <- filter(IED_m_desE, sides=1, rep(1,5)/5)
ma10_nocen_IED_m <- filter(IED_m_desE, sides=1, rep(1,10)/10)
ma15_nocen_IED_m <- filter(IED_m_desE, sides=1, rep(1,15)/15)
plot(ma3_nocen_IED_m, main="Gráfica 45: Media móvil no centrada de la IED original sin tendencia exponencial y desestacionalizada (gray) con promedio movil de 15 (blue)") 
lines(ma5_nocen_IED_m, col ="purple")  
lines(ma10_nocen_IED_m, col ="red" )  
lines(ma15_nocen_IED_m, col ="blue" )  
lines(IED_m_desE, col ="gray") 

ma3_nocen_Export_m <- filter(Export_m_desE, sides=1, rep(1,3)/3)  
ma5_nocen_Export_m <- filter(Export_m_desE, sides=1, rep(1,5)/5)
ma10_nocen_Export_m <- filter(Export_m_desE, sides=1, rep(1,10)/10)
ma15_nocen_Export_m <- filter(Export_m_desE, sides=1, rep(1,15)/15)
plot(ma3_nocen_Export_m, main="Gráfica 46: Media móvil no centrada de las Exportaciones original sin tendencia exponencial y desestacionalizada (gray) con promedio movil de 5 (purple)") 
lines(ma5_nocen_Export_m, col ="purple")  
lines(ma10_nocen_Export_m, col ="red" )  
lines(ma15_nocen_Export_m, col ="blue" )  
lines(Export_m_desE, col ="gray") 


# Revisar la media movil y perdida de datos
print(ma3_nocen_IED_m) #se aplica un promedio movil no centrado con 3 elementos, esperando: que se pierdan 2 datos, además como se le había aplicado la 1era dif log ya se había perdido el primer dato  
print(ma5_nocen_IED_m) #se pierden 4 datos 
print(ma10_nocen_IED_m) #se pierden 9 datos 
print(ma15_nocen_IED_m) #se pierden 14 datos
print(ma3_nocen_Export_m) #se aplica un promedio movil no centrado con 3 elementos, esperando: que se pierdan 2 datos, además como se le había aplicado la 1era dif log ya se había perdido el primer dato  
print(ma5_nocen_Export_m) #se pierden 4 datos 
print(ma10_nocen_Export_m) #se pierden 9 datos 
print(ma15_nocen_Export_m) #se pierden 14 datos

# Media movil centrada de dos lados series sin tendencia exponencial 
ma3_cen_IED_m <- filter(IED_m_desE, sides=2, rep(1,3)/3)  
ma5_cen_IED_m <- filter(IED_m_desE, sides=2, rep(1,5)/5)
ma10_cen_IED_m <- filter(IED_m_desE, sides=2, rep(1,10)/10)
ma15_cen_IED_m <- filter(IED_m_desE, sides=2, rep(1,15)/15)
plot(ma3_cen_IED_m, main="Gráfica 47: Media móvil centrada de la IED original sin tendencia exponencial y desestacionalizada (gray) con promedio movil de 15 (blue)") 
lines(ma5_cen_IED_m, col ="purple")  
lines(ma10_cen_IED_m, col ="red" )  
lines(ma15_cen_IED_m, col ="blue" )  
lines(IED_m_desE, col ="gray") 

ma3_cen_Export_m <- filter(Export_m_desE, sides=2, rep(1,3)/3)  
ma5_cen_Export_m <- filter(Export_m_desE, sides=2, rep(1,5)/5)
ma10_cen_Export_m <- filter(Export_m_desE, sides=2, rep(1,10)/10)
ma15_cen_Export_m <- filter(Export_m_desE, sides=2, rep(1,15)/15)
plot(ma3_cen_Export_m, main="Gráfica 48: Media móvil centrada de las Exportaciones original sin tendencia exponencial y desestacionalizada (gray) con promedio movil de 15 (blue)") 
lines(ma5_cen_Export_m, col ="purple")  
lines(ma10_cen_Export_m, col ="red" )  
lines(ma15_cen_Export_m, col ="blue" )  
lines(Export_m_desE, col ="gray") 

# Revisar la media movil y perdida de datos: centrado 
print(ma3_cen_IED_m) #pierde 2 elementos: uno al principio y el otro al final. De acuerdo con la serie: perdiendo agosto en 2022 al final y al principio se perdió febrero 1993 
print(ma5_cen_IED_m) #se pierden 2 al principio y 2 al final: 2 de agosto de 2022 al final y al principio febrero y marzo de 1993 
print(ma10_cen_IED_m) #analíticamente no se puede porque son elementos pares y se necesita numeros impares; esperando que se perdieran 9 elementos: 4 al principio y 5 al final. R si lo hace pero analíticamente no es correcto. 
print(ma15_cen_IED_m)

print(ma3_cen_Export_m) #pierde 2 elementos: uno al principio y el otro al final. De acuerdo con la serie: perdiendo agosto en 2022 al final y al principio se perdió febrero 1993 
print(ma5_cen_Export_m) #se pierden 2 al principio y 2 al final: 2 de agosto de 2022 al final y al principio febrero y marzo de 1993 
print(ma10_cen_Export_m) #analíticamente no se puede porque son elementos pares y se necesita numeros impares; esperando que se perdieran 9 elementos: 4 al principio y 5 al final. R si lo hace pero analíticamente no es correcto. 
print(ma15_cen_Export_m)

#e. Filtro Hodrick-Prescott

# identificar y ordenar la series 
Export_Des <- ts(PDatos[with(PDatos, order(PDatos$Año)),]$Export, frequency=4, start=c(1999,1))
IED_desL
# Gráficas de las series a trabajar 
plot(Export_Des, main = "Gráfica 49: Exportaciones de Manufacturas de México a EU, originalmente desestacionalizda")
plot(IED_desL, main = "Gráfica 50: IED sin Factor Estacional y sin tendencia lineal, desestacionalizada")

# ======= Filtro Hodrick-Prescott (HP) para obtener =========
#               tendencia y ciclo de las Exportaciones  
Export_Des.hp <- hpfilter(Export_Des) 
summary(Export_Des.hp)
plot(Export_Des.hp) #Las exportaciones con la tendencia 
View(Export_Des.hp)

# El valor estimado de lambda
summary(Export_Des.hp$lambda)  

# Aplicacion del filtro con distintas frecuencias de Lambda
Export_Des.hp1 <- hpfilter(Export_Des, freq=129600, type = "lambda")  
Export_Des.hp2 <- hpfilter(Export_Des, freq=100000, type = "lambda") 
Export_Des.hp3 <- hpfilter(Export_Des, freq=50000, type = "lambda")
Export_Des.hp4 <- hpfilter(Export_Des, freq=10000, type = "lambda")

summary(Export_Des.hp1)
summary(Export_Des.hp2)
summary(Export_Des.hp3)
summary(Export_Des.hp4)

# Grafica con tendencias de acuerdo al HP y distitas frecuencias 
plot(Export_Des.hp1$x,    
     main="Gráfcia 52: Exportaciones: Tendencias con Filtro Hodrick-Prescott",
     col=1, ylab="")
lines(Export_Des.hp1$trend,col=2) #con la tendencia que genera el procedimiento de acuerdo a la lambda
lines(Export_Des.hp2$trend,col=3)
lines(Export_Des.hp3$trend,col=4)
lines(Export_Des.hp4$trend,col=5)
legend("topleft",legend=c("Export", "lambda=129,600", "lambda=100,000",
                          "lambda=50,000", "lambda=10,000"), col=1, cex=.25)

# Grafica con los ciclos de acuerdo al HP y distitas frecuencias #la variable menos la tendencia es el ciclo: comparado con lambdas
plot(Export_Des.hp1$cycle,
     main="Gráfica 53: Exportaciones: Ciclos con Filtro Hodrick-Prescott",
     col=2, ylab="", ylim=range(Export_Des.hp4$cycle,na.rm=TRUE))
lines(Export_Des.hp2$cycle,col=3)
lines(Export_Des.hp3$cycle,col=4)
lines(Export_Des.hp4$cycle,col=5)
legend("bottomleft",legend=c("Export", "lambda=129,600", "lambda=100,000",
                             "lambda=50,000", "lambda=10,000"), col=1, cex=.25)

# ======= Filtro Hodrick-Prescott (HP) para obtener =========
#               tendencia y ciclo de la IED
IED_desL.hp <- hpfilter(IED_desL) 
summary(IED_desL.hp)
plot(IED_desL.hp) #La IED con la tendencia 
View(IED_desL.hp)

IED_desE.hp <- hpfilter(IED_desE) 
plot(IED_desE.hp) #La IED con la tendencia 

IED_desC.hp <- hpfilter(IED_desC) 
plot(IED_desC.hp) #La IED con la tendencia 

# El valor estimado de lambda
summary(IED_desL.hp$lambda) # cambiar la identificacion del filtro

# Aplicacion del filtro con distintas frecuencias de Lambda
IED_desL.hp1 <- hpfilter(IED_desL, freq=1600, type = "lambda") #el lambda para datos trimestrales es 1600
IED_desL.hp2 <- hpfilter(IED_desL, freq=1000, type = "lambda") #aqui solo se modifica para ver el comparativo 
IED_desL.hp3 <- hpfilter(IED_desL, freq=500, type = "lambda")
IED_desL.hp4 <- hpfilter(IED_desL, freq=100, type = "lambda")

summary(IED_desL.hp1)
summary(IED_desL.hp2)
summary(IED_desL.hp3)
summary(IED_desL.hp4)

# Grafica con tendencias de acuerdo al HP y distitas frecuencias
plot(IED_desL.hp1$x,
     main="Gráfica 57: IED desestacionalizada: Tendencias con Filtro Hodrick-Prescott",
     col=1, ylab="")
lines(IED_desL.hp1$trend,col=2)
lines(IED_desL.hp2$trend,col=3)
lines(IED_desL.hp3$trend,col=4)
lines(IED_desL.hp4$trend,col=5)
legend("topleft",legend=c("IED", "lambda=1600", "lambda=1000",
                          "lambda=500", "lambda=100"), col=1, cex=.3)

# Grafica con los ciclos de acuerdo al HP y distitas frecuencias
plot(IED_desL.hp1$cycle,
     main="Gráfica 58: IED desestacionalizada: Ciclos con Filtro Hodrick-Prescott",
     col=2, ylab="", ylim=range(IED_desL.hp4$cycle,na.rm=TRUE))
lines(IED_desL.hp2$cycle,col=3)
lines(IED_desL.hp3$cycle,col=4)
lines(IED_desL.hp4$cycle,col=5)
legend("bottomleft",legend=c("IED", "lambda=1600", "lambda=1000",
                             "lambda=500", "lambda=100"), col=1, cex=.25)

#III.	Análisis de propensiones y elasticidades con modelos
#a.	Coeficientes de correlación, histogramas y distribuciones entre los indicadores

library(dplyr)
library(psych)

names(PDatos)
df<-select(PDatos,IED,Export)
View(df)
pairs.panels(df)
correla<-cor(df)
print(correla)

# b.	Especificación de los modelos: lineal, log-log, lineal-log y log-lineal // c.	Análisis de resultados econométricos (Significancia de coeficientes y R2)

# ==================== Modelos lineal ===================== 
Mod_lin  <- lm(Export_Manu ~ IED_Manu)
summary(Mod_lin)

# Propensiones IED_Manu
pm_IED_Manu_lin  <- 2547 

# Elasticidades tasa de interes
IED_Manu_lin <- 2547*(IED_Manu/Export_Manu) 
plot(IED_Manu_lin, main="Gráfica 60: Elasticidad IED --> Exportaciones",col="purple") 
ep_IED_Manu_lin <- 2547*(mean(IED_Manu)/mean(Export_Manu))  

# =============== Modelos no-lineal (log-log) =====================
Mod_log_log  <- lm(log(Export_Manu) ~ log(IED_Manu))
summary(Mod_log_log) 

# Elasticidades IED
e_IED_Manu_log_log   <- 0.09692 

# Propensiones IED
pm_IED_Manu_log_log <- 0.09692*(Export_Manu/IED_Manu) 
plot(pm_IED_Manu_log_log, main="Gráfica 61: Propensión de la IED ---> Exportaciones", col="blue")  
pmp_IED_Manu_log_log <- 0.09692*(mean(Export_Manu)/mean(IED_Manu))  

# =============== Modelos no-lineal (log-lineal) =====================
Mod_log_lin  <- lm(log(Export_Manu) ~ IED_Manu) 
summary(Mod_log_lin)   

# Propensiones IED
pm_IED_Manu_log_lin   <- 5.496e-05*Export_Manu  
plot(pm_IED_Manu_log_lin, main="Gráfica 62: Propensión Log Exportaciones-IED, Modelo log-lineal",col="red") 
pmp_IED_Manu_log_lin   <- 5.496e-05*(mean(Export_Manu))  

# Elasticidades IED
e_IED_Manu_log_lin   <- 5.496e-05*IED_Manu
plot(e_IED_Manu_log_lin, main="Gráfica 63: Elasticidad Log Exportaciones-IED, Modelo log-lineal",col="black")
ep_IED_Manu_log_lin   <- 5.496e-05*(mean(IED_Manu))

# =============== Modelos no-lineal (lineal-log) =====================
Mod_lin_log  <- lm(Export_Manu ~ log(IED_Manu))
summary(Mod_lin_log)

# Propensiones IED
pm_IED_Manu_lin_log   <- 4947749/Export_Manu
plot(pm_IED_Manu_lin_log, main="Gráfica 64: Propensión Exportaciones-IED, Modelo Lineal-Log",col="green")
pmp_IED_Manu_lin_log   <- 4947749/(mean(Export_Manu))

# Elasticidades IED
e_IED_Manu_lin_log   <- 4947749/Export_Manu
plot(e_IED_Manu_lin_log, main="Gráfica 65: Elasticidad Exportaciones-IED, Modelo Lineal-Log",col="gray")
ep_IED_Manu_lin_log   <- 4947749/(mean(Export_Manu))

# e.	Análisis de resultados de propensiones y elasticidades

# Resumen de propensiones 
# IED
propen_IED_Manu <- c(pm_IED_Manu_lin, pmp_IED_Manu_log_log, pmp_IED_Manu_log_lin, pmp_IED_Manu_lin_log)  
print(propen_IED_Manu)
summary(propen_IED_Manu)

# Resumen de elasticidades: 
# IED
elas_IED_Manu <- c(ep_IED_Manu_lin, e_IED_Manu_log_log, ep_IED_Manu_log_lin, ep_IED_Manu_lin_log) 
print(elas_IED_Manu)  
summary(elas_IED_Manu)  


