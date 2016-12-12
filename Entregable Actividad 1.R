
# Vamos a subir los datos

house_train=read.csv("house_train.csv")




# 1º Fase: Calidad de los Datos.



head(house_train)

summary (house_train)

nrow (house_train)

str(house_train)

dim(house_train)

nrow(unique(house_train[1]))




# Acciones de limpiado de datos:


# Creamos un arcivo con el año y lo añadimos a nuestro data frame

library(data.table)

year=read.delim("year.txt")

head(year)


total <- cbind(house_train,year)

head(total)

# borramos la columna de la fecha que tiene T000000 y el segundo id fruto del cbind

total <- subset( total, select = -2 )

total <- subset( total, select = -21 )

head(total)

# Vamos a borrar las entradas que tengan 0 habitaciones según el fichero 


total2 <- total[!total$bedrooms == "0", ]

head(total2)

nrow(total)
nrow(total2)


# borramos los repetidos

house_train <- total2

house_train <- house_train[!duplicated(house_train[,c(1,4:21)]),]

nrow(house_train)



# echamos un nuevo vistazo de nuevo a nuestro data set.


head(house_train)

summary (house_train)

nrow (house_train)

str(house_train)

# ya tenemos mejor los datos, sin factor.

dim(house_train)

# Tras la la fase de analisis y limpieza de datos. Vamos a por lo que nos piden.


# 1.- Analizar el efecto de la superficie de la vivienda en el precio de la vivienda

# para ello lo que vamos a hacer es un chequeo muy muy sencillo. Vamos a genear unn data frame con 
# la superficie de la vivienda y el precio. Que es exactamente lo que nos piden, aunque no lo necesarrio
# para un buen análisis.

precio_m2 <- house_train[,c(5, 2)]

head(precio_m2)

summary(precio_m2)

# OK, vamos a dibujarlo

plot(precio_m2)

# Y ahora en bonito :oP

library(ggplot2)

ggplot(precio_m2)

G <- ggplot(precio_m2, aes(sqft_living,price))
G + geom_point(aes(color=sqft_living))


# bueno, vamos a ver primero el modelo que nos piden y después ya trataremos de hacer un modelo mejor

mod1_precio_m2=lm(price~sqft_living,data=precio_m2)

plot(precio_m2$sqft_living,precio_m2$price)
abline(mod1_precio_m2,col="red",lty = "dashed")

# vamos a ver las caracteristicas del modelo que hemos hecho 

summary(mod1_precio_m2)

qqnorm(mod1_precio_m2$residuals); qqline(mod1_precio_m2$residuals,col=2)

plot(mod1_precio_m2$residuals)
smoothScatter(mod1_precio_m2$residuals)
hist(mod1_precio_m2$residuals)
confint(mod1_precio_m2,level=0.95)

-47824.434+(282.132*2) - (-47824.434+(282.132*1))


# Vamos a creamos un nuevo modelo. Esta vez porcentual


mod2_precio_m2=lm(log(price)~sqft_living,data=precio_m2)

summary(mod2_precio_m2)

plot(precio_m2$sqft_living,precio_m2$price)
abline(mod2_precio_m2,col="red",lty = "dashed")

# la linea en rojo esta tan abajo porque va en porcentage y cla, la escala es cientos de miles...

qqnorm(mod2_precio_m2$residuals); qqline(mod2_precio_m2$residuals,col=2)

plot(mod2_precio_m2$residuals)
smoothScatter(mod2_precio_m2$residuals)
hist(mod2_precio_m2$residuals)
confint(mod2_precio_m2,level=0.95)

log(1.2220+(0.0003970*2)) - log(1.2220+(0.0003970*1))

# vamos a comparar los dos modelos:


AIC(mod1_precio_m2) 
AIC(mod2_precio_m2)

# vemos que aún asi el resultado del modelo puede estar sesgado por los outliers

boxplot(precio_m2$price)

# Vamos a hacer un tercer modelo con estadística Robusta

if (!require("MASS")){
  install.packages("MASS") 
  library(MASS)
}

if (!require("caTools")){
  install.packages("caTools") 
  library(caTools)
}

mod3_precio_m2=rlm(price~sqft_living,data=precio_m2)


summary(mod3_precio_m2)

# comparamos 1er y 3r modelo

plot(precio_m2$sqft_living,precio_m2$price)
abline(mod1_precio_m2,col="red",lty = "dashed")
abline(mod3_precio_m2,col="blue",lty = "dashed")


# vemos como se corrige la línea de regresión

qqnorm(mod3_precio_m2$residuals); qqline(mod3_precio_m2$residuals,col=2)

plot(mod3_precio_m2$residuals)
smoothScatter(mod3_precio_m2$residuals)
hist(mod3_precio_m2$residuals)
confint.default(mod3_precio_m2,level=0.95)


# vamos a ver otros modelos

# Robusto por porcentaje

mod4_precio_m2=rlm(log(price)~sqft_living, data=precio_m2)

summary(mod4_precio_m2)


qqnorm(mod4_precio_m2$residuals); qqline(mod4_precio_m2$residuals,col=2)

plot(mod4_precio_m2$residuals)
smoothScatter(mod4_precio_m2$residuals)
hist(mod4_precio_m2$residuals)
confint.default(mod4_precio_m2,level=0.95)

# comparamos los cuatro

AIC(mod1_precio_m2) 
AIC(mod2_precio_m2)
AIC(mod3_precio_m2)
AIC(mod4_precio_m2)

# el mejor modelo nos da el segundo

# Elaboración del modeo predictivo


cor(house_train)


head(house_train)


#dividimos entre train y test

# primero carg


house_train$waterfront=as.factor(house_train$waterfront)
house_train$condition=as.factor(house_train$condition)
house_train$zipcode=as.factor(house_train$zipcode)



# Vamos a crear el modelo predictivo lineal por porcentage


mod5_predict=lm(log(price)~sqft_living+waterfront+condition+zipcode, data=house_train)

summary (mod5_predict)

qqnorm(mod5_predict$residuals); qqline(mod5_predict$residuals,col=2)

plot(mod5_predict$residuals)
smoothScatter(mod5_predict$residuals)
hist(mod5_predict$residuals)

AIC(mod5_predict)

# Ahora aplicamos el modelo predictivo al csv de las casas que tenemos que estimar

house_test=read.csv("house_test.csv")

house_test$waterfront=as.factor(house_test$waterfront)
house_test$condition=as.factor(house_test$condition)
house_test$zipcode=as.factor(house_test$zipcode)

house_test$price=exp(predict(mod5_predict,newdata=house_test,type="response"))
summary(house_test)
write.csv(house_test, "house_test_valorado.csv")


            

# 8oD  ------ THAT´s ALL FOLKS  -----------






