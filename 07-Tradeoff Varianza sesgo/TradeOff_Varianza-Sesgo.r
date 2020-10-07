#install.packages("plotly") #correr esta linea si no tiene instalado plotly
library(plotly)
#.libPaths( c( .libPaths(), "/home/jimmy/R/x86_64-pc-linux-gnu-library/3.4") )

f = function(x) x^2

f_datos_sint = function(f,  n= 100) {
  x   = runif(n , min = 0, max = 1)
  err = rnorm(n , mean = 0, sd = 0.5)
  y   = f(x) + err
  data.frame(x, y)
}

data_sint= f_datos_sint(f)
mod_0 = lm(y ~ 1,data = data_sint)
mod_1 = lm(y ~ poly(x, degree = 1), data = data_sint)
mod_2 = lm(y ~ poly(x, degree = 2), data = data_sint)
mod_3 = lm(y ~ poly(x, degree = 5), data = data_sint)

options(repr.plot.width=8, repr.plot.height=7)
plot(y ~ x, data = data_sint, col = "black", pch = 20,main = "Ajuste por Polinomios a los datos generados")
sec = seq(from = 0, to = 3, by = 0.01)
x=data.frame(x = sec)
lines(sec, f(sec), col = "black", lwd = 2)
lines(sec, predict(mod_0, x), col = "blue",  lwd = 2)
lines(sec, predict(mod_1, x), col = "red",   lwd = 2)
lines(sec, predict(mod_2, x), col = "purple", lwd = 2)
lines(sec, predict(mod_3, x), col = "orange",  lwd = 2)
legend("topleft", c("y ~ 1", "y ~ poly(x, 1)", "y ~ poly(x, 2)",  "y ~ poly(x, 4)", "x^2"), 
col = c("blue", "red", "purple", "orange", "black"), lty = c(1, 1, 1, 1, 1))

simulaciones = 200  #numero de simulaciones
nomodelos    = 4 #numero de modelos
x = data.frame(x = 0.8) #punto en el que vamos a hacer predicciones
predicciones = matrix(0, nrow = simulaciones, ncol = nomodelos)

for (i in 1:simulaciones) {
   #generamos la data 
  data_sint = f_datos_sint(f)

  # hacemos el ajuste de los modelos
  mod_0 = lm(y ~ 1, data = data_sint)
  mod_1 = lm(y ~ poly(x, degree = 1), data = data_sint)
  mod_2 = lm(y ~ poly(x, degree = 2), data = data_sint)
  mod_3 = lm(y ~ poly(x, degree = 5), data = data_sint)

  # obtenemos las predicciones para cada modelo
  predicciones[i, 1] = predict(mod_0, x)
  predicciones[i, 2] = predict(mod_1, x)
  predicciones[i, 3] = predict(mod_2, x)
  predicciones[i, 4] = predict(mod_3, x)
}

#verificamos las predicciones
head(predicciones,2)

sesgo = function(estimado, valorverdadero) {
  mean(estimado) - valorverdadero
}
varianza = function(estimado) {
  mean((estimado - mean(estimado)) ^ 2)
}
sesgomodelos    = apply(predicciones, 2, sesgo, valorverdadero = f(x = 0.8))
varianzamodelos = apply(predicciones, 2, varianza)
print("y ~ 1        y ~ poly(x, 1)      y ~ poly(x, 2)      y ~ poly(x, 5)")
print("Sesgo")
sesgomodelos
print("Varianza")
varianzamodelos

fig <- plot_ly(y =predicciones[,1] , type = "box",boxmean=TRUE, name= "y ~ 1")
fig <- fig %>% add_trace(y = predicciones[,2],boxmean=TRUE,  name= "y ~ poly(x, 1)")
fig <- fig %>% add_trace(y = predicciones[,3], boxmean=TRUE, name= "y ~ poly(x, 2)")
fig <- fig %>% add_trace(y = predicciones[,4],boxmean=TRUE,name="y ~ poly(x, 4)")
fig <- fig %>% layout(title = "Boxplot para los diferentes grados del polinomio")
fig
