install.packages("nortest",dependencies = TRUE)
install.packages('car',dependencies = TRUE) 
install.packages('faraway',dependencies = TRUE) 
install.packages('tseries',dependencies = TRUE) 

#Cargar las bibliotecas
library(faraway)#linealidad 
library(tseries)#acf 
library(nortest) #test de normalidad 
library(car) #para qqPlot y levene 

dataset=read.csv('UScrime.csv',sep = ',',header = TRUE)
#y=dataset[,1]
X=subset(dataset, select = -y)
dataset[1:2,]
dim(dataset)

modelo=lm(y~., data=dataset)
summary(modelo)

options(repr.plot.width=7, repr.plot.height=7)
opar=par(mfrow=c(2,2))
# Residuos parciales para 1,3,4,5
prplot(modelo,1)
prplot(modelo,3)
prplot(modelo,4)
prplot(modelo,5)
Xpar(opar)

X  Y
2  1 1.5
3  4  3
4  3   


[1,2,3,4,5,6,7,8,10]

options(repr.plot.width=5,repr.plot.height=3)
resid = modelo$res
# Prueba de Independencia
acf(resid) 

runs.test(as.factor(resid > 0))

grupo = c(rep(1, 15), rep(2, 15), rep(3,17))
bartlett.test(resid, grupo)

grupo = as.factor(c(rep(1, 15), rep(2, 15), rep(3,17)))
leveneTest(resid, grupo)

opar=par(mfrow=c(1,2))
options(repr.plot.width=7,repr.plot.height=4)
qqPlot(resid)
hist(resid,freq=FALSE, breaks=8, main="Histograma de los residuales",
     col="cornflowerblue",xlim = c(min(resid), max(resid)))
lines(density(resid),col="red")
par(opar)

ad.test(resid)
lillie.test(resid)
cvm.test(resid)

vif(modelo)

# Leverage
p = dim(X)[2]
n =  dim(X)[1]
# La funcion hat obtiene la diagonal de matriz sombrero
h = hat(X)
options(repr.plot.width=7,repr.plot.height=3.5)
plot(h, xlab = "index", ylab = "leverage",col="blue")
abline(h = 2 * (p + 1)/n)
text(h, pos = 1)

s2 = anova(modelo)$"Mean Sq"[11]
resstud = resid/sqrt(s2 * (1 - h))
plot(resstud, xlab = "Index", ylab = "Residuos Estudentizados", ylim = c(-4, 4))
abline(h = c(-3, 3))
text(resstud, pos = 1)
