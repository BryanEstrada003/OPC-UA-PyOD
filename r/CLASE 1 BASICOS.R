
#Operaciones básicas

1 + 8
7 - 4 
8 / 8
7 * 9
2**3

#Comparación

8 < 6
7 >= 3
8 == 7

#Variables

#Asignar

a = 9
b <- 7

#Case sensitive
A = 2

a != A

a + b

c <- a  * b 
(d <- a / b)

e <- "Hello"

f <- FALSE

#Vectores

g <- c(1, 2, 4)

h <- c(3, 5, 6, 7)

i <- c("Hello", "world")

j <- c(FALSE, TRUE, T, F, TRUE)

g + h

2 * g

g * h

#POSICION DENTRO DEL VECTOR
g[1]
g[3]
g[4] = 8

#Funciones

sqrt(c+1)

is.logical(j)
is.numeric(i)
is.character(g)


#Suma

sum(h)

#Largo del vector

length(h)

#Promedio 

sum(h)/length(h)


#Medidas tendencia central

#Media

?mean

edad <- c(24, 26, 22, 28, 21, NA, 20,
          21, 22, 20, 19, 25, 20, 19, 20, 20, 
          19, 21, 26, 19)
edad

mean(edad)

mean(edad, na.rm = TRUE)


#Mediana

median(edad, na.rm = T)


#Moda


install.packages("DescTools")
library(DescTools)


Mode(edad, na.rm = T)


#Medidas de dispersion

#Varianza

var(edad, na.rm = T)

#Desviacion estandar

sqrt(var(edad, na.rm = T))
sd(edad, na.rm = T)


#Rango

range(edad, na.rm = T)

Range(edad, na.rm = T)

#Extremos

min(edad, na.rm = T)
max(edad, na.rm = T)

#Medidas de posicion

#Cuantiles

#Cuartiles

quantile(edad, na.rm = T)

?quantile

##


#Quintiles

quantile(x = edad, 
         probs = seq(.2, .8, .2), 
         na.rm = T, names = T)

#Diagrama de caja

?boxplot

boxplot(edad)

boxplot(x = edad, 
        main = "Diagrama de cajas", 
        sub = "Edad estudiantes", 
        xlab = "Edad", ylab = "Años",
        col = "lightblue")


#Dataframes

data("iris")

iris

head(iris, 14)
tail(iris)

View(iris)

str(iris)

#Manejo

iris[1, 1]

iris[1, ]

iris[,2]

iris$Species
iris$Sepal.Length

lsep <- iris$Sepal.Length



#Resumen

summary(lsep)

boxplot(lsep)

#Formula y ~ x 

?boxplot

boxplot(formula = iris$Sepal.Length ~ iris$Species)


#Histograma

?hist

hist(lsep)

hist(x = lsep, right = F)
abline(v = mean(lsep), col = "red", lwd = 2)
abline(v = median(lsep), col = "darkgreen", lwd = 2)

install.packages("agricolae")
library(agricolae)

#Sesgo
??Skew
skewness(lsep)
Skew(lsep)


#Kurtosis
?Kurt
kurtosis(lsep)
Kurt(lsep)


#Tabla de frecuencia

?table.freq

table.freq(lsep)
(tab <- table.freq(hist(x = lsep, right = F)))

class(tab)

#Poligono
?plot
plot(x = tab$Main, y = tab$Frequency, type = "b")


#Ojiva

plot(x = tab$Main, y = tab$CPF, type = "b")


#Diagrama de barras

summary(iris$Species)


est <- c(18, 23, 21, 19, 26)
dias <- c("L", "Ma", "Mi", "J", "V")

?barplot

names(est) <- dias

est
barplot(est)


#Dispersion bivariada

cov(iris$Sepal.Length, iris$Petal.Length)

cor(iris$Sepal.Length, iris$Petal.Length)


plot(x = iris$Sepal.Length, y = iris$Petal.Length)

plot(x = iris$Sepal.Length, 
     y = iris$Petal.Length, 
     col = iris$Species)
