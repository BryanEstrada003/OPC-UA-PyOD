print(df_new)

head(df_new)

estadistica <- df_new$nota_estadistica

mean(estadistica)
var(estadistica)
sd(estadistica)

##

#Ho: mu_estadistica = 24
#Ha: mu_estadistica != 24


set.seed(91)
mestadistica <- sample(estadistica, size = 50)

mean(mestadistica)

?t.test

t.test(x = mestadistica, 
       alternative = "two.sided", 
       mu = 6, conf.level = 0.98)

##


#Normalidad

hist(estadistica)
hist(mestadistica)

qqnorm(estadistica)
qqline(estadistica)

qqnorm(mestadistica)
qqline(mestadistica)

##

#Prueba Ji cuadrado

?chisq.test
chisq.test(estadistica)
chisq.test(mestadistica)

#Prueba KS

?ks.test

?distributions

ks.test(estadistica, "pnorm", 24, 3.5)
ks.test(mestadistica, "pnorm", 24, 3.5)


#Prueba de 2 medias


dieta <- read.csv("~/datasets/mice_pheno.csv")

head(dieta)

#Probando normalidad

mean(dieta$Bodyweight, na.rm = T)
sd(dieta$Bodyweight, na.rm = T)

qqnorm(dieta$Bodyweight)
qqline(dieta$Bodyweight)

chisq.test(dieta$Bodyweight)

ks.test(dieta$Bodyweight, "pnorm", 28.8, 6.5)

#Revisando los datos

View(dieta)

library(dplyr)

chow <- dieta %>%
  filter(Diet == "chow") %>%
  select(Bodyweight) %>%
  unlist()

hf <- dieta %>%
  filter(Diet == "hf") %>%
  select(Bodyweight) %>%
  unlist()



#Prueba de varianzas
?var.test
var.test(x = hf, y = chow,
         alternative = "two.sided",
         conf.level = 0.9)

#
#Las varianzas no son iguales

#H0: mu_hf == mu_chow
#Ha: mu_hf > mu_chow
t.test(x = hf, y = chow, 
       alternative = "greater",
       mu = 0, 
       paired = F, 
       var.equal = T)


#Prueba pareada

imc <- read.csv("~/datasets/mice_IMC.csv")


head(imc)

#H0: mu_before == mu_after -> mu_diff == 0
#Ha: mu_before < mu_after -> mu_diff < 0

library(dplyr)

imc2 <- imc %>%
  mutate(dif = Before - After)

t.test(x = imc2$dif, 
       alternative = "less", mu = 0, 
       conf.level = .95)

##
t.test(x = imc$Before, y = imc$After, 
       alternative = "less", mu = 0, 
       paired = T,
       conf.level =  .95)


##


#Prueba Independencia

autos <- read.csv("~/datasets/Autos_defectos_planta.csv",
                  stringsAsFactors = T)
head(autos)

table(autos$DEFECTO, autos$PLANTA)

addmargins(table(autos$DEFECTO, autos$PLANTA))

round(prop.table(table(autos$DEFECTO, autos$PLANTA)), 2)
addmargins(round(prop.table(table(autos$DEFECTO, autos$PLANTA)), 2))

#Grafico

library(ggplot2)

ggplot(data = autos, aes(x = DEFECTO, fill = PLANTA)) +
  geom_bar()

ggplot(data = autos, aes(x = PLANTA, fill = DEFECTO)) +
  geom_bar() +
  coord_flip()

#H0: Los defectos son independientes de la planta de produccion
#Ha: No es cierto H0

?chisq.test
chisq.test(x = table(autos$DEFECTO, autos$PLANTA))

?prop.test
