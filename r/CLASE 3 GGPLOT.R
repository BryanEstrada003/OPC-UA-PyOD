
#Librer�a para gr�ficos GGPLOT2

install.packages("ggplot2")
library(ggplot2)

#Librer�a para manejo de datos DPLYR

install.packages("dplyr")
library(dplyr)

#Librer�a de datos GAPMINDER

install.packages("gapminder")
library(gapminder)


#GGPLOT 2

#Estructura de capas

#Datos: Dataframe, tibble, etc.
#Est�tica: Variables(x, y), color, tama�o, forma
#Geometr�a: Histograma, dispersi�n, diagrama de caja
#Extras: Escalas, etiquetas, textos, etc

#Codificaci�n

#ggplot(data = df, mapping = aes(x, y, ...)) +
# geom_XXX(aes()) +
# extras1() +
# extras2() +
# ...
# extran()



# Datos GAPMINDER

data("gapminder")

gapminder

#
#Revisar los datos

head(gapminder)

str(gapminder)

tail(gapminder)


#

#Revisando year

min(gapminder$year)
max(gapminder$year)

unique(gapminder$year)

#

#Revisando continent

unique(gapminder$continent)

summary(gapminder$continent)

#

#Revisando country

unique(gapminder$country)

#

#Gr�fico inicial

ggplot(data = gapminder, 
       mapping = aes(x = year, y = lifeExp)) +
  geom_line()

#


#

#Grafico expectativa de vida y pib per cap

ggplot(data = gapminder,
       mapping = aes(x = gdpPercap, y = lifeExp)) +
  geom_point()

#


#

#Seleccionando datos de 2007

#Sobre dplyr

#filter, select, mutate

#

# La tuber�a (the pipe) %>%

data_2007 <- gapminder %>%
  filter(year == 2007)


ggplot(data = data_2007, 
       mapping = aes(x = gdpPercap, y = lifeExp)) +
  geom_point()


#

#Cambiar una variable y agregar otra
#Aplicando logaritmo

dflog_2007 <- data_2007 %>%
  mutate(log_gdppc = log10(gdpPercap))


ggplot(data = dflog_2007, 
       mapping = aes(x = log_gdppc, y = lifeExp)) +
  geom_point()


#Aplicando log directamente sobre gr�fico

ggplot(data = data_2007,
       mapping = aes(x = gdpPercap, y = lifeExp)) +   
  geom_point() +  
  scale_x_log10()

#Aplicando color por continente


ggplot(data = data_2007, 
       mapping = aes(x = gdpPercap,
                     y = lifeExp, 
                     color = continent)) +   
  geom_point() +  
  scale_x_log10()


#Aplicando tama�o por poblaci�n

ggplot(data = data_2007, 
       mapping = aes(x = gdpPercap, y = lifeExp, 
                     color = continent, size = pop)) +   
  geom_point() +  
  scale_x_log10()


#Aplicando etiquetas

ggplot(data = gapminder %>%
         filter(year == 2007), 
       mapping = aes(x = gdpPercap, y = lifeExp, 
                     color = continent, size = pop)) +   
  geom_point() +  
  scale_x_log10() +
  labs(title = "Expectativa de vida seg�n PIB per c�pita",
       subtitle = "A�o 2007", 
       caption = "Fuente: Gapminder",
       x = "PIB per c�pita \n ($/hab)",
       y = "Expectativa de vida  \n (a�os)",
       size = "Poblaci�n",
       color = "Continente")



#Todos los datos todos


ggplot(data = gapminder, 
       mapping = aes(x = gdpPercap, y = lifeExp, 
                     color = continent, size = pop)) +   
  geom_point() +  
  scale_x_log10() +
  labs(title = "Expectativa de vida seg�n PIB per c�pita",
       subtitle = "1952 - 2007", 
       caption = "Fuente: Gapminder",
       x = "PIB per c�pita \n ($/hab)",
       y = "Expectativa de vida  \n (a�os)",
       size = "Poblaci�n",
       color = "Continente")


#Mosaico

ggplot(data = gapminder, 
       mapping = aes(x = gdpPercap, y = lifeExp, 
                     color = continent, size = pop)) +   
  geom_point() +  
  scale_x_log10() +
  labs(title = "Expectativa de vida seg�n PIB per c�pita",
       subtitle = "1952 - 2007", 
       caption = "Fuente: Gapminder",
       x = "PIB per c�pita \n ($/hab)",
       y = "Expectativa de vida  \n (a�os)",
       size = "Poblaci�n",
       color = "Continente") +
  facet_wrap(~year)



#Comparando esperanza de vida medianas

data_2007 %>%
  group_by(continent) %>%
  summarise(Vida_Mediana = median(lifeExp),
            Vida_Media = mean(lifeExp))


#Boxplot

ggplot(data_2007, aes(continent, lifeExp)) +
  geom_boxplot()


#Gr�fico de l�nea

ggplot(gapminder, aes(year, lifeExp)) +
  geom_line()


gapminder %>%
  group_by(continent, year) %>%
  summarise(Vida_Mediana = median(lifeExp))

ggplot(gapminder %>%
         group_by(continent, year) %>%
         summarise(Vida_Mediana = median(lifeExp)), 
       aes(year, Vida_Mediana, color = continent)) +
  geom_line(size = 1.2, linetype = 1)



#Histograma

#N�mero de intervalos (bins)
ggplot(gapminder, aes(lifeExp)) +
  geom_histogram(bins = 10)

#Con un ancho definido

ggplot(gapminder, aes(lifeExp)) +
    geom_histogram(binwidth = 10)


ggplot(data_2007, aes(lifeExp)) +
  geom_histogram( bins = 10)


#Dividir continente

ggplot(data_2007, aes(lifeExp, fill = continent)) +
  geom_histogram( bins = 10)

ggplot(data_2007, aes(lifeExp, fill = continent)) +
  geom_histogram( bins = 10) +
  facet_wrap(~continent)

#Densidad - Pol�gono de Frecuencia

ggplot(data_2007, aes(lifeExp)) +
  geom_density()

ggplot(data_2007, aes(lifeExp, fill = continent)) +
  geom_density()


#Cambiando la transparencia

ggplot(data_2007, aes(lifeExp, fill = continent)) +
  geom_density(alpha = 0.6)

ggplot(data_2007, aes(lifeExp, fill = continent)) +
  geom_density(alpha = 0.6) +
  facet_wrap(~continent)


#Pol�gono de frecuencias

ggplot(data_2007, aes(lifeExp)) +
  geom_freqpoly(bins = 10)


ggplot(data_2007, aes(lifeExp, color = continent)) +
  geom_freqpoly()


ggplot(data_2007, aes(lifeExp, color = continent)) +
  geom_freqpoly(bins = 10) +
  facet_wrap(~continent)


#Carga de archivos externos

#Lectura de csv

?read.csv
who <- read.csv("~/datasets/who_disease.csv")

str(who)

head(who)

#Revisando year

min(who$year)
max(who$year)

unique(who$year)

#Revisando region

unique(who$region)

table(who$region)


#Revisando disease

unique(who$disease)


who_2016 <- who %>%
  filter(year == 2016)

who_2016_d <- who_2016 %>%
  group_by(region, disease) %>%
  summarise(total = sum(cases))

who_2016_d
ggplot(who_2016_d, aes(disease, total)) +
  geom_col()

ggplot(who_2016_d, aes(disease, total, fill = region)) +
  geom_col()

ggplot(who_2016_d, aes(region, total, fill = disease)) +
  geom_col()



# %in%

pc_d <- c("yfever", "rubella", "polio", "diphtheria")


who_2016_d$disease %in% pc_d

who_2016_d %>%
  mutate(disease2 = ifelse(disease %in% pc_d, "others", disease))

ggplot(who_2016_d %>%
         mutate(disease2 = ifelse(disease %in% pc_d, "others", disease)),
       aes(region, total, fill = disease2)) +
  geom_col()

#Ajuste

ggplot(who_2016_d %>%
         mutate(disease2 = ifelse(disease %in% pc_d, "others", disease)),
       aes(region, total, fill = disease2)) +
  geom_col(position = "dodge")


ggplot(who_2016_d %>%
         mutate(disease2 = ifelse(disease %in% pc_d, "others", disease)),
       aes(region, total, fill = disease2)) +
  geom_col(position = "fill")

#Agregando temas

ggplot(who_2016_d %>%
         mutate(disease2 = ifelse(disease %in% pc_d, "others", disease)),
       aes(region, total, fill = disease2)) +
  geom_col(position = "dodge") +
  coord_flip() +
  theme_classic()



