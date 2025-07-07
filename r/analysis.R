# Definir los nuevos nombres de las columnas

OPCUA_dataset <- read.csv("~/Escritorio/COMPUTER SCIENCE/2025-pao-i/metodología/pruebas/OPCUA_dataset_public - Original.csv")

# OPCUA_dataset$nota_estadistica <- as.numeric(gsub(",", ".", OPCUA_dataset$nota_estadistica, fixed = TRUE))

library(dplyr)
# OPCUA_dataset <- OPCUA_dataset %>% select(-estudiante)

#-------------------------------------------------------------------------------------------------------------------------

install.packages("skimr")
library(skimr)

skim(OPCUA_dataset)

#-------------------------------------------------------------------------------------------------------------------------

library(ggplot2)
library(dplyr)

# Identificar columnas numéricas
numeric_cols <- names(OPCUA_dataset)[sapply(OPCUA_dataset, is.numeric)]

# Crear directorio si no existe
dir_path <- "~/Escritorio/COMPUTER SCIENCE/2025-pao-i/metodología/pruebas/histogramas"
if(!dir.exists(dir_path)) dir.create(dir_path, recursive = TRUE)

# Generar y guardar histogramas con formato por defecto
for(col in numeric_cols) {
  # Nombre del archivo
  file_name <- paste0(dir_path, "/hist_", col, ".png")
  
  # Abrir dispositivo PNG
  png(file_name)
  
  # Generar histograma con todos los valores por defecto
  hist(OPCUA_dataset[[col]], main = paste("Histogram of", col), xlab = col)
  
  # Cerrar dispositivo
  dev.off()
}

#-------------------------------------------------------------------------------------------------------------------------

summary(OPCUA_dataset)


output_file <- "~/Escritorio/COMPUTER SCIENCE/2025-pao-i/metodología/pruebas/resumen_estadistico.txt"

sink(output_file)
cat("========== RESUMEN ESTADÍSTICO ==========\n\n")
cat("=== SUMMARY ===\n")
print(summary(OPCUA_dataset))
cat("\n\n=== SKIM ===\n")
print(skim(OPCUA_dataset))
sink()

#-------------------------------------------------------------------------------------------------------------------------

library(ggplot2)
library(dplyr)

# Identificar columnas categóricas (factores o caracteres)
categorical_cols <- OPCUA_dataset %>% 
  select(where(is.character)) %>% 
  names()

# Crear directorio para guardar los gráficos si no existe
if(!dir.exists("~/Escritorio/COMPUTER SCIENCE/2025-pao-i/metodología/pruebas/barras")) {
  dir.create("~/Escritorio/COMPUTER SCIENCE/2025-pao-i/metodología/pruebas/barras", recursive = TRUE)
}

# Función para generar gráficos de barras
generar_grafico_barras <- function(data, columna) {
  ggplot(data, aes(x = .data[[columna]], fill = .data[[columna]])) +
    geom_bar(stat = "count", color = "black", width = 0.7) +
    labs(x = columna, 
         y = "Frecuencia Absoluta", 
         title = paste("Distribución de", columna)) +
    scale_fill_viridis_d() +  # Escala de colores automática y accesible
    theme_bw() +
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 45, hjust = 1))
}

# Generar y guardar gráficos para cada columna categórica
for(col in categorical_cols) {
  p <- generar_grafico_barras(OPCUA_dataset, col)
  
  ggsave(filename = paste0("~/Escritorio/COMPUTER SCIENCE/2025-pao-i/metodología/pruebas/barras/bar_", col, ".png"),
         plot = p, 
         width = 8, 
         height = 6, 
         dpi = 300)
}

#-------------------------------------------------------------------------------------------------------------------------

library(ggplot2)
library(dplyr)

# Identificar columnas numéricas
numeric_cols <- OPCUA_dataset %>% select(where(is.numeric)) %>% names()

# Crear directorio para guardar los gráficos
dir_path <- "~/Escritorio/COMPUTER SCIENCE/2025-pao-i/metodología/pruebas/boxplots_horizontales"
if(!dir.exists(dir_path)) dir.create(dir_path, recursive = TRUE)

# Función para generar boxplots horizontales
generar_boxplot_horizontal <- function(data, columna) {
  ggplot(data, aes(x = .data[[columna]])) +
    geom_boxplot(fill = "darkseagreen", color = "black") +
    coord_flip() +  # Esto hace el gráfico horizontal
    labs(title = paste("Boxplot horizontal de", columna),
         x = columna) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank())
}

# Generar y guardar gráficos
for(col in numeric_cols) {
  p <- generar_boxplot_horizontal(OPCUA_dataset, col)
  
  ggsave(filename = paste0(dir_path, "/boxplot_horizontal_", col, ".png"),
         plot = p, 
         width = 8, 
         height = 6, 
         dpi = 300)
}

#----------------------------------------------------GRAFICA DE DISPERSION (DEMORA MUCHO)---------------------------------------------------------------------
library(ggplot2)
library(dplyr)
library(purrr)

# 1. Identificar columnas numéricas
numeric_cols <- OPCUA_dataset %>% 
  select(where(is.numeric)) %>% 
  names()

# 2. Crear directorio para guardar los gráficos
dir_path <- "~/Escritorio/COMPUTER SCIENCE/2025-pao-i/metodología/pruebas/graficos_dispersion"
if(!dir.exists(dir_path)) {
  dir.create(dir_path, recursive = TRUE)
}

# 3. Generar combinaciones únicas de variables
combinaciones <- combn(numeric_cols, 2, simplify = FALSE)

# 4. Función para crear gráficos básicos (sin ggMarginal)
crear_grafico_simple <- function(data, var_x, var_y) {
  ggplot(data, aes(x = .data[[var_x]], y = .data[[var_y]])) +
    geom_point(color = "darkgreen", alpha = 0.6) +
    labs(x = var_x, y = var_y, 
         title = paste(var_y, "vs", var_x)) +
    theme_light() +
    theme(plot.title = element_text(hjust = 0.5))
}

# 5. Generar y guardar todos los gráficos (versión simplificada)
walk(combinaciones, ~{
  var_x <- .x[1]
  var_y <- .x[2]
  
  p <- crear_grafico_simple(OPCUA_dataset, var_x, var_y)
  
  ggsave(
    filename = paste0(dir_path, "/dispersion_", var_y, "_vs_", var_x, ".png"),
    plot = p,
    width = 8,
    height = 6,
    dpi = 300
  )
})
#-------------------------------------------------------------------------------------------------------------------------
library(ggplot2)
library(dplyr)

# 1. Identificar variables
# Variables numéricas (para el eje Y)
numeric_cols <- OPCUA_dataset %>% select(where(is.numeric)) %>% names()
# Variables categóricas (para agrupar/fill)
categorical_cols <- OPCUA_dataset %>% select(where(is.factor) | where(is.character)) %>% names()

# 2. Crear directorio para guardar los gráficos
dir_path <- "~/Escritorio/COMPUTER SCIENCE/2025-pao-i/metodología/pruebas/boxplots_grupos"
if(!dir.exists(dir_path)) {
  dir.create(dir_path, recursive = TRUE)
}

# 3. Función para crear boxplots por grupo
crear_boxplot_grupos <- function(data, num_var, cat_var) {
  ggplot(data, aes(x = .data[[num_var]], y = .data[[cat_var]], fill = .data[[cat_var]])) + 
    geom_boxplot() + 
    stat_boxplot(geom = "errorbar", width = 0.25) +
    labs(x = num_var, 
         y = cat_var, 
         title = paste("Distribución de", num_var, "por", cat_var),
         fill = cat_var) +
    theme_light() +
    theme(legend.position = "none",
          plot.title = element_text(hjust = 0.5))
}

# 4. Generar todas las combinaciones posibles
if(length(numeric_cols) > 0 & length(categorical_cols) > 0) {
  # Generar y guardar todos los gráficos posibles
  for(num_var in numeric_cols) {
    for(cat_var in categorical_cols) {
      p <- crear_boxplot_grupos(OPCUA_dataset, num_var, cat_var)
      
      ggsave(
        filename = paste0(dir_path, "/boxplot_", num_var, "_por_", cat_var, ".png"),
        plot = p,
        width = 8,
        height = 6,
        dpi = 300
      )
    }
  }
} else {
  message("No hay suficientes variables (se necesitan al menos 1 numérica y 1 categórica)")
}

#-------------------------------------------------------------------------------------------------------------------------
library(corrplot)

# 1. Calcular la matriz de correlación
cor_data <- OPCUA_dataset %>%
  select(where(is.numeric)) %>%
  cor(use = "complete.obs")

# 2. Configurar el dispositivo gráfico para guardar como PNG
png(filename = "~/Escritorio/COMPUTER SCIENCE/2025-pao-i/metodología/pruebas/heatmap_correlacion_corrplot.png",
    width = 800, height = 600)

# 3. Generar el gráfico
corrplot(cor_data, method = "color", type = "upper",
         tl.col = "black", tl.srt = 45,
         addCoef.col = "black", number.cex = 0.7,
         title = "Matriz de Correlación",
         mar = c(0,0,2,0))  # Ajustar márgenes (inferior, izquierdo, superior, derecho)

# 4. Cerrar el dispositivo gráfico
dev.off()

#------------------------------------PRUEBAS DE HIPOTESIS (NO CORRER POR DEFINIR)-------------------------------------------------------------------------------------
#MEDIA
# - ASUMIMOS NORMALIDAD Y ESAS WEAS
# H0=6.5
# H1>6.5
estadistica <- OPCUA_dataset$nota_estadistica
(n <- length(estadistica)) # Tamaño de la muestra
(var <- sd(estadistica)) # Varianza Poblacional
mu_0 <- 6.5# Media bajo la Hipótesis Nula
(x_barra <- mean(estadistica))# Media Muestral
alpha <- 0.05

(Z <- (x_barra - mu_0)/sqrt(var/n))# Estadístico de Prueba
(z_alpha <- qnorm(1 - (alpha/2))) # Percentil 1 - 0.03 de Una dist. Normal Estándar
(abs(Z) > z_alpha)

# P(|Z|>z) = P(Z < -z) + P(Z > z) = 2*P(Z > z)
(pvalue <- 2*pnorm(Z,lower.tail = TRUE))
(pvalue < alpha)


# medias entre H y M
men <- OPCUA_dataset[OPCUA_dataset$sexo=="H",]
women <- OPCUA_dataset[OPCUA_dataset$sexo=="M",]

est_men <- men$nota_estadistica
est_women <- women$nota_estadistica

est_women
#install.packages("kableExtra")
library(kableExtra)
#tamaño de las muestras
nA <- length(est_men)
nB <- length(est_women)
#grados de libertad
vA = nA - 1
vB = nB - 1
#medias
mediaA <- mean(est_men)
mediaB <- mean(est_women)
#sumas de cuadrados de diferencias
ssA <- sum((est_men - mean(est_men))^2)
ssB <- sum((est_women - mean(est_women))^2)
#tablas parámetros de las muestras
dA <- data.frame(nA, vA, mediaA, ssA)
dB <- data.frame(nB, vB, mediaB, ssB)
kable(dA, format = "markdown")
kable(dB, format = "markdown")

#varianza agrupada
s2p <- (ssA+ssB)/(vA+vB)
sprintf("s2_p: %g", s2p)

#error estándar de la diferencia entre medias
sxAxB <- sqrt((s2p/nA) + (s2p/nB))
sprintf("s_xA-xB: %g", sxAxB)

#cálculo de t
tcalc <- abs((mediaA-mediaB)/sxAxB)
sprintf("t-calculada: %g", tcalc)

#t para un alfa dado
alfa = 0.05
P1 = 1 - alfa
P2 = 1 - alfa/2
gl = vA + vB
t1 <- qt(P1, gl)
t2 <- qt(P2, gl)
sprintf("t_1cola: %g", t1)
sprintf("t_2colas: %g", t2)
#p-errorI para la t-calculada
Ptcalc1 <- pt(tcalc, gl, lower.tail = FALSE)
sprintf("P_errorI_1cola: %g", Ptcalc1)

sprintf("P_errorI_2cola: %g", Ptcalc1*2)
#hipótesis alterna: droga A - drogaB diferente de 0 (menor o mayor)
pruebat <- t.test(est_men,est_women, var.equal = TRUE, alternative = "two.sided")
pruebat






# medias entre H y M
madrugador <- OPCUA_dataset[OPCUA_dataset$horario_tomado=="07h00 - 09h00",]
diario <- OPCUA_dataset[OPCUA_dataset$horario_tomado=="09h00 - 11h00",]

est_m <- madrugador$nota_estadistica
est_d <- diario$nota_estadistica

#install.packages("kableExtra")
library(kableExtra)
#tamaño de las muestras
nA <- length(est_m)
nB <- length(est_d)
#grados de libertad
vA = nA - 1
vB = nB - 1
#medias
mediaA <- mean(est_m)
mediaB <- mean(est_d)
#sumas de cuadrados de diferencias
ssA <- sum((est_m - mean(est_m))^2)
ssB <- sum((est_d - mean(est_d))^2)
#tablas parámetros de las muestras
dA <- data.frame(nA, vA, mediaA, ssA)
dB <- data.frame(nB, vB, mediaB, ssB)
kable(dA, format = "markdown")
kable(dB, format = "markdown")

#varianza agrupada
s2p <- (ssA+ssB)/(vA+vB)
sprintf("s2_p: %g", s2p)

#error estándar de la diferencia entre medias
sxAxB <- sqrt((s2p/nA) + (s2p/nB))
sprintf("s_xA-xB: %g", sxAxB)

#cálculo de t
tcalc <- abs((mediaA-mediaB)/sxAxB)
sprintf("t-calculada: %g", tcalc)

#t para un alfa dado
alfa = 0.05
P1 = 1 - alfa
P2 = 1 - alfa/2
gl = vA + vB
t1 <- qt(P1, gl)
t2 <- qt(P2, gl)
sprintf("t_1cola: %g", t1)
sprintf("t_2colas: %g", t2)
#p-errorI para la t-calculada
Ptcalc1 <- pt(tcalc, gl, lower.tail = FALSE)
sprintf("P_errorI_1cola: %g", Ptcalc1)

sprintf("P_errorI_2cola: %g", Ptcalc1*2)
#hipótesis alterna: droga A - drogaB diferente de 0 (menor o mayor)
pruebat <- t.test(est_m,est_d, var.equal = TRUE, alternative = "two.sided")
pruebat




#proporcioneZzzzz
# medias entre H y M
# Crear vectores con los éxitos (aprobados) y totales para cada grupo
hombres_aprobados <- sum(men$nota_estadistica >= 6)
total_hombres <- length(men$nota_estadistica)

mujeres_aprobadas <- sum(women$nota_estadistica >= 6)
total_mujeres <- length(women$nota_estadistica)

# Realizar el test de proporciones
resultado_prop <- prop.test(x = c(hombres_aprobados, mujeres_aprobadas),
                            n = c(total_hombres, total_mujeres))
resultado_prop

## prueba de independencia
tabla <- table(OPCUA_dataset$carrera, OPCUA_dataset$sexo) # Elaborar tabla cruzada
chisq.test(tabla)$expected # Ver valores esperados en la tabla cruzada


chi <-chisq.test(tabla) #Guardamos los resultados del test en un objeto
chi

