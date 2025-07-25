# ===============================================================================
# SCRIPT DE PREPROCESAMIENTO PARA DATASET OPC UA
# Análisis de Ciberseguridad en Sistemas Industriales
# ===============================================================================

# Limpiar entorno
rm(list = ls())
gc()

# ===============================================================================
# 1. INSTALACIÓN Y CARGA DE LIBRERÍAS
# ===============================================================================
# Lista de paquetes necesarios con alternativas compatibles
packages <- c(
  "Matrix", "dplyr", "ggplot2", "corrplot", "skimr", "naniar", "mice",
  "caret", "ROSE", "DMwR2", "smotefamily", 
  "gridExtra", "reshape2", "RColorBrewer", "viridis", "plotly",
  "kableExtra", "DT", "psych", "Hmisc", "pheatmap",
  "randomForest", "e1071", "ROCR", "pROC", "MLmetrics"
)

# Función mejorada para instalar paquetes
install_if_missing <- function(packages) {
  # Instalar remotes si no está disponible para instalaciones desde GitHub
  if (!require("remotes", character.only = TRUE)) {
    install.packages("remotes")
  }
  
  # Instalar Matrix primero por ser dependencia crítica
  if (!require("Matrix", character.only = TRUE)) {
    install.packages("Matrix", dependencies = TRUE)
  }
  
  # Verificar e instalar otros paquetes
  new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
  
  if(length(new_packages)) {
    # Instalar con dependencias básicas para evitar conflictos
    install.packages(new_packages, 
                     dependencies = c("Depends", "Imports", "LinkingTo"))
  }
  
  # Instalar versiones específicas de paquetes problemáticos
  if (!require("GGally", character.only = TRUE)) {
    try({
      remotes::install_version("GGally", version = "2.1.2")  # Última versión compatible con R 4.2
      packages <- c(packages, "GGally")  # Añadir a la lista si se instala
    }, silent = TRUE)
  }
}

# Instalar paquetes faltantes con manejo de errores
try({
  install_if_missing(packages)
}, silent = TRUE)

# Función segura para cargar librerías
safe_load <- function(pkg) {
  suppressMessages({
    if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
      warning(paste("Paquete", pkg, "no pudo ser cargado"), call. = FALSE)
      return(FALSE)
    }
    return(TRUE)
  })
}

# Cargar librerías con manejo de errores
loaded <- sapply(packages, safe_load)

# Mostrar resumen de instalación
message("\n=== Resumen de instalación ===")
message("Paquetes instalados correctamente: ", sum(loaded), "/", length(packages))
if (any(!loaded)) {
  message("Paquetes no cargados:")
  message(paste(names(loaded)[!loaded], collapse = ", "))
}

# Intento alternativo para VIM (si es absolutamente necesario)
if ("VIM" %in% packages && !"VIM" %in% installed.packages()[,"Package"]) {
  message("\nIntentando instalar VIM desde GitHub...")
  try({
    remotes::install_github("statistikat/VIM")
    if (require("VIM")) {
      message("VIM instalado correctamente desde GitHub")
      loaded["VIM"] <- TRUE
    }
  }, silent = TRUE)
}

# Verificar paquetes esenciales
essential <- c("dplyr", "ggplot2", "caret", "mice")
missing_essential <- essential[!essential %in% installed.packages()[,"Package"]]
if (length(missing_essential) > 0) {
  warning("\n¡Atención! Paquetes esenciales faltantes: ", 
          paste(missing_essential, collapse = ", "))
} else {
  message("\nTodos los paquetes esenciales están instalados")
}

# Recomendación final
if (any(!loaded)) {
  message("\nRecomendaciones:")
  if ("GGally" %in% names(loaded)[!loaded]) {
    message("- Para visualización: Usa pairs() de base R o ggplot2 + patchwork")
  }
  if ("VIM" %in% names(loaded)[!loaded]) {
    message("- Para datos faltantes: Usa naniar::gg_miss_upset() en lugar de VIM")
  }
}
# ===============================================================================
# 2. CONFIGURACIÓN Y CARGA DE DATOS
# ===============================================================================

# Configurar directorio de trabajo usando variable
directorio_trabajo <- "~/mai/OPC-UA-PyOD"

# Establecer directorio de trabajo
setwd(directorio_trabajo)

# Cargar dataset concatenando la ruta
cat("Cargando dataset OPC UA...\n")
ruta_dataset <- file.path(directorio_trabajo, "dataset", "OPCUA_dataset_public - Original.csv")
opcua_data <- read.csv(ruta_dataset, stringsAsFactors = FALSE)

# Verificar carga
cat("Dataset cargado correctamente. Dimensiones:", dim(opcua_data), "\n")

# Crear directorio para resultados
output_dir <- file.path(directorio_trabajo, "results")
if(!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Subdirectorios para organizar resultados
dirs <- c("plots", "tables", "processed_data", "models")
for(dir in dirs) {
  if(!dir.exists(file.path(output_dir, dir))) {
    dir.create(file.path(output_dir, dir), recursive = TRUE)
  }
}

# ===============================================================================
# 3. ANÁLISIS EXPLORATORIO INICIAL
# ===============================================================================

cat("Realizando análisis exploratorio inicial...\n")

# Información básica del dataset
cat("\n=== INFORMACIÓN BÁSICA DEL DATASET ===\n")
cat("Dimensiones:", dim(opcua_data), "\n")
cat("Columnas:", ncol(opcua_data), "\n")
cat("Filas:", nrow(opcua_data), "\n")

# Estructura del dataset
str(opcua_data)

# Resumen estadístico inicial
summary_stats <- skim(opcua_data)
print(summary_stats)

# Guardar resumen estadístico
write.csv(summary_stats, file.path(output_dir, "tables", "summary_statistics.csv"))

# ===============================================================================
# 4. ANÁLISIS DE VARIABLES CATEGÓRICAS Y ETIQUETAS
# ===============================================================================

cat("\n=== ANÁLISIS DE VARIABLES CATEGÓRICAS ===\n")

# Identificar variables categóricas automáticamente
categorical_vars <- names(opcua_data)[sapply(opcua_data, function(x) {
  is.factor(x) | is.character(x) | (is.logical(x) & n_distinct(x) <= 10)
})]
if(!("label" %in% categorical_vars)) categorical_vars <- c(categorical_vars, "label")
# Mostrar las variables categóricas identificadas
cat("\nVariables categóricas identificadas:\n")
print(categorical_vars)

# Análisis de la variable objetivo (label)
cat("\nDistribución de la variable objetivo 'label':\n")
label_dist <- table(opcua_data$label)
print(label_dist)
print(prop.table(label_dist))

# Análisis de multi_label
cat("\nDistribución de 'multi_label':\n")
multi_label_dist <- table(opcua_data$multi_label)
print(multi_label_dist)
print(prop.table(multi_label_dist))

# Visualización de distribución de clases
p1 <- ggplot(opcua_data, aes(x = label, fill = label)) +
  geom_bar(stat = "count", color = "black") +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +
  labs(title = "Distribución de Clases (Label)", 
       x = "Clase", y = "Frecuencia") +
  theme_minimal() +
  scale_fill_viridis_d()

p2 <- ggplot(opcua_data, aes(x = multi_label, fill = multi_label)) +
  geom_bar(stat = "count", color = "black") +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5, size = 3) +
  labs(title = "Distribución de Multi-Clases", 
       x = "Tipo de Ataque", y = "Frecuencia") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_viridis_d()

# Guardar gráficos
ggsave(file.path(output_dir, "plots", "class_distribution.png"), 
       grid.arrange(p1, p2, ncol = 2), width = 12, height = 6, dpi = 300)

# ===============================================================================
# 5. ANÁLISIS DE VALORES FALTANTES
# ===============================================================================

cat("\n=== ANÁLISIS DE VALORES FALTANTES ===\n")

# Asegurarse que los paquetes necesarios están cargados
if (!require("dplyr") | !require("tidyr")) {
  stop("Los paquetes dplyr y tidyr son requeridos")
}

# Contar valores faltantes por columna (versión actualizada)
missing_values <- opcua_data %>%
  summarise(across(everything(), ~sum(is.na(.)))) %>%
  pivot_longer(cols = everything(), 
               names_to = "Variable", 
               values_to = "Missing_Count") %>%
  mutate(Missing_Percentage = (Missing_Count / nrow(opcua_data)) * 100) %>%
  arrange(desc(Missing_Count))

print(missing_values)

# Visualización de valores faltantes (versión con naniar como alternativa a VIM)
if(sum(missing_values$Missing_Count) > 0) {
  # Crear directorio para plots si no existe
  if (!dir.exists(file.path(output_dir, "plots"))) {
    dir.create(file.path(output_dir, "plots"), recursive = TRUE)
  }
  
  # Versión con naniar (alternativa moderna a VIM)
  if (require("naniar")) {
    png(file.path(output_dir, "plots", "missing_values_pattern.png"), 
        width = 1200, height = 800, res = 150)
    print(naniar::gg_miss_var(opcua_data, show_pct = TRUE) +
            theme_minimal() +
            scale_fill_manual(values = c('navyblue', 'red')))
    dev.off()
    cat("Gráfico de valores faltantes guardado en:", 
        file.path(output_dir, "plots", "missing_values_pattern.png"), "\n")
  } else {
    warning("Paquete naniar no disponible para visualización")
  }
} else {
  cat("No se encontraron valores faltantes en el dataset.\n")
}

# ===============================================================================
# 6. ANÁLISIS DE VARIABLES NUMÉRICAS
# ===============================================================================

cat("\n=== ANÁLISIS DE VARIABLES NUMÉRICAS ===\n")

# 1. Cargar paquetes requeridos --------------------------------------------
if (!require("dplyr")) install.packages("dplyr")
if (!require("tidyr")) install.packages("tidyr")
library(dplyr)
library(tidyr)

# 2. Identificar variables numéricas --------------------------------------
numeric_vars <- opcua_data %>%
  select(-all_of(categorical_vars)) %>%
  select(where(is.numeric)) %>%
  names()

cat("Variables numéricas identificadas:", length(numeric_vars), "\n")
print(numeric_vars)

# 3. Análisis con psych si está disponible ---------------------------------
if(require("psych", quietly = TRUE)){
  cat("\nCalculando estadísticas con psych::describe()\n")
  
  # Calcular estadísticas
  numeric_summary <- opcua_data %>%
    select(all_of(numeric_vars)) %>%
    psych::describe() 
  
  # Convertir a dataframe y limpiar nombres
  numeric_summary <- as.data.frame(numeric_summary) %>%
    mutate(Variable = rownames(.)) %>%
    select(Variable, n = n, mean, sd, min, max, median, skew, kurtosis) %>%
    mutate(across(where(is.numeric), ~round(., 4)))
  
  # Guardar resultados
  write.csv(numeric_summary, 
            file.path(output_dir, "tables", "numeric_summary_psych.csv"),
            row.names = FALSE)
  
  print(numeric_summary)
  
} else {
  cat("\nEl paquete psych no está disponible\n")
}

# 4. Análisis con tidyverse (alternativa principal) ------------------------
cat("\nCalculando estadísticas con dplyr/tidyr\n")

numeric_summary <- opcua_data %>%
  select(all_of(numeric_vars)) %>%
  summarise(across(
    everything(),
    list(
      mean = ~mean(., na.rm = TRUE),
      sd = ~sd(., na.rm = TRUE),
      min = ~min(., na.rm = TRUE),
      q25 = ~quantile(., 0.25, na.rm = TRUE),
      median = ~median(., na.rm = TRUE),
      q75 = ~quantile(., 0.75, na.rm = TRUE),
      max = ~max(., na.rm = TRUE),
      na = ~sum(is.na(.))
    )
  )) %>%
  pivot_longer(
    everything(),
    names_to = c("Variable", ".value"),
    names_sep = "_"
  ) %>%
  mutate(across(where(is.numeric), ~round(., 4)))

# Guardar resultados
write.csv(numeric_summary,
          file.path(output_dir, "tables", "numeric_summary.csv"),
          row.names = FALSE)

print(numeric_summary)

# ===============================================================================
# 7. DETECCIÓN DE OUTLIERS
# ===============================================================================

cat("\n=== DETECCIÓN DE OUTLIERS ===\n")

# Función para detectar outliers usando IQR
detect_outliers_iqr <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  return(x < lower_bound | x > upper_bound)
}

# Detectar outliers para cada variable numérica
outlier_summary <- opcua_data %>%
  select(all_of(numeric_vars)) %>%
  summarise_all(~sum(detect_outliers_iqr(.), na.rm = TRUE)) %>%
  gather(key = "Variable", value = "Outlier_Count") %>%
  mutate(Outlier_Percentage = (Outlier_Count / nrow(opcua_data)) * 100) %>%
  arrange(desc(Outlier_Count))

print(outlier_summary)

# Visualización de outliers con boxplots
create_boxplots <- function(data, vars, ncol = 3) {
  plots <- list()
  for(i in seq_along(vars)) {
    var <- vars[i]
    plots[[i]] <- ggplot(data, aes(y = .data[[var]])) +
      geom_boxplot(fill = "lightblue", alpha = 0.7) +
      labs(title = paste("Boxplot:", var), y = var) +
      theme_minimal() +
      theme(axis.text.x = element_blank(),
            axis.ticks.x = element_blank())
  }
  return(plots)
}

# Crear boxplots para las primeras 12 variables numéricas
boxplot_vars <- head(numeric_vars, 12)
boxplots <- create_boxplots(opcua_data, boxplot_vars)

# Guardar boxplots
ggsave(file.path(output_dir, "plots", "outliers_boxplots.png"),
       do.call(grid.arrange, c(boxplots, ncol = 3)),
       width = 15, height = 12, dpi = 300)

# ===============================================================================
# 8. ANÁLISIS DE CORRELACIONES
# ===============================================================================

cat("\n=== ANÁLISIS DE CORRELACIONES ===\n")

# Calcular matriz de correlación con manejo de NA
cor_matrix <- opcua_data %>%
  select(all_of(numeric_vars)) %>%
  cor(use = "complete.obs")  # 'complete.obs' usa solo observaciones completas

# Verificar y limpiar la matriz de correlación
if(any(is.na(cor_matrix))) {
  cat("\nAdvertencia: La matriz de correlación contiene NA. Revisar variables constantes.\n")
  # Reemplazar NA por 0 (solo para visualización)
  cor_matrix[is.na(cor_matrix)] <- 0
}

# Encontrar correlaciones altas (> 0.8)
high_cor <- which(abs(cor_matrix) > 0.8 & abs(cor_matrix) < 1, arr.ind = TRUE)
high_cor_pairs <- data.frame(
  Var1 = rownames(cor_matrix)[high_cor[,1]],
  Var2 = colnames(cor_matrix)[high_cor[,2]],
  Correlation = cor_matrix[high_cor]
) %>%
  arrange(desc(abs(Correlation)))

cat("\nPares de variables con correlación alta (>0.8):\n")
print(head(high_cor_pairs, 20))

# Visualización de matriz de correlación básica
png(file.path(output_dir, "plots", "correlation_matrix.png"),
    width = 1200, height = 1000, res = 150)
corrplot(cor_matrix, method = "color", type = "upper",
         tl.col = "black", tl.srt = 45, tl.cex = 0.8,
         title = "Matriz de Correlación - Variables Numéricas",
         mar = c(0,0,2,0))
dev.off()

# Heatmap detallado con pheatmap - Versión robusta
heatmap_file <- file.path(output_dir, "plots", "correlation_heatmap_detailed.png")

tryCatch({
  # Asegurarnos de que la matriz no tenga NA/Inf
  clean_cor_matrix <- cor_matrix
  clean_cor_matrix[is.na(clean_cor_matrix) | is.infinite(clean_cor_matrix)] <- 0
  
  # Configurar parámetros del gráfico
  pheatmap_plot <- pheatmap(clean_cor_matrix,
                            color = colorRampPalette(c("#053061", "white", "#67001F"))(100),
                            main = "Matriz de Correlación Detallada",
                            fontsize_row = 8,
                            fontsize_col = 8,
                            angle_col = 45,
                            display_numbers = ncol(clean_cor_matrix) <= 15,  # Mostrar números solo si hay pocas variables
                            number_format = "%.2f",
                            number_color = "black",
                            cluster_rows = nrow(clean_cor_matrix) > 1,
                            cluster_cols = ncol(clean_cor_matrix) > 1,
                            silent = TRUE)  # No mostrar en la consola
  
  # Guardar con control de errores
  png(heatmap_file, width = 1400, height = 1200, res = 150)
  grid::grid.newpage()
  grid::grid.draw(pheatmap_plot$gtable)
  dev.off()
  
  cat("\nHeatmap de correlación guardado correctamente en:", heatmap_file, "\n")
  
}, error = function(e) {
  if(dev.cur() > 1) dev.off()  # Cerrar dispositivo gráfico si está abierto
  cat("\nError al generar el heatmap:", e$message, "\n")
  
  # Intentar guardar una versión simplificada como respaldo
  try({
    png(heatmap_file, width = 1400, height = 1200, res = 150)
    corrplot(cor_matrix, method = "color", type = "full",
             tl.col = "black", tl.srt = 45, tl.cex = 0.8,
             title = "Matriz de Correlación (versión simplificada)")
    dev.off()
    cat("Se generó una versión simplificada del heatmap.\n")
  }, silent = TRUE)
})

# ===============================================================================
# 9. PREPROCESAMIENTO DE DATOS
# ===============================================================================

cat("\n=== INICIANDO PREPROCESAMIENTO ===\n")

# Crear copia para preprocesamiento
opcua_processed <- opcua_data

# 9.1 Conversión de tipos de datos
cat("Convirtiendo tipos de datos...\n")

# Convertir variables categóricas a factor
opcua_processed$proto <- as.factor(opcua_processed$proto)
opcua_processed$service <- as.factor(opcua_processed$service)
opcua_processed$label <- as.factor(opcua_processed$label)
opcua_processed$multi_label <- as.factor(opcua_processed$multi_label)

# 9.2 Tratamiento de IPs (crear variables categóricas simplificadas)
cat("Procesando direcciones IP...\n")

# Extraer subred de las IPs (primeros 3 octetos)
opcua_processed$src_subnet <- sapply(strsplit(opcua_processed$src_ip, "\\."), 
                                   function(x) paste(x[1:3], collapse = "."))
opcua_processed$dst_subnet <- sapply(strsplit(opcua_processed$dst_ip, "\\."), 
                                   function(x) paste(x[1:3], collapse = "."))

# Convertir a factor
opcua_processed$src_subnet <- as.factor(opcua_processed$src_subnet)
opcua_processed$dst_subnet <- as.factor(opcua_processed$dst_subnet)

# 9.3 Tratamiento de outliers
cat("Tratando outliers...\n")

# Función para winsorizar outliers (reemplazar por percentiles 5 y 95)
winsorize <- function(x, probs = c(0.05, 0.95)) {
  quantiles <- quantile(x, probs = probs, na.rm = TRUE)
  x[x < quantiles[1]] <- quantiles[1]
  x[x > quantiles[2]] <- quantiles[2]
  return(x)
}

# Aplicar winsorización a variables con muchos outliers
high_outlier_vars <- outlier_summary %>%
  filter(Outlier_Percentage > 10) %>%
  pull(Variable)

for(var in high_outlier_vars) {
  opcua_processed[[var]] <- winsorize(opcua_processed[[var]])
}

# 9.4 Normalización de variables numéricas
cat("Normalizando variables numéricas...\n")

# Identificar variables numéricas para normalizar (excluyendo timestamps y flags)
vars_to_normalize <- numeric_vars[!grepl("flow|Start|End|flags", numeric_vars)]

# Crear objeto de preprocesamiento
preprocess_params <- preProcess(opcua_processed[vars_to_normalize], 
                               method = c("center", "scale"))

# Aplicar normalización
opcua_processed[vars_to_normalize] <- predict(preprocess_params, 
                                            opcua_processed[vars_to_normalize])

# ===============================================================================
# 10. ANÁLISIS DE BALANCEO DE CLASES
# ===============================================================================

cat("\n=== ANÁLISIS DE BALANCEO DE CLASES ===\n")

# Análisis detallado del desbalance
class_analysis <- opcua_processed %>%
  group_by(label, multi_label) %>%
  summarise(count = n(), .groups = 'drop') %>%
  mutate(percentage = round(count / sum(count) * 100, 2)) %>%
  arrange(desc(count))

print(class_analysis)

# Guardar análisis de clases
write.csv(class_analysis, file.path(output_dir, "tables", "class_analysis.csv"))

# ===============================================================================
# 11. TÉCNICAS DE BALANCEO DE CLASES
# ===============================================================================

cat("\n=== APLICANDO TÉCNICAS DE BALANCEO ===\n")

# Preparar datos para balanceo (solo variables numéricas + label)
balance_data <- opcua_processed %>%
  select(all_of(vars_to_normalize), label)

# 11.1 SMOTE (Synthetic Minority Oversampling Technique)
cat("Aplicando SMOTE...\n")

if(require("smotefamily")){
  set.seed(123)
  # Guardar los niveles originales del factor
  original_levels <- levels(balance_data$label)
  
  # SMOTE requiere que todas las variables sean numéricas
  smote_data_prep <- balance_data
  smote_data_prep$label <- as.integer(smote_data_prep$label) - 1  # Convertir a 0-based index
  
  # Verificar que hay al menos 2 instancias por clase
  table_labels <- table(smote_data_prep$label)
  if(any(table_labels < 2)){
    cat("\nAdvertencia: Algunas clases tienen menos de 2 instancias. SMOTE puede fallar.\n")
    cat("Distribución actual:\n")
    print(table(balance_data$label))
  }
  
  # Generar data sintética para clases minoritarias
  smote_result <- tryCatch({
    smotefamily::SMOTE(
      X = smote_data_prep[, -which(names(smote_data_prep) == "label")],
      target = smote_data_prep$label,
      K = min(3, nrow(smote_data_prep) - 1),  # Asegurar que K no sea mayor que el número de instancias
      dup_size = 1
    )$data
  }, error = function(e) {
    cat("\nError al aplicar SMOTE:", e$message, "\n")
    return(NULL)
  })
  
  if(!is.null(smote_result)) {
    # Renombrar y convertir la columna de clase
    names(smote_result)[names(smote_result) == "class"] <- "label"
    
    # Convertir de nuevo a factor usando los niveles originales
    smote_result$label <- factor(smote_result$label, 
                                 levels = 0:(length(original_levels)-1),
                                 labels = original_levels)
    
    cat("\nDistribución después de SMOTE:\n")
    print(table(smote_result$label))
    
    # Combinar con los datos originales
    smote_result <- rbind(
      balance_data,
      smote_result %>% select(names(balance_data))
    )
  } else {
    cat("\nNo se pudo aplicar SMOTE. Usando datos originales.\n")
    smote_result <- balance_data
  }
} else {
  cat("Paquete smotefamily no disponible para SMOTE multiclase.\n")
  smote_result <- balance_data
}
# 11.2 ROSE (Random Over-Sampling Examples)
cat("Aplicando ROSE...\n")

set.seed(123)
# ROSE puede manejar directamente el desbalance multiclase
rose_result <- ROSE(label ~ ., data = balance_data, seed = 123)$data

cat("Distribución después de ROSE:\n")
print(table(rose_result$label))

# 11.3 Undersampling
cat("Aplicando Undersampling...\n")

# Encontrar el tamaño de la segunda clase más pequeña para no eliminar demasiados datos
second_smallest_class_size <- sort(table(balance_data$label))[2]

set.seed(123)
undersample_result <- balance_data %>%
  group_by(label) %>%
  sample_n(min(n(), second_smallest_class_size)) %>% # No reducir más allá de la 2da clase más pequeña
  ungroup()

cat("Distribución después de Undersampling:\n")
print(table(undersample_result$label))

# ===============================================================================
# 12. SELECCIÓN DE CARACTERÍSTICAS
# ===============================================================================

cat("\n=== SELECCIÓN DE CARACTERÍSTICAS ===\n")

# 1. Eliminar variables altamente correlacionadas -------------------------

# Verificar y limpiar la matriz de correlación primero
cor_matrix <- cor(opcua_processed[vars_to_normalize], use = "complete.obs")

# Manejar posibles NA/NaN en la matriz de correlación
if(any(is.na(cor_matrix))) {
  cor_matrix[is.na(cor_matrix)] <- 0  # Reemplazar NA con 0 para el cálculo
  warning("Se encontraron NA en la matriz de correlación. Se reemplazaron por 0 para el cálculo.")
}

# Encontrar variables correlacionadas con manejo de errores
high_cor_vars <- tryCatch({
  caret::findCorrelation(cor_matrix, cutoff = 0.9, names = TRUE)
}, error = function(e) {
  cat("Error en findCorrelation:", e$message, "\n")
  character(0)  # Devolver vector vacío si hay error
})

if(length(high_cor_vars) > 0) {
  cat("Variables eliminadas por alta correlación:", paste(high_cor_vars, collapse = ", "), "\n")
  
  # Actualizar lista de variables
  vars_to_normalize <- setdiff(vars_to_normalize, high_cor_vars)
} else {
  cat("No se encontraron variables con correlación > 0.9\n")
}

# 2. Selección basada en importancia con Random Forest --------------------

# Verificar que hay variables para analizar
if(length(vars_to_normalize) == 0) {
  stop("No quedan variables para analizar después de eliminar las correlacionadas")
}

cat("Calculando importancia de variables con Random Forest...\n")

# Crear fórmula dinámica
rf_formula <- as.formula(paste("label ~", paste(vars_to_normalize, collapse = " + ")))

set.seed(123)
rf_model <- tryCatch({
  randomForest(rf_formula, 
               data = balance_data[c(vars_to_normalize, "label")], 
               importance = TRUE, 
               ntree = 100)
}, error = function(e) {
  cat("Error al entrenar Random Forest:", e$message, "\n")
  NULL
})

if(!is.null(rf_model)) {
  # Obtener importancia de variables
  var_importance <- randomForest::importance(rf_model)
  
  # Convertir a dataframe ordenado
  var_importance_df <- data.frame(
    Variable = rownames(var_importance),
    MeanDecreaseAccuracy = var_importance[, "MeanDecreaseAccuracy"],
    MeanDecreaseGini = var_importance[, "MeanDecreaseGini"]
  ) %>%
    arrange(desc(MeanDecreaseAccuracy))
  
  print(head(var_importance_df, 15))
  
  # Visualizar importancia de variables
  p_importance <- ggplot(head(var_importance_df, 15), 
                         aes(x = reorder(Variable, MeanDecreaseAccuracy), 
                             y = MeanDecreaseAccuracy)) +
    geom_col(fill = "steelblue", alpha = 0.7) +
    coord_flip() +
    labs(title = "Importancia de Variables (Random Forest)",
         x = "Variables", y = "Mean Decrease Accuracy") +
    theme_minimal()
  
  ggsave(file.path(output_dir, "plots", "variable_importance.png"), 
         p_importance, width = 10, height = 8, dpi = 300)
  
  # Guardar resultados de importancia
  write.csv(var_importance_df,
            file.path(output_dir, "tables", "variable_importance.csv"),
            row.names = FALSE)
  
} else {
  warning("No se pudo calcular la importancia de variables debido a errores en el modelo")
}

cat("\nSelección de características completada\n")

# ===============================================================================
# 13. GUARDAR DATASETS PROCESADOS
# ===============================================================================

cat("\n=== GUARDANDO DATASETS PROCESADOS ===\n")

# Dataset original procesado (normalizado, sin balancear)
write.csv(opcua_processed, 
          file.path(output_dir, "processed_data", "opcua_processed_original.csv"), 
          row.names = FALSE)

# Dataset balanceado con SMOTE
write.csv(smote_result, 
         file.path(output_dir, "processed_data", "opcua_smote_balanced.csv"), 
         row.names = FALSE)
# Dataset balanceado con ROSE
write.csv(rose_result, 
         file.path(output_dir, "processed_data", "opcua_rose_balanced.csv"), 
         row.names = FALSE)

# Dataset con undersampling
write.csv(undersample_result, 
         file.path(output_dir, "processed_data", "opcua_undersampled.csv"), 
         row.names = FALSE)

# Guardar parámetros de preprocesamiento
saveRDS(preprocess_params, 
        file.path(output_dir, "processed_data", "preprocessing_parameters.rds"))

# ===============================================================================
# 14. EVALUACIÓN COMPARATIVA DE DATASETS
# ===============================================================================

cat("\n=== EVALUACIÓN COMPARATIVA ===\n")

# Función mejorada para evaluar datasets
evaluate_dataset_cv <- function(data, dataset_name, k = 5) {
  if (is.null(data) || nrow(data) == 0) {
    cat("Dataset", dataset_name, "está vacío, saltando evaluación.\n")
    return(NULL)
  }
  
  cat("Evaluando dataset:", dataset_name, "con validación cruzada de", k, "folds...\n")
  
  set.seed(123)
  # Crear folds estratificados
  folds <- caret::createFolds(data$label, k = k, list = TRUE, returnTrain = TRUE)
  
  # Almacenar métricas
  metrics <- list(
    Accuracy = numeric(k),
    Balanced_Accuracy = numeric(k),
    Weighted_F1 = numeric(k),
    Macro_F1 = numeric(k)
  )
  
  for (i in 1:k) {
    train_indices <- folds[[i]]
    test_indices <- setdiff(1:nrow(data), train_indices)
    
    train_data <- data[train_indices, ]
    test_data <- data[test_indices, ]
    
    # Asegurar que los niveles de los factores coincidan
    train_data$label <- factor(train_data$label)
    test_data$label <- factor(test_data$label, levels = levels(train_data$label))
    
    # Entrenar modelo
    rf_model <- randomForest(label ~ ., data = train_data, ntree = 100)
    predictions <- predict(rf_model, test_data)
    
    # Matriz de confusión
    cm <- caret::confusionMatrix(predictions, test_data$label)
    
    # Métricas base
    metrics$Accuracy[i] <- cm$overall["Accuracy"]
    metrics$Balanced_Accuracy[i] <- mean(cm$byClass["Balanced Accuracy"], na.rm = TRUE)
    
    # Cálculo robusto de F1 scores
    f1_scores <- sapply(levels(test_data$label), function(cls) {
      MLmetrics::F1_Score(y_true = as.numeric(test_data$label == cls),
                          y_pred = as.numeric(predictions == cls),
                          positive = 1)
    })
    
    # Pesos para F1 ponderado
    class_weights <- table(test_data$label)/length(test_data$label)
    
    metrics$Weighted_F1[i] <- sum(f1_scores * class_weights, na.rm = TRUE)
    metrics$Macro_F1[i] <- mean(f1_scores, na.rm = TRUE)
  }
  
  # Resultado promedio
  data.frame(
    Dataset = dataset_name,
    Accuracy = mean(metrics$Accuracy, na.rm = TRUE),
    Balanced_Accuracy = mean(metrics$Balanced_Accuracy, na.rm = TRUE),
    Weighted_F1 = mean(metrics$Weighted_F1, na.rm = TRUE),
    Macro_F1 = mean(metrics$Macro_F1, na.rm = TRUE)
  )
}

# Evaluación segura con manejo de errores
evaluation_results <- purrr::map_dfr(names(datasets_to_evaluate), function(name) {
  tryCatch({
    evaluate_dataset_cv(datasets_to_evaluate[[name]], name)
  }, error = function(e) {
    cat("Error evaluando", name, ":", e$message, "\n")
    data.frame(Dataset = name, 
               Accuracy = NA, 
               Balanced_Accuracy = NA,
               Weighted_F1 = NA,
               Macro_F1 = NA)
  })
})

# ===============================================================================
# 15. REPORTE FINAL
# ===============================================================================

cat("\n=== GENERANDO REPORTE FINAL ===\n")

# Determinar el mejor dataset basado en el F1-Score Ponderado
best_dataset_name <- ""
if (exists("evaluation_results") && !is.null(evaluation_results) && nrow(evaluation_results) > 0) {
  best_dataset_name <- evaluation_results %>%
    arrange(desc(Weighted_F1)) %>%
    slice(1) %>%
    pull(Dataset)
} else {
  warning("No se encontraron resultados de evaluación para determinar el mejor dataset.")
}

# Crear reporte resumen
report <- list(
  dataset_info = list(
    original_rows = nrow(opcua_data),
    original_cols = ncol(opcua_data),
    numeric_vars = length(numeric_vars),
    categorical_vars = length(categorical_vars)
  ),
  class_distribution = as.data.frame(table(opcua_data$label)),
  missing_values = sum(missing_values$Missing_Count),
  outliers_detected = sum(outlier_summary$Outlier_Count),
  high_correlations = nrow(high_cor_pairs),
  preprocessing_applied = c("Normalization", "Outlier_Treatment", "Class_Balancing (SMOTE, ROSE, Undersampling)"),
  evaluation_summary = evaluation_results,
  best_dataset = best_dataset_name
)

# Guardar reporte
saveRDS(report, file.path(output_dir, "final_report.rds"))

# Crear resumen en texto
sink(file.path(output_dir, "preprocessing_summary.txt"))
cat("=== REPORTE DE PREPROCESAMIENTO - DATASET OPC UA ===\n\n")
cat("Dataset Original:\n")
cat("- Filas:", report$dataset_info$original_rows, "\n")
cat("- Columnas:", report$dataset_info$original_cols, "\n")
cat("- Variables numéricas:", report$dataset_info$numeric_vars, "\n")
cat("- Variables categóricas:", report$dataset_info$categorical_vars, "\n\n")

cat("Distribución de Clases Original:\n")
print(report$class_distribution)
cat("\n")

cat("Problemas Detectados:\n")
cat("- Valores faltantes:", report$missing_values, "\n")
cat("- Outliers detectados:", report$outliers_detected, "\n")
cat("- Correlaciones altas:", report$high_correlations, "\n\n")

cat("Técnicas de Preprocesamiento Aplicadas:\n")
cat(paste("-", report$preprocessing_applied, collapse = "\n"), "\n\n")

cat("Evaluación Comparativa de Datasets (usando Random Forest con CV de 5 folds):\n")
if (exists("evaluation_results") && !is.null(evaluation_results)) {
  print(knitr::kable(evaluation_results, format = "pipe", digits = 4))
} else {
  cat("No se pudo generar la tabla de evaluación.\n")
}
cat("\n")

cat("Mejor dataset según F1-Score Ponderado:", report$best_dataset, "\n")
sink()

cat("\n=== PREPROCESAMIENTO COMPLETADO ===\n")
cat("Todos los resultados se han guardado en:", output_dir, "\n")
cat("Archivos generados:\n")
cat("- Datasets procesados en: processed_data/\n")
cat("- Gráficos en: plots/\n")
cat("- Tablas en: tables/\n")
cat("- Reporte final: preprocessing_summary.txt\n")

