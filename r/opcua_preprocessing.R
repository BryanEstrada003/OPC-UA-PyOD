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

# Lista de paquetes necesarios
packages <- c(
  "dplyr", "ggplot2", "corrplot", "skimr", "VIM", "mice", 
  "caret", "ROSE", "DMwR", "smotefamily", "performanceEstimation",
  "gridExtra", "reshape2", "RColorBrewer", "viridis", "plotly",
  "kableExtra", "DT", "psych", "Hmisc", "GGally", "pheatmap",
  "randomForest", "e1071", "ROCR", "pROC", "MLmetrics"
)

# Función para instalar paquetes si no están instalados
install_if_missing <- function(packages) {
  new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
  if(length(new_packages)) {
    install.packages(new_packages, dependencies = TRUE)
  }
}

# Instalar paquetes faltantes
install_if_missing(packages)

# Cargar librerías
suppressMessages({
  lapply(packages, library, character.only = TRUE)
})

# ===============================================================================
# 2. CONFIGURACIÓN Y CARGA DE DATOS
# ===============================================================================

# Configurar directorio de trabajo (ajustar según tu ruta)
# setwd("~/tu_directorio_de_trabajo")

# Cargar dataset
cat("Cargando dataset OPC UA...\n")
opcua_data <- read.csv("OPCUA_dataset_public-Original.csv", stringsAsFactors = FALSE)

# Crear directorio para resultados
output_dir <- "opcua_analysis_results"
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

# Identificar variables categóricas
categorical_vars <- c("src_ip", "dst_ip", "proto", "service", "label", "multi_label")

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

# Contar valores faltantes por columna
missing_values <- opcua_data %>%
  summarise_all(~sum(is.na(.))) %>%
  gather(key = "Variable", value = "Missing_Count") %>%
  mutate(Missing_Percentage = (Missing_Count / nrow(opcua_data)) * 100) %>%
  arrange(desc(Missing_Count))

print(missing_values)

# Visualización de valores faltantes
if(sum(missing_values$Missing_Count) > 0) {
  p_missing <- VIM::aggr(opcua_data, col = c('navyblue','red'), 
                        numbers = TRUE, sortVars = TRUE)
  
  # Guardar gráfico de valores faltantes
  png(file.path(output_dir, "plots", "missing_values_pattern.png"), 
      width = 1200, height = 800, res = 150)
  VIM::aggr(opcua_data, col = c('navyblue','red'), 
           numbers = TRUE, sortVars = TRUE)
  dev.off()
} else {
  cat("No se encontraron valores faltantes en el dataset.\n")
}

# ===============================================================================
# 6. ANÁLISIS DE VARIABLES NUMÉRICAS
# ===============================================================================

cat("\n=== ANÁLISIS DE VARIABLES NUMÉRICAS ===\n")

# Identificar variables numéricas (excluyendo IPs que son categóricas)
numeric_vars <- opcua_data %>%
  select(-all_of(categorical_vars)) %>%
  select(where(is.numeric)) %>%
  names()

cat("Variables numéricas identificadas:", length(numeric_vars), "\n")
print(numeric_vars)

# Estadísticas descriptivas para variables numéricas
numeric_summary <- opcua_data %>%
  select(all_of(numeric_vars)) %>%
  describe()

print(numeric_summary)

# Guardar estadísticas descriptivas
write.csv(numeric_summary, file.path(output_dir, "tables", "numeric_summary.csv"))

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

# Calcular matriz de correlación
cor_matrix <- opcua_data %>%
  select(all_of(numeric_vars)) %>%
  cor(use = "complete.obs")

# Encontrar correlaciones altas (> 0.8)
high_cor <- which(abs(cor_matrix) > 0.8 & abs(cor_matrix) < 1, arr.ind = TRUE)
high_cor_pairs <- data.frame(
  Var1 = rownames(cor_matrix)[high_cor[,1]],
  Var2 = colnames(cor_matrix)[high_cor[,2]],
  Correlation = cor_matrix[high_cor]
) %>%
  arrange(desc(abs(Correlation)))

cat("Pares de variables con correlación alta (>0.8):\n")
print(head(high_cor_pairs, 20))

# Visualización de matriz de correlación
png(file.path(output_dir, "plots", "correlation_matrix.png"),
    width = 1200, height = 1000, res = 150)
corrplot(cor_matrix, method = "color", type = "upper",
         tl.col = "black", tl.srt = 45, tl.cex = 0.8,
         title = "Matriz de Correlación - Variables Numéricas",
         mar = c(0,0,2,0))
dev.off()

# Heatmap más detallado con pheatmap
png(file.path(output_dir, "plots", "correlation_heatmap_detailed.png"),
    width = 1400, height = 1200, res = 150)
pheatmap(cor_matrix, 
         color = colorRampPalette(c("blue", "white", "red"))(100),
         main = "Matriz de Correlación Detallada",
         fontsize = 8)
dev.off()

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

set.seed(123)
smote_result <- SMOTE(label ~ ., data = balance_data, perc.over = 200, perc.under = 150)

cat("Distribución después de SMOTE:\n")
print(table(smote_result$label))

# 11.2 ROSE (Random Over-Sampling Examples)
cat("Aplicando ROSE...\n")

set.seed(123)
rose_result <- ROSE(label ~ ., data = balance_data, seed = 123)$data

cat("Distribución después de ROSE:\n")
print(table(rose_result$label))

# 11.3 Undersampling
cat("Aplicando Undersampling...\n")

# Encontrar la clase minoritaria
min_class_size <- min(table(balance_data$label))

set.seed(123)
undersample_result <- balance_data %>%
  group_by(label) %>%
  sample_n(min_class_size) %>%
  ungroup()

cat("Distribución después de Undersampling:\n")
print(table(undersample_result$label))

# ===============================================================================
# 12. SELECCIÓN DE CARACTERÍSTICAS
# ===============================================================================

cat("\n=== SELECCIÓN DE CARACTERÍSTICAS ===\n")

# 12.1 Eliminar variables altamente correlacionadas
high_cor_vars <- findCorrelation(cor_matrix, cutoff = 0.9)
if(length(high_cor_vars) > 0) {
  vars_to_remove <- names(opcua_processed[vars_to_normalize])[high_cor_vars]
  cat("Variables eliminadas por alta correlación:", paste(vars_to_remove, collapse = ", "), "\n")
  
  # Actualizar lista de variables
  vars_to_normalize <- vars_to_normalize[!vars_to_normalize %in% vars_to_remove]
}

# 12.2 Selección basada en importancia (usando Random Forest)
cat("Calculando importancia de variables con Random Forest...\n")

set.seed(123)
rf_model <- randomForest(label ~ ., data = balance_data[c(vars_to_normalize, "label")], 
                        importance = TRUE, ntree = 100)

# Obtener importancia de variables
var_importance <- importance(rf_model)
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

# Función para evaluar un dataset
evaluate_dataset <- function(data, dataset_name) {
  set.seed(123)
  
  # División train/test
  train_index <- createDataPartition(data$label, p = 0.7, list = FALSE)
  train_data <- data[train_index, ]
  test_data <- data[-train_index, ]
  
  # Entrenar modelo Random Forest
  rf_model <- randomForest(label ~ ., data = train_data, ntree = 100)
  
  # Predicciones
  predictions <- predict(rf_model, test_data)
  
  # Métricas
  cm <- confusionMatrix(predictions, test_data$label)
  
  return(list(
    dataset = dataset_name,
    accuracy = cm$overall["Accuracy"],
    sensitivity = cm$byClass["Sensitivity"],
    specificity = cm$byClass["Specificity"],
    f1_score = cm$byClass["F1"]
  ))
}

# Evaluar diferentes datasets
datasets_to_evaluate <- list(
  "Original" = balance_data,
  "SMOTE" = smote_result,
  "ROSE" = rose_result,
  "Undersampled" = undersample_result
)

evaluation_results <- map_dfr(names(datasets_to_evaluate), function(name) {
  result <- evaluate_dataset(datasets_to_evaluate[[name]], name)
  data.frame(
    Dataset = result$dataset,
    Accuracy = result$accuracy,
    Sensitivity = result$sensitivity,
    Specificity = result$specificity,
    F1_Score = result$f1_score
  )
})

print(evaluation_results)

# Guardar resultados de evaluación
write.csv(evaluation_results, 
          file.path(output_dir, "tables", "dataset_evaluation_comparison.csv"), 
          row.names = FALSE)

# ===============================================================================
# 15. REPORTE FINAL
# ===============================================================================

cat("\n=== GENERANDO REPORTE FINAL ===\n")

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
  preprocessing_applied = c("Normalization", "Outlier_Treatment", "Class_Balancing"),
  best_dataset = evaluation_results[which.max(evaluation_results$F1_Score), "Dataset"]
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

cat("Evaluación de Datasets:\n")
print(evaluation_results)
cat("\n")

cat("Mejor dataset según F1-Score:", report$best_dataset, "\n")
sink()

cat("\n=== PREPROCESAMIENTO COMPLETADO ===\n")
cat("Todos los resultados se han guardado en:", output_dir, "\n")
cat("Archivos generados:\n")
cat("- Datasets procesados en: processed_data/\n")
cat("- Gráficos en: plots/\n")
cat("- Tablas en: tables/\n")
cat("- Reporte final: preprocessing_summary.txt\n")

