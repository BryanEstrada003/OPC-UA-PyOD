# ===============================================================================
# SCRIPT SIMPLE DE PREPROCESAMIENTO PARA DATASET OPC UA
# Adaptado del script original del usuario para análisis de ciberseguridad
# ===============================================================================

# Limpiar entorno
rm(list = ls())

# ===============================================================================
# 1. INSTALACIÓN Y CARGA DE LIBRERÍAS BÁSICAS
# ===============================================================================

# Instalar paquetes si no están disponibles
required_packages <- c("dplyr", "ggplot2", "skimr", "corrplot", "caret", 
                      "ROSE", "DMwR", "VIM", "gridExtra", "viridis")

install_if_missing <- function(packages) {
  new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
  if(length(new_packages)) {
    install.packages(new_packages, dependencies = TRUE)
  }
}

install_if_missing(required_packages)

# Cargar librerías
library(dplyr)
library(ggplot2)
library(skimr)
library(corrplot)
library(caret)
library(ROSE)
library(DMwR)
library(VIM)
library(gridExtra)
library(viridis)

# ===============================================================================
# 2. CARGAR DATASET OPC UA
# ===============================================================================

# Cargar el dataset (ajustar la ruta según tu ubicación)
OPCUA_dataset <- read.csv("OPCUA_dataset_public-Original.csv")

cat("Dataset cargado exitosamente!\n")
cat("Dimensiones:", dim(OPCUA_dataset), "\n")

# ===============================================================================
# 3. CREAR DIRECTORIOS PARA RESULTADOS
# ===============================================================================

# Crear directorios para organizar resultados
base_dir <- "analisis_opcua_resultados"
dirs <- c("histogramas", "barras", "boxplots", "correlaciones", "datos_procesados")

for(dir in dirs) {
  dir_path <- file.path(base_dir, dir)
  if(!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }
}

# ===============================================================================
# 4. ANÁLISIS EXPLORATORIO BÁSICO
# ===============================================================================

cat("\n=== ANÁLISIS EXPLORATORIO INICIAL ===\n")

# Resumen estadístico usando skimr
cat("Generando resumen estadístico...\n")
resumen_estadistico <- skim(OPCUA_dataset)
print(resumen_estadistico)

# Guardar resumen estadístico
output_file <- file.path(base_dir, "resumen_estadistico.txt")
sink(output_file)
cat("========== RESUMEN ESTADÍSTICO DATASET OPC UA ==========\n\n")
cat("=== INFORMACIÓN BÁSICA ===\n")
cat("Filas:", nrow(OPCUA_dataset), "\n")
cat("Columnas:", ncol(OPCUA_dataset), "\n")
cat("Tamaño en memoria:", format(object.size(OPCUA_dataset), units = "MB"), "\n\n")

cat("=== SUMMARY ===\n")
print(summary(OPCUA_dataset))
cat("\n\n=== SKIM ===\n")
print(resumen_estadistico)

cat("\n\n=== DISTRIBUCIÓN DE CLASES ===\n")
cat("Variable 'label':\n")
print(table(OPCUA_dataset$label))
print(prop.table(table(OPCUA_dataset$label)))

cat("\nVariable 'multi_label':\n")
print(table(OPCUA_dataset$multi_label))
print(prop.table(table(OPCUA_dataset$multi_label)))
sink()

# ===============================================================================
# 5. ANÁLISIS DE VARIABLES NUMÉRICAS
# ===============================================================================

cat("\n=== ANÁLISIS DE VARIABLES NUMÉRICAS ===\n")

# Identificar columnas numéricas (excluyendo IPs y timestamps)
numeric_cols <- OPCUA_dataset %>% 
  select(where(is.numeric)) %>%
  select(-contains("Start"), -contains("End")) %>%  # Excluir timestamps
  names()

cat("Variables numéricas identificadas:", length(numeric_cols), "\n")

# Generar histogramas para variables numéricas
cat("Generando histogramas...\n")
for(col in numeric_cols) {
  file_name <- file.path(base_dir, "histogramas", paste0("hist_", col, ".png"))
  
  png(file_name, width = 800, height = 600)
  hist(OPCUA_dataset[[col]], 
       main = paste("Histograma de", col), 
       xlab = col,
       col = "lightblue",
       border = "black")
  dev.off()
}

# Generar boxplots horizontales
cat("Generando boxplots...\n")
for(col in numeric_cols) {
  file_name <- file.path(base_dir, "boxplots", paste0("boxplot_", col, ".png"))
  
  p <- ggplot(OPCUA_dataset, aes(x = .data[[col]])) +
    geom_boxplot(fill = "lightgreen", color = "black") +
    coord_flip() +
    labs(title = paste("Boxplot de", col), x = col) +
    theme_minimal()
  
  ggsave(file_name, plot = p, width = 10, height = 6, dpi = 300)
}

# ===============================================================================
# 6. ANÁLISIS DE VARIABLES CATEGÓRICAS
# ===============================================================================

cat("\n=== ANÁLISIS DE VARIABLES CATEGÓRICAS ===\n")

# Identificar variables categóricas importantes
categorical_cols <- c("proto", "service", "label", "multi_label")

# Función para generar gráficos de barras mejorados
generar_grafico_barras_opcua <- function(data, columna) {
  # Limitar a las categorías más frecuentes si hay muchas
  if(length(unique(data[[columna]])) > 15) {
    top_categories <- data %>%
      count(.data[[columna]], sort = TRUE) %>%
      head(15) %>%
      pull(.data[[columna]])
    
    data_filtered <- data %>%
      filter(.data[[columna]] %in% top_categories)
  } else {
    data_filtered <- data
  }
  
  ggplot(data_filtered, aes(x = .data[[columna]], fill = .data[[columna]])) +
    geom_bar(stat = "count", color = "black", width = 0.7) +
    geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +
    labs(x = columna, 
         y = "Frecuencia", 
         title = paste("Distribución de", columna)) +
    scale_fill_viridis_d() +
    theme_minimal() +
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 45, hjust = 1))
}

# Generar gráficos de barras
for(col in categorical_cols) {
  if(col %in% names(OPCUA_dataset)) {
    p <- generar_grafico_barras_opcua(OPCUA_dataset, col)
    
    file_name <- file.path(base_dir, "barras", paste0("bar_", col, ".png"))
    ggsave(file_name, plot = p, width = 10, height = 6, dpi = 300)
  }
}

# ===============================================================================
# 7. ANÁLISIS DE CORRELACIONES
# ===============================================================================

cat("\n=== ANÁLISIS DE CORRELACIONES ===\n")

# Calcular matriz de correlación para variables numéricas
cor_data <- OPCUA_dataset %>%
  select(all_of(numeric_cols)) %>%
  cor(use = "complete.obs")

# Generar heatmap de correlación
file_name <- file.path(base_dir, "correlaciones", "heatmap_correlacion.png")
png(file_name, width = 1200, height = 1000)
corrplot(cor_data, 
         method = "color", 
         type = "upper",
         tl.col = "black", 
         tl.srt = 45,
         tl.cex = 0.8,
         title = "Matriz de Correlación - Variables Numéricas OPC UA",
         mar = c(0,0,2,0))
dev.off()

# Encontrar correlaciones altas
high_cor <- which(abs(cor_data) > 0.8 & abs(cor_data) < 1, arr.ind = TRUE)
if(length(high_cor) > 0) {
  high_cor_pairs <- data.frame(
    Variable1 = rownames(cor_data)[high_cor[,1]],
    Variable2 = colnames(cor_data)[high_cor[,2]],
    Correlacion = cor_data[high_cor]
  ) %>%
    arrange(desc(abs(Correlacion)))
  
  cat("Variables con correlación alta (>0.8):\n")
  print(head(high_cor_pairs, 10))
  
  # Guardar correlaciones altas
  write.csv(high_cor_pairs, 
            file.path(base_dir, "correlaciones", "correlaciones_altas.csv"), 
            row.names = FALSE)
}

# ===============================================================================
# 8. ANÁLISIS DEL DESBALANCE DE CLASES
# ===============================================================================

cat("\n=== ANÁLISIS DEL DESBALANCE DE CLASES ===\n")

# Análisis detallado de las clases
cat("Distribución de la variable 'label':\n")
label_dist <- table(OPCUA_dataset$label)
print(label_dist)
print(prop.table(label_dist))

cat("\nDistribución de la variable 'multi_label':\n")
multi_label_dist <- table(OPCUA_dataset$multi_label)
print(multi_label_dist)
print(prop.table(multi_label_dist))

# Visualización del desbalance
p1 <- ggplot(OPCUA_dataset, aes(x = label, fill = label)) +
  geom_bar(stat = "count", color = "black") +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +
  labs(title = "Distribución de Clases Binarias", 
       x = "Clase", y = "Frecuencia") +
  scale_fill_viridis_d() +
  theme_minimal()

p2 <- ggplot(OPCUA_dataset, aes(x = multi_label, fill = multi_label)) +
  geom_bar(stat = "count", color = "black") +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5, size = 3) +
  labs(title = "Distribución de Multi-Clases", 
       x = "Tipo de Ataque", y = "Frecuencia") +
  scale_fill_viridis_d() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Guardar gráfico combinado
combined_plot <- grid.arrange(p1, p2, ncol = 2)
ggsave(file.path(base_dir, "distribucion_clases.png"), 
       combined_plot, width = 14, height = 6, dpi = 300)

# ===============================================================================
# 9. PREPROCESAMIENTO DE DATOS
# ===============================================================================

cat("\n=== INICIANDO PREPROCESAMIENTO ===\n")

# Crear copia para preprocesamiento
OPCUA_procesado <- OPCUA_dataset

# 9.1 Convertir variables categóricas a factor
cat("Convirtiendo variables categóricas...\n")
OPCUA_procesado$proto <- as.factor(OPCUA_procesado$proto)
OPCUA_procesado$service <- as.factor(OPCUA_procesado$service)
OPCUA_procesado$label <- as.factor(OPCUA_procesado$label)
OPCUA_procesado$multi_label <- as.factor(OPCUA_procesado$multi_label)

# 9.2 Normalizar variables numéricas
cat("Normalizando variables numéricas...\n")

# Seleccionar variables para normalizar (excluyendo timestamps y flags)
vars_normalizar <- numeric_cols[!grepl("flow|Start|End|flags", numeric_cols)]

# Aplicar normalización (z-score)
preprocess_params <- preProcess(OPCUA_procesado[vars_normalizar], 
                               method = c("center", "scale"))
OPCUA_procesado[vars_normalizar] <- predict(preprocess_params, 
                                           OPCUA_procesado[vars_normalizar])

# 9.3 Tratar outliers (winsorización)
cat("Tratando outliers...\n")

winsorize <- function(x, probs = c(0.05, 0.95)) {
  quantiles <- quantile(x, probs = probs, na.rm = TRUE)
  x[x < quantiles[1]] <- quantiles[1]
  x[x > quantiles[2]] <- quantiles[2]
  return(x)
}

# Aplicar winsorización a variables normalizadas
for(var in vars_normalizar) {
  OPCUA_procesado[[var]] <- winsorize(OPCUA_procesado[[var]])
}

# ===============================================================================
# 10. TÉCNICAS DE BALANCEO DE CLASES
# ===============================================================================

cat("\n=== APLICANDO TÉCNICAS DE BALANCEO ===\n")

# Preparar datos para balanceo (solo variables numéricas + label)
datos_balanceo <- OPCUA_procesado %>%
  select(all_of(vars_normalizar), label) %>%
  na.omit()

cat("Datos preparados para balanceo:", dim(datos_balanceo), "\n")
cat("Distribución original:\n")
print(table(datos_balanceo$label))

# 10.1 SMOTE
cat("\nAplicando SMOTE...\n")
set.seed(123)
datos_smote <- SMOTE(label ~ ., data = datos_balanceo, 
                     perc.over = 200, perc.under = 150)

cat("Distribución después de SMOTE:\n")
print(table(datos_smote$label))

# 10.2 ROSE
cat("\nAplicando ROSE...\n")
set.seed(123)
datos_rose <- ROSE(label ~ ., data = datos_balanceo, seed = 123)$data

cat("Distribución después de ROSE:\n")
print(table(datos_rose$label))

# 10.3 Undersampling
cat("\nAplicando Undersampling...\n")
min_class_size <- min(table(datos_balanceo$label))

set.seed(123)
datos_undersample <- datos_balanceo %>%
  group_by(label) %>%
  sample_n(min_class_size) %>%
  ungroup()

cat("Distribución después de Undersampling:\n")
print(table(datos_undersample$label))

# ===============================================================================
# 11. GUARDAR DATASETS PROCESADOS
# ===============================================================================

cat("\n=== GUARDANDO DATASETS PROCESADOS ===\n")

# Guardar dataset original procesado
write.csv(OPCUA_procesado, 
          file.path(base_dir, "datos_procesados", "opcua_procesado_original.csv"), 
          row.names = FALSE)

# Guardar datasets balanceados
write.csv(datos_smote, 
          file.path(base_dir, "datos_procesados", "opcua_smote.csv"), 
          row.names = FALSE)

write.csv(datos_rose, 
          file.path(base_dir, "datos_procesados", "opcua_rose.csv"), 
          row.names = FALSE)

write.csv(datos_undersample, 
          file.path(base_dir, "datos_procesados", "opcua_undersample.csv"), 
          row.names = FALSE)

# Guardar parámetros de preprocesamiento
saveRDS(preprocess_params, 
        file.path(base_dir, "datos_procesados", "parametros_preprocesamiento.rds"))

# ===============================================================================
# 12. EVALUACIÓN RÁPIDA DE DATASETS
# ===============================================================================

cat("\n=== EVALUACIÓN RÁPIDA DE DATASETS ===\n")

# Función simple para evaluar un dataset
evaluar_dataset_simple <- function(datos, nombre_dataset) {
  set.seed(123)
  
  # División train/test
  train_index <- createDataPartition(datos$label, p = 0.7, list = FALSE)
  train_data <- datos[train_index, ]
  test_data <- datos[-train_index, ]
  
  # Modelo Random Forest simple
  library(randomForest)
  modelo_rf <- randomForest(label ~ ., data = train_data, ntree = 100)
  
  # Predicciones
  predicciones <- predict(modelo_rf, test_data)
  
  # Matriz de confusión
  cm <- confusionMatrix(predicciones, test_data$label)
  
  return(data.frame(
    Dataset = nombre_dataset,
    Accuracy = round(cm$overall["Accuracy"], 4),
    Sensitivity = round(cm$byClass["Sensitivity"], 4),
    Specificity = round(cm$byClass["Specificity"], 4),
    F1_Score = round(cm$byClass["F1"], 4)
  ))
}

# Evaluar diferentes datasets
datasets_evaluar <- list(
  "Original" = datos_balanceo,
  "SMOTE" = datos_smote,
  "ROSE" = datos_rose,
  "Undersampled" = datos_undersample
)

resultados_evaluacion <- do.call(rbind, 
  lapply(names(datasets_evaluar), function(nombre) {
    evaluar_dataset_simple(datasets_evaluar[[nombre]], nombre)
  })
)

cat("Resultados de evaluación:\n")
print(resultados_evaluacion)

# Guardar resultados
write.csv(resultados_evaluacion, 
          file.path(base_dir, "evaluacion_datasets.csv"), 
          row.names = FALSE)

# ===============================================================================
# 13. REPORTE FINAL SIMPLE
# ===============================================================================

cat("\n=== GENERANDO REPORTE FINAL ===\n")

# Crear reporte final
reporte_final <- file.path(base_dir, "reporte_final_simple.txt")
sink(reporte_final)

cat("=== REPORTE FINAL - PREPROCESAMIENTO DATASET OPC UA ===\n\n")

cat("INFORMACIÓN DEL DATASET:\n")
cat("- Dataset original:", nrow(OPCUA_dataset), "filas x", ncol(OPCUA_dataset), "columnas\n")
cat("- Variables numéricas analizadas:", length(numeric_cols), "\n")
cat("- Variables categóricas analizadas:", length(categorical_cols), "\n\n")

cat("DISTRIBUCIÓN DE CLASES ORIGINAL:\n")
print(table(OPCUA_dataset$label))
cat("Porcentajes:\n")
print(round(prop.table(table(OPCUA_dataset$label)) * 100, 2))
cat("\n")

cat("PROBLEMAS IDENTIFICADOS:\n")
cat("- Desbalance extremo de clases\n")
cat("- Variables en diferentes escalas\n")
cat("- Presencia de outliers\n")
cat("- Variables altamente correlacionadas\n\n")

cat("TÉCNICAS APLICADAS:\n")
cat("- Normalización de variables numéricas\n")
cat("- Tratamiento de outliers (winsorización)\n")
cat("- Balanceo de clases (SMOTE, ROSE, Undersampling)\n")
cat("- Análisis de correlaciones\n\n")

cat("EVALUACIÓN DE DATASETS BALANCEADOS:\n")
print(resultados_evaluacion)
cat("\n")

mejor_dataset <- resultados_evaluacion[which.max(resultados_evaluacion$F1_Score), "Dataset"]
cat("MEJOR DATASET SEGÚN F1-SCORE:", mejor_dataset, "\n\n")

cat("ARCHIVOS GENERADOS:\n")
cat("- Gráficos exploratorios en subdirectorios correspondientes\n")
cat("- Datasets procesados en: datos_procesados/\n")
cat("- Resumen estadístico: resumen_estadistico.txt\n")
cat("- Evaluación de datasets: evaluacion_datasets.csv\n")
cat("- Este reporte: reporte_final_simple.txt\n\n")

cat("RECOMENDACIONES:\n")
cat("1. Usar el dataset", mejor_dataset, "para entrenar modelos\n")
cat("2. Considerar métricas balanceadas (F1-Score) en lugar de Accuracy\n")
cat("3. Validar resultados con validación cruzada\n")
cat("4. Considerar ensemble methods para mejorar rendimiento\n")

sink()

# ===============================================================================
# 14. MENSAJE FINAL
# ===============================================================================

cat("\n=== PREPROCESAMIENTO COMPLETADO EXITOSAMENTE ===\n")
cat("Directorio de resultados:", base_dir, "\n")
cat("Archivos principales generados:\n")
cat("- resumen_estadistico.txt: Análisis estadístico completo\n")
cat("- reporte_final_simple.txt: Reporte ejecutivo\n")
cat("- evaluacion_datasets.csv: Comparación de técnicas de balanceo\n")
cat("- datos_procesados/: Datasets listos para machine learning\n\n")

cat("PRÓXIMOS PASOS RECOMENDADOS:\n")
cat("1. Revisar el reporte final para entender los resultados\n")
cat("2. Usar el dataset", mejor_dataset, "para entrenar modelos\n")
cat("3. Implementar validación cruzada para evaluación robusta\n")
cat("4. Considerar técnicas de ensemble para mejorar detección\n\n")

cat("¡Análisis completado! Revisa los archivos generados para más detalles.\n")

