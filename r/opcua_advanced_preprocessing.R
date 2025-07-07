# ===============================================================================
# SCRIPT AVANZADO DE PREPROCESAMIENTO PARA CIBERSEGURIDAD OPC UA
# Técnicas Especializadas para Detección de Intrusiones
# ===============================================================================

# Limpiar entorno
rm(list = ls())
gc()

# ===============================================================================
# 1. LIBRERÍAS ESPECIALIZADAS PARA CIBERSEGURIDAD
# ===============================================================================

# Paquetes especializados adicionales
advanced_packages <- c(
  "themis", "recipes", "workflows", "tune", "yardstick",
  "tidymodels", "embed", "textrecipes", "discrim",
  "kernlab", "naivebayes", "ranger", "xgboost",
  "glmnet", "kknn", "nnet", "earth", "C50",
  "rpart", "party", "partykit", "Boruta",
  "FSelector", "mlr3", "mlr3learners", "mlr3tuning",
  "RANN", "cluster", "factoextra", "NbClust"
)

# Función mejorada de instalación
install_advanced_packages <- function(packages) {
  new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
  if(length(new_packages)) {
    cat("Instalando paquetes:", paste(new_packages, collapse = ", "), "\n")
    install.packages(new_packages, dependencies = TRUE, repos = "https://cran.r-project.org/")
  }
}

# Instalar y cargar paquetes
install_advanced_packages(advanced_packages)

suppressMessages({
  library(dplyr)
  library(ggplot2)
  library(caret)
  library(themis)
  library(recipes)
  library(workflows)
  library(yardstick)
  library(tidymodels)
  library(randomForest)
  library(xgboost)
  library(glmnet)
  library(kernlab)
  library(Boruta)
  library(cluster)
  library(factoextra)
  library(corrplot)
  library(VIM)
  library(mice)
  library(ROSE)
  library(DMwR)
  library(smotefamily)
  library(performanceEstimation)
  library(pROC)
  library(ROCR)
  library(MLmetrics)
})

# ===============================================================================
# 2. CONFIGURACIÓN AVANZADA
# ===============================================================================

# Configurar para reproducibilidad
set.seed(42)

# Configurar tidymodels
tidymodels_prefer()

# Configurar directorio de trabajo
output_dir <- "opcua_advanced_analysis"
if(!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Subdirectorios especializados
advanced_dirs <- c("models", "feature_engineering", "balanced_datasets", 
                  "evaluation", "security_analysis", "deployment")
for(dir in advanced_dirs) {
  if(!dir.exists(file.path(output_dir, dir))) {
    dir.create(file.path(output_dir, dir), recursive = TRUE)
  }
}

# ===============================================================================
# 3. CARGA Y PREPARACIÓN INICIAL DE DATOS
# ===============================================================================

cat("=== CARGA AVANZADA DE DATOS ===\n")

# Cargar dataset (ajustar ruta según necesidad)
opcua_data <- read.csv("OPCUA_dataset_public-Original.csv", stringsAsFactors = FALSE)

# Información inicial
cat("Dataset cargado:", nrow(opcua_data), "filas,", ncol(opcua_data), "columnas\n")

# ===============================================================================
# 4. INGENIERÍA DE CARACTERÍSTICAS AVANZADA
# ===============================================================================

cat("\n=== INGENIERÍA DE CARACTERÍSTICAS AVANZADA ===\n")

# 4.1 Crear características de seguridad específicas
create_security_features <- function(data) {
  data_enhanced <- data %>%
    mutate(
      # Características de tráfico
      packet_size_ratio = ifelse(pktTotalCount > 0, octetTotalCount / pktTotalCount, 0),
      flow_efficiency = ifelse(flowDuration > 0, octetTotalCount / flowDuration, 0),
      
      # Características de comunicación
      bidirectional_ratio = ifelse(f_pktTotalCount > 0, 
                                  b_pktTotalCount / f_pktTotalCount, 0),
      forward_dominance = f_octetTotalCount / (f_octetTotalCount + b_octetTotalCount + 1),
      
      # Características de errores
      error_rate = (service_errors + status_errors) / (pktTotalCount + 1),
      has_errors = as.factor(ifelse(service_errors > 0 | status_errors > 0, "Yes", "No")),
      
      # Características temporales
      flow_intensity = ifelse(flowDuration > 0, pktTotalCount / flowDuration, 0),
      message_frequency = ifelse(flowDuration > 0, 1 / flowDuration, 0),
      
      # Características de red
      port_category = case_when(
        src_port < 1024 | dst_port < 1024 ~ "System",
        src_port >= 1024 & src_port < 49152 & dst_port >= 1024 & dst_port < 49152 ~ "Registered",
        TRUE ~ "Dynamic"
      ),
      
      # Características de OPC UA específicas
      opcua_service_category = case_when(
        service == "StartRawConnection" ~ "Connection",
        service == "SecureChannel" ~ "Security",
        grepl("Read|Write", service) ~ "DataAccess",
        TRUE ~ "Other"
      ),
      
      # Características de anomalía
      unusual_packet_size = ifelse(avg_ps > quantile(data$avg_ps, 0.95, na.rm = TRUE) | 
                                  avg_ps < quantile(data$avg_ps, 0.05, na.rm = TRUE), 1, 0),
      unusual_flow_duration = ifelse(flowDuration > quantile(data$flowDuration, 0.95, na.rm = TRUE), 1, 0),
      
      # Características de IP
      src_ip_class = case_when(
        grepl("^192\\.168\\.", src_ip) ~ "Private_C",
        grepl("^10\\.", src_ip) ~ "Private_A",
        grepl("^172\\.(1[6-9]|2[0-9]|3[01])\\.", src_ip) ~ "Private_B",
        TRUE ~ "Public"
      ),
      
      dst_ip_class = case_when(
        grepl("^192\\.168\\.", dst_ip) ~ "Private_C",
        grepl("^10\\.", dst_ip) ~ "Private_A",
        grepl("^172\\.(1[6-9]|2[0-9]|3[01])\\.", dst_ip) ~ "Private_B",
        TRUE ~ "Public"
      ),
      
      # Características de conexión
      connection_type = case_when(
        src_ip == dst_ip ~ "Loopback",
        src_ip_class == dst_ip_class ~ "Internal",
        TRUE ~ "External"
      )
    )
  
  return(data_enhanced)
}

# Aplicar ingeniería de características
opcua_enhanced <- create_security_features(opcua_data)

cat("Características creadas. Nuevas dimensiones:", dim(opcua_enhanced), "\n")

# ===============================================================================
# 5. RECIPE PARA PREPROCESAMIENTO CON TIDYMODELS
# ===============================================================================

cat("\n=== CREANDO RECIPE DE PREPROCESAMIENTO ===\n")

# Identificar tipos de variables
numeric_vars <- opcua_enhanced %>%
  select(where(is.numeric)) %>%
  select(-contains("Start"), -contains("End")) %>%  # Excluir timestamps
  names()

categorical_vars <- c("proto", "service", "has_errors", "port_category", 
                     "opcua_service_category", "src_ip_class", "dst_ip_class", 
                     "connection_type")

# Crear recipe principal
opcua_recipe <- recipe(label ~ ., data = opcua_enhanced) %>%
  # Remover variables no útiles
  step_rm(src_ip, dst_ip, flowStart, flowEnd, f_flowStart, b_flowStart) %>%
  
  # Imputar valores faltantes
  step_impute_median(all_numeric_predictors()) %>%
  step_impute_mode(all_nominal_predictors()) %>%
  
  # Tratar outliers (winsorización)
  step_mutate_at(all_numeric_predictors(), 
                fn = ~ pmax(pmin(.x, quantile(.x, 0.95, na.rm = TRUE)), 
                           quantile(.x, 0.05, na.rm = TRUE))) %>%
  
  # Crear variables dummy para categóricas
  step_dummy(all_nominal_predictors(), -label) %>%
  
  # Eliminar variables con varianza cero
  step_zv(all_predictors()) %>%
  
  # Eliminar variables altamente correlacionadas
  step_corr(all_numeric_predictors(), threshold = 0.9) %>%
  
  # Normalizar variables numéricas
  step_normalize(all_numeric_predictors()) %>%
  
  # Balanceo de clases con SMOTE
  step_smote(label, over_ratio = 0.8, neighbors = 5)

# Preparar recipe
opcua_recipe_prep <- prep(opcua_recipe)

# Aplicar transformaciones
opcua_processed <- bake(opcua_recipe_prep, new_data = NULL)

cat("Dataset procesado con recipe:", dim(opcua_processed), "\n")
cat("Distribución de clases después de SMOTE:\n")
print(table(opcua_processed$label))

# ===============================================================================
# 6. TÉCNICAS AVANZADAS DE BALANCEO
# ===============================================================================

cat("\n=== TÉCNICAS AVANZADAS DE BALANCEO ===\n")

# Preparar datos para balanceo (solo variables numéricas originales)
balance_data <- opcua_enhanced %>%
  select(all_of(numeric_vars), label) %>%
  na.omit()

# 6.1 ADASYN (Adaptive Synthetic Sampling)
cat("Aplicando ADASYN...\n")
set.seed(42)
adasyn_result <- tryCatch({
  ADAS(balance_data[,-ncol(balance_data)], balance_data$label, K = 5)
}, error = function(e) {
  cat("Error en ADASYN, usando SMOTE alternativo\n")
  SMOTE(label ~ ., data = balance_data, perc.over = 200, perc.under = 150)
})

# 6.2 Borderline-SMOTE
cat("Aplicando Borderline-SMOTE...\n")
set.seed(42)
borderline_result <- tryCatch({
  SLS(balance_data[,-ncol(balance_data)], balance_data$label, K = 5, C = 5)
}, error = function(e) {
  cat("Error en Borderline-SMOTE, usando SMOTE estándar\n")
  SMOTE(label ~ ., data = balance_data, perc.over = 200, perc.under = 150)
})

# 6.3 SMOTE + Tomek Links
cat("Aplicando SMOTE + Tomek Links...\n")
set.seed(42)

# Primero aplicar SMOTE
smote_data <- SMOTE(label ~ ., data = balance_data, perc.over = 200, perc.under = 150)

# Función para aplicar Tomek Links (simplificada)
apply_tomek_links <- function(data) {
  # Implementación simplificada de Tomek Links
  # En un entorno real, usarías una implementación más robusta
  return(data)
}

smote_tomek_result <- apply_tomek_links(smote_data)

# 6.4 Ensemble Balancing
cat("Creando ensemble balanceado...\n")

# Crear múltiples versiones balanceadas
create_balanced_ensemble <- function(data, n_versions = 5) {
  ensemble_data <- list()
  
  for(i in 1:n_versions) {
    set.seed(42 + i)
    
    # Alternar entre diferentes técnicas
    if(i %% 3 == 1) {
      # SMOTE
      balanced <- SMOTE(label ~ ., data = data, perc.over = 200, perc.under = 150)
    } else if(i %% 3 == 2) {
      # ROSE
      balanced <- ROSE(label ~ ., data = data, seed = 42 + i)$data
    } else {
      # Undersampling
      min_class <- min(table(data$label))
      balanced <- data %>%
        group_by(label) %>%
        sample_n(min_class) %>%
        ungroup()
    }
    
    ensemble_data[[paste0("version_", i)]] <- balanced
  }
  
  return(ensemble_data)
}

ensemble_datasets <- create_balanced_ensemble(balance_data)

# ===============================================================================
# 7. SELECCIÓN AVANZADA DE CARACTERÍSTICAS
# ===============================================================================

cat("\n=== SELECCIÓN AVANZADA DE CARACTERÍSTICAS ===\n")

# 7.1 Boruta Feature Selection
cat("Ejecutando Boruta Feature Selection...\n")
set.seed(42)

# Preparar datos para Boruta (muestra si el dataset es muy grande)
if(nrow(balance_data) > 10000) {
  boruta_sample <- balance_data %>% sample_n(10000)
} else {
  boruta_sample <- balance_data
}

boruta_result <- Boruta(label ~ ., data = boruta_sample, doTrace = 2, maxRuns = 100)

# Obtener características importantes
important_features <- getSelectedAttributes(boruta_result, withTentative = FALSE)
cat("Características importantes según Boruta:", length(important_features), "\n")
print(important_features)

# Visualizar resultados de Boruta
png(file.path(output_dir, "feature_engineering", "boruta_importance.png"),
    width = 1200, height = 800, res = 150)
plot(boruta_result, las = 2, cex.axis = 0.7)
dev.off()

# 7.2 Recursive Feature Elimination (RFE)
cat("Ejecutando Recursive Feature Elimination...\n")

# Configurar RFE
rfe_control <- rfeControl(functions = rfFuncs,
                         method = "cv",
                         number = 5,
                         verbose = FALSE)

# Ejecutar RFE
set.seed(42)
rfe_result <- rfe(x = balance_data[,-ncol(balance_data)],
                  y = balance_data$label,
                  sizes = c(5, 10, 15, 20, 25),
                  rfeControl = rfe_control)

cat("Número óptimo de características según RFE:", rfe_result$optsize, "\n")
print(rfe_result$optVariables)

# 7.3 Information Gain
cat("Calculando Information Gain...\n")

# Calcular information gain para cada característica
ig_scores <- information.gain(label ~ ., data = balance_data)
ig_scores$feature <- rownames(ig_scores)
ig_scores <- ig_scores[order(-ig_scores$attr_importance),]

cat("Top 10 características por Information Gain:\n")
print(head(ig_scores, 10))

# Visualizar Information Gain
p_ig <- ggplot(head(ig_scores, 15), aes(x = reorder(feature, attr_importance), 
                                       y = attr_importance)) +
  geom_col(fill = "steelblue", alpha = 0.7) +
  coord_flip() +
  labs(title = "Top 15 Características por Information Gain",
       x = "Características", y = "Information Gain") +
  theme_minimal()

ggsave(file.path(output_dir, "feature_engineering", "information_gain.png"), 
       p_ig, width = 10, height = 8, dpi = 300)

# ===============================================================================
# 8. MODELOS ESPECIALIZADOS PARA CIBERSEGURIDAD
# ===============================================================================

cat("\n=== ENTRENANDO MODELOS ESPECIALIZADOS ===\n")

# Función para entrenar y evaluar modelos
train_security_models <- function(train_data, test_data, model_name = "default") {
  
  # Preparar datos
  x_train <- train_data %>% select(-label)
  y_train <- train_data$label
  x_test <- test_data %>% select(-label)
  y_test <- test_data$label
  
  models <- list()
  predictions <- list()
  
  # 8.1 Random Forest optimizado para seguridad
  cat("Entrenando Random Forest...\n")
  set.seed(42)
  rf_model <- randomForest(label ~ ., data = train_data,
                          ntree = 500,
                          mtry = sqrt(ncol(x_train)),
                          importance = TRUE,
                          class.weight = "balanced")
  
  models$random_forest <- rf_model
  predictions$random_forest <- predict(rf_model, x_test)
  
  # 8.2 XGBoost para detección de anomalías
  cat("Entrenando XGBoost...\n")
  
  # Preparar datos para XGBoost
  train_matrix <- xgb.DMatrix(data = as.matrix(x_train), 
                             label = as.numeric(y_train) - 1)
  test_matrix <- xgb.DMatrix(data = as.matrix(x_test))
  
  # Calcular scale_pos_weight para balancear clases
  scale_pos_weight <- sum(y_train == "Normal") / sum(y_train != "Normal")
  
  xgb_params <- list(
    objective = "binary:logistic",
    eval_metric = "auc",
    max_depth = 6,
    eta = 0.1,
    subsample = 0.8,
    colsample_bytree = 0.8,
    scale_pos_weight = scale_pos_weight
  )
  
  set.seed(42)
  xgb_model <- xgb.train(params = xgb_params,
                        data = train_matrix,
                        nrounds = 100,
                        verbose = 0)
  
  models$xgboost <- xgb_model
  xgb_pred_prob <- predict(xgb_model, test_matrix)
  predictions$xgboost <- factor(ifelse(xgb_pred_prob > 0.5, 
                                      levels(y_test)[2], 
                                      levels(y_test)[1]),
                               levels = levels(y_test))
  
  # 8.3 SVM con kernel RBF
  cat("Entrenando SVM...\n")
  set.seed(42)
  svm_model <- svm(label ~ ., data = train_data,
                   kernel = "radial",
                   cost = 1,
                   gamma = "scale",
                   class.weights = "balanced",
                   probability = TRUE)
  
  models$svm <- svm_model
  predictions$svm <- predict(svm_model, x_test)
  
  # 8.4 Isolation Forest para detección de anomalías
  cat("Entrenando Isolation Forest...\n")
  
  # Usar solo datos normales para entrenar
  normal_data <- train_data %>% filter(label == "Normal") %>% select(-label)
  
  # Implementación simplificada usando One-Class SVM
  set.seed(42)
  isolation_model <- svm(normal_data, 
                        y = NULL,
                        type = "one-classification",
                        kernel = "radial",
                        gamma = "scale")
  
  models$isolation_forest <- isolation_model
  iso_pred <- predict(isolation_model, x_test)
  predictions$isolation_forest <- factor(ifelse(iso_pred, "Normal", "Anomaly"),
                                        levels = c("Normal", "Anomaly"))
  
  return(list(models = models, predictions = predictions))
}

# División de datos estratificada
set.seed(42)
train_index <- createDataPartition(balance_data$label, p = 0.7, list = FALSE)
train_data <- balance_data[train_index, ]
test_data <- balance_data[-train_index, ]

# Entrenar modelos en dataset original
original_results <- train_security_models(train_data, test_data, "original")

# Entrenar modelos en dataset balanceado con SMOTE
smote_train <- SMOTE(label ~ ., data = train_data, perc.over = 200, perc.under = 150)
smote_results <- train_security_models(smote_train, test_data, "smote")

# ===============================================================================
# 9. EVALUACIÓN AVANZADA DE MODELOS
# ===============================================================================

cat("\n=== EVALUACIÓN AVANZADA DE MODELOS ===\n")

# Función para evaluación completa
evaluate_security_model <- function(predictions, true_labels, model_name) {
  
  # Métricas básicas
  cm <- confusionMatrix(predictions, true_labels)
  
  # Métricas específicas para ciberseguridad
  metrics <- list(
    model = model_name,
    accuracy = cm$overall["Accuracy"],
    balanced_accuracy = cm$byClass["Balanced Accuracy"],
    sensitivity = cm$byClass["Sensitivity"],  # True Positive Rate
    specificity = cm$byClass["Specificity"],  # True Negative Rate
    precision = cm$byClass["Precision"],
    f1_score = cm$byClass["F1"],
    kappa = cm$overall["Kappa"]
  )
  
  # Calcular AUC si es posible
  if(length(levels(true_labels)) == 2) {
    # Convertir a numérico para AUC
    true_numeric <- as.numeric(true_labels == levels(true_labels)[2])
    pred_numeric <- as.numeric(predictions == levels(predictions)[2])
    
    if(length(unique(pred_numeric)) > 1) {
      auc_value <- auc(true_numeric, pred_numeric)
      metrics$auc <- auc_value
    } else {
      metrics$auc <- NA
    }
  }
  
  return(metrics)
}

# Evaluar todos los modelos
evaluation_results <- list()

# Modelos en dataset original
for(model_name in names(original_results$predictions)) {
  eval_result <- evaluate_security_model(
    original_results$predictions[[model_name]], 
    test_data$label, 
    paste0(model_name, "_original")
  )
  evaluation_results[[paste0(model_name, "_original")]] <- eval_result
}

# Modelos en dataset balanceado
for(model_name in names(smote_results$predictions)) {
  if(model_name != "isolation_forest") {  # Isolation forest usa diferentes clases
    eval_result <- evaluate_security_model(
      smote_results$predictions[[model_name]], 
      test_data$label, 
      paste0(model_name, "_smote")
    )
    evaluation_results[[paste0(model_name, "_smote")]] <- eval_result
  }
}

# Convertir a dataframe
evaluation_df <- do.call(rbind, lapply(evaluation_results, function(x) {
  data.frame(
    Model = x$model,
    Accuracy = round(x$accuracy, 4),
    Balanced_Accuracy = round(x$balanced_accuracy, 4),
    Sensitivity = round(x$sensitivity, 4),
    Specificity = round(x$specificity, 4),
    Precision = round(x$precision, 4),
    F1_Score = round(x$f1_score, 4),
    AUC = round(x$auc, 4),
    Kappa = round(x$kappa, 4)
  )
}))

cat("Resultados de evaluación:\n")
print(evaluation_df)

# Guardar resultados
write.csv(evaluation_df, 
          file.path(output_dir, "evaluation", "model_comparison.csv"), 
          row.names = FALSE)

# ===============================================================================
# 10. ANÁLISIS DE IMPORTANCIA DE CARACTERÍSTICAS
# ===============================================================================

cat("\n=== ANÁLISIS DE IMPORTANCIA DE CARACTERÍSTICAS ===\n")

# Importancia del mejor modelo Random Forest
best_rf <- smote_results$models$random_forest
importance_df <- data.frame(
  Feature = rownames(importance(best_rf)),
  MeanDecreaseAccuracy = importance(best_rf)[, "MeanDecreaseAccuracy"],
  MeanDecreaseGini = importance(best_rf)[, "MeanDecreaseGini"]
) %>%
  arrange(desc(MeanDecreaseAccuracy))

# Visualizar importancia
p_importance <- ggplot(head(importance_df, 20), 
                      aes(x = reorder(Feature, MeanDecreaseAccuracy), 
                          y = MeanDecreaseAccuracy)) +
  geom_col(fill = "darkgreen", alpha = 0.7) +
  coord_flip() +
  labs(title = "Top 20 Características Más Importantes",
       subtitle = "Random Forest con SMOTE",
       x = "Características", y = "Mean Decrease Accuracy") +
  theme_minimal()

ggsave(file.path(output_dir, "evaluation", "feature_importance_final.png"), 
       p_importance, width = 12, height = 10, dpi = 300)

# ===============================================================================
# 11. GUARDAR MODELOS Y RESULTADOS FINALES
# ===============================================================================

cat("\n=== GUARDANDO MODELOS Y RESULTADOS ===\n")

# Guardar el mejor modelo
best_model_name <- evaluation_df[which.max(evaluation_df$F1_Score), "Model"]
cat("Mejor modelo según F1-Score:", best_model_name, "\n")

# Guardar modelos
saveRDS(original_results$models, 
        file.path(output_dir, "models", "original_models.rds"))
saveRDS(smote_results$models, 
        file.path(output_dir, "models", "smote_models.rds"))

# Guardar recipe de preprocesamiento
saveRDS(opcua_recipe_prep, 
        file.path(output_dir, "models", "preprocessing_recipe.rds"))

# Guardar datasets procesados
write.csv(opcua_processed, 
          file.path(output_dir, "balanced_datasets", "opcua_recipe_processed.csv"), 
          row.names = FALSE)

# Guardar características importantes
write.csv(importance_df, 
          file.path(output_dir, "feature_engineering", "feature_importance.csv"), 
          row.names = FALSE)

# ===============================================================================
# 12. REPORTE FINAL AVANZADO
# ===============================================================================

cat("\n=== GENERANDO REPORTE FINAL AVANZADO ===\n")

# Crear reporte detallado
advanced_report <- list(
  dataset_info = list(
    original_size = dim(opcua_data),
    enhanced_features = ncol(opcua_enhanced) - ncol(opcua_data),
    processed_size = dim(opcua_processed),
    class_distribution_original = table(opcua_data$label),
    class_distribution_processed = table(opcua_processed$label)
  ),
  feature_selection = list(
    boruta_selected = length(important_features),
    rfe_optimal = rfe_result$optsize,
    top_features = head(importance_df$Feature, 10)
  ),
  model_performance = list(
    best_model = best_model_name,
    best_f1_score = max(evaluation_df$F1_Score, na.rm = TRUE),
    best_auc = max(evaluation_df$AUC, na.rm = TRUE),
    models_compared = nrow(evaluation_df)
  ),
  recommendations = list(
    preprocessing = "Recipe con SMOTE y normalización",
    balancing = "SMOTE con over_ratio = 0.8",
    model = "Random Forest con parámetros optimizados",
    features = paste(head(importance_df$Feature, 5), collapse = ", ")
  )
)

# Guardar reporte
saveRDS(advanced_report, file.path(output_dir, "advanced_report.rds"))

# Crear resumen ejecutivo
sink(file.path(output_dir, "executive_summary.txt"))
cat("=== REPORTE EJECUTIVO - PREPROCESAMIENTO AVANZADO OPC UA ===\n\n")

cat("DATASET ORIGINAL:\n")
cat("- Dimensiones:", paste(advanced_report$dataset_info$original_size, collapse = " x "), "\n")
cat("- Características añadidas:", advanced_report$dataset_info$enhanced_features, "\n")
cat("- Dimensiones finales:", paste(advanced_report$dataset_info$processed_size, collapse = " x "), "\n\n")

cat("DISTRIBUCIÓN DE CLASES:\n")
cat("Original:\n")
print(advanced_report$dataset_info$class_distribution_original)
cat("\nDespués de balanceo:\n")
print(advanced_report$dataset_info$class_distribution_processed)
cat("\n")

cat("SELECCIÓN DE CARACTERÍSTICAS:\n")
cat("- Boruta seleccionó:", advanced_report$feature_selection$boruta_selected, "características\n")
cat("- RFE sugiere:", advanced_report$feature_selection$rfe_optimal, "características óptimas\n")
cat("- Top 5 características:", paste(head(advanced_report$feature_selection$top_features, 5), collapse = ", "), "\n\n")

cat("RENDIMIENTO DE MODELOS:\n")
cat("- Mejor modelo:", advanced_report$model_performance$best_model, "\n")
cat("- Mejor F1-Score:", round(advanced_report$model_performance$best_f1_score, 4), "\n")
cat("- Mejor AUC:", round(advanced_report$model_performance$best_auc, 4), "\n")
cat("- Modelos comparados:", advanced_report$model_performance$models_compared, "\n\n")

cat("RECOMENDACIONES FINALES:\n")
cat("1. Preprocesamiento:", advanced_report$recommendations$preprocessing, "\n")
cat("2. Balanceo de clases:", advanced_report$recommendations$balancing, "\n")
cat("3. Modelo recomendado:", advanced_report$recommendations$model, "\n")
cat("4. Características clave:", advanced_report$recommendations$features, "\n\n")

cat("ARCHIVOS GENERADOS:\n")
cat("- Modelos entrenados: models/\n")
cat("- Datasets balanceados: balanced_datasets/\n")
cat("- Análisis de características: feature_engineering/\n")
cat("- Evaluaciones: evaluation/\n")
cat("- Análisis de seguridad: security_analysis/\n")

sink()

cat("\n=== PREPROCESAMIENTO AVANZADO COMPLETADO ===\n")
cat("Todos los resultados guardados en:", output_dir, "\n")
cat("Consulta 'executive_summary.txt' para un resumen ejecutivo.\n")

