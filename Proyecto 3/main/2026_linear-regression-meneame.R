# -----------------------------------------------------------------------------
# This code snippet was generated for educational purposes as part of the
# Intelligent Systems course at the University of Deusto.
# -------------------------------------------------------------------------
#
# Authors / Maintainers:
#   - Roberto Carballedo
#   - Fernando Boto
#   - Enrique Onieva
#
# Last updated: March 2026
# Code generated & revised using: Gemini Pro.
#
# Educational use only — University of Deusto
# =========================================================================

# Load required packages
library(caret)
library(ggplot2)
library(lattice)

# --- WORKSPACE CLEANUP ---
cat("\014") # Clear console
if(!is.null(dev.list())) dev.off() # Clear plots
rm(list=ls()) # Clean workspace variables

# Set working directory to the script's location
tryCatch({
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}, error = function(e) message("Warning: Save the script before running rstudioapi."))

# Load custom functions
source("linear-regression-utils.R")

#-----------------------
# READ AND PREPARE DATA
#-----------------------
# Read data
data_original <- read.csv(file="../data/2026_meneame-news.csv", sep=",", header = TRUE)
data <- data_original

# Remove non-numerical columns and label column (Project 4)
data$Texto <- NULL
data$Etiqueta <- NULL

# We separate the data into two datasets to avoid Target Leakage 
# since we are predicting Comentarios and Clicks separately.
data_comentarios <- data
data_comentarios$Clicks <- NULL

data_clicks <- data
data_clicks$Comentarios <- NULL

#----------------------
# PRELIMINARY ANALYSIS
#----------------------
# Print data summary for Comentarios (can be done for clicks too)
print.data.summary(data_comentarios)
# Print data correlations
print.data.correlations(data_comentarios, "Comentarios")

# Plot data distribution is commented out for this specific dataset
# because attempting to plot a grid for 50 independent TF-IDF variables
# causes an "Error in plot.new(): figure margins too large" in the R window.
# plot.data.distribution(data_comentarios, "Comentarios")
# plot.data.distribution(data_clicks, "Clicks")

#-----------------------------------
# GENERATE AND ANALYZE LINEAR MODEL (COMENTARIOS)
#-----------------------------------
# Percentage of training examples
training_p <- 0.7

best_mae_com <- Inf
best_model_com <- NULL
best_test_data_com <- NULL
best_prediction_com <- NULL

# Generate 10 models and pick the best one
for (i in 1:10) {
  # Generate data partition (80% training / 20% test). 
  training_samples <- createDataPartition(y = data_comentarios$Comentarios, p = training_p, list = FALSE)
  
  # Split training and test data
  training_data_com <- data_comentarios[training_samples, ]
  test_data_com     <- data_comentarios[-training_samples, ]
  
  # Create Linear Model predicting 'Comentarios' using all other available features
  model_com <- lm(formula = Comentarios ~ ., data = training_data_com)
  
  # Make predictions using the test data
  prediction_com <- predict(model_com, test_data_com)
  
  # Calculate Mean Absolute Error (MAE)
  mean_avg_error_com <- mean(abs(prediction_com - test_data_com$Comentarios), na.rm = TRUE)
  
  if (mean_avg_error_com < best_mae_com) {
    best_mae_com <- mean_avg_error_com
    best_model_com <- model_com
    best_test_data_com <- test_data_com
    best_prediction_com <- prediction_com
  }
}

print("========= RESULTS FOR COMENTARIOS =========")

# Print Mean Absolute Error
print(paste0("- Best Mean Absolute Error: ", round(best_mae_com, 4)))

# Print summary of the model and coefficients
summary(best_model_com)
print.model.summary(best_model_com)

# 1 y 2. Identificar las palabras con mayor y menor coeficiente negativo
coefs_com <- coef(best_model_com)[-1] # Excluir el Intercept ("Intercepto")
coefs_com_neg <- coefs_com[!is.na(coefs_com) & coefs_com < 0]

print("1. Top 10 palabras con MAYOR coeficiente negativo [Comentarios] (más lejos de 0):")
print(head(sort(coefs_com_neg, decreasing = FALSE), 10))

print("2. Top 10 palabras con MENOR coeficiente negativo [Comentarios] (más cerca de 0):")
print(head(sort(coefs_com_neg, decreasing = TRUE), 10))

# 3. Identificar las 10 noticias más "sorprendentes"
diff_com <- best_test_data_com$Comentarios - best_prediction_com

top5_pos_com_idx <- order(diff_com, decreasing = TRUE)[1:5]
print("3a. Las 5 con mayor diferencia positiva (Real > Predicción) [Comentarios]:")
print(data_original$Texto[as.numeric(rownames(best_test_data_com[top5_pos_com_idx, ]))])

top5_neg_com_idx <- order(diff_com, decreasing = FALSE)[1:5]
print("3b. Las 5 con mayor diferencia negativa (Real < Predicción) [Comentarios]:")
print(data_original$Texto[as.numeric(rownames(best_test_data_com[top5_neg_com_idx, ]))])

# Mejora del modelo eliminando variables menos relevantes (p-value alto)
print("--- PRUEBA DE MEJORA DEL MODELO (COMENTARIOS) ---")
summary_com <- summary(best_model_com)
p_values_com <- summary_com$coefficients[-1, "Pr(>|t|)"]
significant_vars_com <- names(p_values_com[p_values_com < 0.10]) # Consideramos relevantes las < 0.10

if (length(significant_vars_com) > 0) {
  formula_improved_com <- as.formula(paste("Comentarios ~", paste(significant_vars_com, collapse = " + ")))
  model_improved_com <- lm(formula = formula_improved_com, data = training_data_com)
  prediction_improved_com <- predict(model_improved_com, best_test_data_com)
  mae_improved_com <- mean(abs(prediction_improved_com - best_test_data_com$Comentarios), na.rm = TRUE)
  print(paste0("MAE modelo original: ", round(best_mae_com, 4)))
  print(paste0("MAE modelo simplificado: ", round(mae_improved_com, 4)))
  print(if(mae_improved_com < best_mae_com) "¡El modelo MEJORÓ al eliminar las variables irrelevantes!" else "El modelo NO mejoró al quitar variables.")
}

# Show the Residual Plot
plot.model.residuals(best_test_data_com$Comentarios, best_prediction_com, "Comentarios")

#-----------------------------------
# GENERATE AND ANALYZE LINEAR MODEL (CLICKS)
#-----------------------------------
best_mae_cli <- Inf
best_model_cli <- NULL
best_test_data_cli <- NULL
best_prediction_cli <- NULL

# Generate 10 models and pick the best one
for (i in 1:10) {
  training_samples <- createDataPartition(y = data_clicks$Clicks, p = training_p, list = FALSE)
  
  training_data_cli <- data_clicks[training_samples, ]
  test_data_cli     <- data_clicks[-training_samples, ]
  
  model_cli <- lm(formula = Clicks ~ ., data = training_data_cli)
  prediction_cli <- predict(model_cli, test_data_cli)
  mean_avg_error_cli <- mean(abs(prediction_cli - test_data_cli$Clicks), na.rm = TRUE)
  
  if (mean_avg_error_cli < best_mae_cli) {
    best_mae_cli <- mean_avg_error_cli
    best_model_cli <- model_cli
    best_test_data_cli <- test_data_cli
    best_prediction_cli <- prediction_cli
  }
}

print("========= RESULTS FOR CLICKS =========")

# Print Mean Absolute Error
print(paste0("- Best Mean Absolute Error: ", round(best_mae_cli, 4)))

# Print summary of the model and coefficients
summary(best_model_cli)
print.model.summary(best_model_cli)

# 1 y 2. Identificar las palabras con mayor y menor coeficiente negativo
coefs_cli <- coef(best_model_cli)[-1]
coefs_cli_neg <- coefs_cli[!is.na(coefs_cli) & coefs_cli < 0]

print("1. Top 10 palabras con MAYOR coeficiente negativo [Clicks] (más lejos de 0):")
print(head(sort(coefs_cli_neg, decreasing = FALSE), 10))

print("2. Top 10 palabras con MENOR coeficiente negativo [Clicks] (más cerca de 0):")
print(head(sort(coefs_cli_neg, decreasing = TRUE), 10))

# 3. Identificar las 10 noticias más "sorprendentes"
diff_cli <- best_test_data_cli$Clicks - best_prediction_cli

top5_pos_cli_idx <- order(diff_cli, decreasing = TRUE)[1:5]
print("3a. Las 5 con mayor diferencia positiva (Real > Predicción) [Clicks]:")
print(data_original$Texto[as.numeric(rownames(best_test_data_cli[top5_pos_cli_idx, ]))])

top5_neg_cli_idx <- order(diff_cli, decreasing = FALSE)[1:5]
print("3b. Las 5 con mayor diferencia negativa (Real < Predicción) [Clicks]:")
print(data_original$Texto[as.numeric(rownames(best_test_data_cli[top5_neg_cli_idx, ]))])

# Mejora del modelo eliminando variables menos relevantes (p-value alto)
print("--- PRUEBA DE MEJORA DEL MODELO (CLICKS) ---")
summary_cli <- summary(best_model_cli)
p_values_cli <- summary_cli$coefficients[-1, "Pr(>|t|)"]
significant_vars_cli <- names(p_values_cli[p_values_cli < 0.10])

if (length(significant_vars_cli) > 0) {
  formula_improved_cli <- as.formula(paste("Clicks ~", paste(significant_vars_cli, collapse = " + ")))
  model_improved_cli <- lm(formula = formula_improved_cli, data = training_data_cli)
  prediction_improved_cli <- predict(model_improved_cli, best_test_data_cli)
  mae_improved_cli <- mean(abs(prediction_improved_cli - best_test_data_cli$Clicks), na.rm = TRUE)
  print(paste0("MAE modelo original: ", round(best_mae_cli, 4)))
  print(paste0("MAE modelo simplificado: ", round(mae_improved_cli, 4)))
  print(if(mae_improved_cli < best_mae_cli) "¡El modelo MEJORÓ al eliminar las variables irrelevantes!" else "El modelo NO mejoró al quitar variables.")
}

# Show the Residual Plot
plot.model.residuals(best_test_data_cli$Clicks, best_prediction_cli, "Clicks")

#-----------------------------------
# GENERACION AUTOMATICA DE NOTICIAS
#-----------------------------------
print("========= 1000 VECTORES ALEATORIOS =========")
feature_cols <- colnames(data_comentarios)[colnames(data_comentarios) != "Comentarios"]
num_features <- length(feature_cols)

# Set seed for reproducibility
set.seed(12345)

# Generate 1000 random vectors with exactly 5 'ones'
rand_data <- t(replicate(1000, {
  vec <- numeric(num_features)
  vec[sample(1:num_features, 5)] <- 1
  vec
}))

rand_df <- as.data.frame(rand_data)
colnames(rand_df) <- feature_cols

# Predict using both best models
rand_df$Pred_Comentarios <- predict(best_model_com, rand_df)
rand_df$Pred_Clicks <- predict(best_model_cli, rand_df)

# Order top 10 by comments
top10_com_df <- rand_df[order(rand_df$Pred_Comentarios, decreasing = TRUE), ][1:10, ]
print("--- Top 10 vectores con más Comentarios previstos ---")
for (i in 1:10) {
  words <- paste(feature_cols[top10_com_df[i, feature_cols] == 1], collapse = ", ")
  cat(sprintf("Vector %2d (Pred: %.2f comentarios) -> Palabras: %s\n", i, top10_com_df$Pred_Comentarios[i], words))
}

# Order top 10 by clicks
cat("\n")
top10_cli_df <- rand_df[order(rand_df$Pred_Clicks, decreasing = TRUE), ][1:10, ]
print("--- Top 10 vectores con más Clicks previstos ---")
for (i in 1:10) {
  words <- paste(feature_cols[top10_cli_df[i, feature_cols] == 1], collapse = ", ")
  cat(sprintf("Vector %2d (Pred: %.2f clicks) -> Palabras: %s\n", i, top10_cli_df$Pred_Clicks[i], words))
}
