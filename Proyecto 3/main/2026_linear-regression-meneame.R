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
data <- read.csv(file="../data/2026_meneame-news.csv", sep=",", header = TRUE)

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

#-----------------------------------
# GENERATE AND ANALYZE LINEAR MODEL (COMENTARIOS)
#-----------------------------------
# Percentage of training examples
training_p <- 0.8

best_mae_com <- Inf
best_model_com <- NULL
best_test_data_com <- NULL
best_prediction_com <- NULL

# Generate 10 models and pick the best one
for (i in 1:10) {
  # Generate data partition (80% training / 20% test). 
  training_samples <- createDataPartition(y = data_comentarios$Comentarios, p = training_p, list = FALSE)
  
  # Split training and test data
  training_data <- data_comentarios[training_samples, ]
  test_data     <- data_comentarios[-training_samples, ]
  
  # Create Linear Model predicting 'Comentarios' using all other available features
  model <- lm(formula = Comentarios ~ ., data = training_data)
  
  # Make predictions using the test data
  prediction <- predict(model, test_data)
  
  # Calculate Mean Absolute Error (MAE)
  mean_avg_error <- mean(abs(prediction - test_data$Comentarios), na.rm = TRUE)
  
  if (mean_avg_error < best_mae_com) {
    best_mae_com <- mean_avg_error
    best_model_com <- model
    best_test_data_com <- test_data
    best_prediction_com <- prediction
  }
}

print("========= RESULTS FOR COMENTARIOS =========")

# Print Mean Absolute Error
print(paste0("- Best Mean Absolute Error: ", round(best_mae_com, 4)))

# Print summary of the model and coefficients
summary(best_model_com)
print.model.summary(best_model_com)

# Identify top 5 positive and negative differences
diff_com <- best_test_data_com$Comentarios - best_prediction_com

top5_pos_com_idx <- order(diff_com, decreasing = TRUE)[1:5]
print("Top 5 noticias con mayor diferencia positiva (Real > Predicción) [Comentarios]:")
print(best_test_data_com[top5_pos_com_idx, ])

top5_neg_com_idx <- order(diff_com, decreasing = FALSE)[1:5]
print("Top 5 noticias con mayor diferencia negativa (Real < Predicción) [Comentarios]:")
print(best_test_data_com[top5_neg_com_idx, ])

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
  
  training_data <- data_clicks[training_samples, ]
  test_data     <- data_clicks[-training_samples, ]
  
  model <- lm(formula = Clicks ~ ., data = training_data)
  prediction <- predict(model, test_data)
  mean_avg_error <- mean(abs(prediction - test_data$Clicks), na.rm = TRUE)
  
  if (mean_avg_error < best_mae_cli) {
    best_mae_cli <- mean_avg_error
    best_model_cli <- model
    best_test_data_cli <- test_data
    best_prediction_cli <- prediction
  }
}

print("========= RESULTS FOR CLICKS =========")

# Print Mean Absolute Error
print(paste0("- Best Mean Absolute Error: ", round(best_mae_cli, 4)))

# Print summary of the model and coefficients
summary(best_model_cli)
print.model.summary(best_model_cli)

# Identify top 5 positive and negative differences
diff_cli <- best_test_data_cli$Clicks - best_prediction_cli

top5_pos_cli_idx <- order(diff_cli, decreasing = TRUE)[1:5]
print("Top 5 noticias con mayor diferencia positiva (Real > Predicción) [Clicks]:")
print(best_test_data_cli[top5_pos_cli_idx, ])

top5_neg_cli_idx <- order(diff_cli, decreasing = FALSE)[1:5]
print("Top 5 noticias con mayor diferencia negativa (Real < Predicción) [Clicks]:")
print(best_test_data_cli[top5_neg_cli_idx, ])

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
