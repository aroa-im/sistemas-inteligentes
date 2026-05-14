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
# Last updated: Mayo 2026
# Code generated & revised using: Gemini Pro.
#
# Educational use only — University of Deusto
# =========================================================================

# Load required packages
library(lattice)
library(ggplot2)
library(caret)
library(rpart)
library(rpart.plot)

# --- WORKSPACE CLEANUP ---
cat("\014") # Clear console
if(!is.null(dev.list())) dev.off() # Clear plots
rm(list=ls()) # Clean workspace variables

# Set working directory to the script's location
tryCatch({
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}, error = function(e) message("Warning: Save the script before running rstudioapi."))

# Load custom functions
source("decision-tree-learning-utils.R")

#-----------------------
# 1. LOAD AND PREPARE DATA
#-----------------------
# Read data from CSV
filename <- "../data/2026_meneame-news.csv"
data_original <- read.csv(file = filename, header = TRUE, stringsAsFactors = FALSE)
data <- data_original

# Remove the Texto column as text cannot be used directly in trees
data$Texto <- NULL

# Convert the numerical columns (Clicks, Comentarios) into qualitative ranges
# This is explicitly requested in the problem description
# Let's break them into quartiles (4 breaks)
tryCatch({
    data$Comentarios <- cut(data$Comentarios, breaks = 4, labels = c("Low", "Mid", "High", "VeryHigh"))
    data$Clicks <- cut(data$Clicks, breaks = 4, labels = c("Low", "Mid", "High", "VeryHigh"))
}, error = function(e) { message("Could not cut numeric variables") })

# Convert all variables to factors
data[] <- lapply(data, as.factor)

#---------------------------------------------------
# 2. GENERATE 10 DECISION TREE MODELS (MULTI-CLASS)
#---------------------------------------------------
training_p <- 0.75

best_accuracy_multi <- 0
best_model_multi <- NULL
best_test_data_multi <- NULL
best_pred_multi <- NULL

print("========= 10 MODELOS: MÚLTIPLES ETIQUETAS =========")

for (i in 1:10) {
  # Partition: 75% train / 25% test
  training_indexes <- createDataPartition(y = data$Etiqueta, p = training_p, list = FALSE)
  
  training_data <- data[training_indexes, ]
  test_data     <- data[-training_indexes, ]
  
  # Create Decision Tree: depth limited to 5
  model <- rpart(formula = Etiqueta ~ ., data = training_data, control = rpart.control(maxdepth = 5))
  
  # Evaluate accuracy
  prediction <- predict(model, test_data, type = "class")
  
  prediction_results <- table(test_data$Etiqueta, prediction)
  confusion_mat <- confusionMatrix(prediction_results)
  accuracy <- confusion_mat$overall["Accuracy"]
  
  if (accuracy > best_accuracy_multi) {
    best_accuracy_multi <- accuracy
    best_model_multi <- model
    best_test_data_multi <- test_data
    best_pred_multi <- prediction
  }
}

# Evaluate the BEST multi-class model
print(paste0("Mejor Accuracy (Múltiples Etiquetas): ", round(best_accuracy_multi * 100, 2), "%"))

pred_results_multi <- table(best_test_data_multi$Etiqueta, best_pred_multi)
precision_multi <- diag(pred_results_multi) / colSums(pred_results_multi)
recall_multi <- diag(pred_results_multi) / rowSums(pred_results_multi)

cat("\nMétricas por Clase [Mejor Modelo]:\n")
for (cls in names(precision_multi)) {
  p_val <- ifelse(is.nan(precision_multi[cls]), 0, precision_multi[cls])
  r_val <- ifelse(is.nan(recall_multi[cls]), 0, recall_multi[cls])
  cat(sprintf(" - %s -> Precision: %.2f%% | Recall: %.2f%%\n", cls, p_val * 100, r_val * 100))
}

# Image and rules of the best tree
rpart.plot(best_model_multi, type = 2, extra = 104, tweak = 1.1, main = "Meneame - Etiquetas")
cat("\n--- Reglas del Árbol (Etiquetas) ---\n")
rpart.rules(best_model_multi, style = "tall", cover = TRUE)

# Top 5 most relevant attributes
top5_attrs_multi <- head(names(best_model_multi$variable.importance), 5)
cat("\nTop 5 atributos más relevantes:\n")
print(top5_attrs_multi)

# List of wrongly labeled news
cat("\n--- Noticias mal etiquetadas ---\n")
wrong_idx <- which(best_test_data_multi$Etiqueta != best_pred_multi)
if (length(wrong_idx) > 0) {
  # Print the first 5 wrong examples just as summary
  limit_w <- min(5, length(wrong_idx))
  for (j in 1:limit_w) {
    orig_idx <- as.numeric(rownames(best_test_data_multi[wrong_idx[j], ]))
    cat(sprintf("Predijo: '%s' | Real: '%s' | Titular: %s\n", 
                best_pred_multi[wrong_idx[j]], 
                best_test_data_multi$Etiqueta[wrong_idx[j]], 
                data_original$Texto[orig_idx]))
  }
} else {
  cat("¡Ninguna! Clasificación perfecta.\n")
}

#---------------------------------------------------
# 3. BINARY CLASSIFICATION: POLÍTICA / NO POLÍTICA
#---------------------------------------------------
cat("\n\n========= PROBLEMA BINARIO: POLÍTICA / NO POLÍTICA =========\n")

# Adapt the dataset
data_bin <- data
data_bin$Etiqueta <- ifelse(data_bin$Etiqueta == "politica", "politica", "no-politica")
data_bin$Etiqueta <- as.factor(data_bin$Etiqueta)

best_accuracy_bin <- 0
best_model_bin <- NULL
best_test_data_bin <- NULL
best_pred_bin <- NULL

for (i in 1:10) {
  training_indexes <- createDataPartition(y = data_bin$Etiqueta, p = training_p, list = FALSE)
  training_data_bin <- data_bin[training_indexes, ]
  test_data_bin     <- data_bin[-training_indexes, ]
  
  model_bin <- rpart(formula = Etiqueta ~ ., data = training_data_bin, control = rpart.control(maxdepth = 5))
  prediction_bin <- predict(model_bin, test_data_bin, type = "class")
  
  pred_res <- table(test_data_bin$Etiqueta, prediction_bin)
  conf_mat <- confusionMatrix(pred_res)
  acc <- conf_mat$overall["Accuracy"]
  
  if (acc > best_accuracy_bin) {
    best_accuracy_bin <- acc
    best_model_bin <- model_bin
    best_test_data_bin <- test_data_bin
    best_pred_bin <- prediction_bin
  }
}

print(paste0("Mejor Accuracy (Politica / No Politica): ", round(best_accuracy_bin * 100, 2), "%"))

pred_results_bin <- table(best_test_data_bin$Etiqueta, best_pred_bin)
precision_bin <- diag(pred_results_bin) / colSums(pred_results_bin)
recall_bin <- diag(pred_results_bin) / rowSums(pred_results_bin)

cat("\nMétricas por Clase [Problema Binario]:\n")
for (cls in names(precision_bin)) {
  p_val <- ifelse(is.nan(precision_bin[cls]), 0, precision_bin[cls])
  r_val <- ifelse(is.nan(recall_bin[cls]), 0, recall_bin[cls])
  cat(sprintf(" - %s -> Precision: %.2f%% | Recall: %.2f%%\n", cls, p_val * 100, r_val * 100))
}

rpart.plot(best_model_bin, type = 2, extra = 104, tweak = 1.1, main = "Meneame - Binario (Pol/NoPol)", box.palette="RdBu")

# Top 5 most relevant attributes for Binary problem
top5_attrs_bin <- head(names(best_model_bin$variable.importance), 5)
cat("\nTop 5 atributos más relevantes (Binario):\n")
print(top5_attrs_bin)
