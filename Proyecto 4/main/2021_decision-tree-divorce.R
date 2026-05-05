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
# Last updated: April 2026
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
filename <- "../data/2021_divorce.csv"
data <- read.csv(file = filename, sep = ";", header = TRUE)

# Convert all columns to factors for classification
data[] <- lapply(data, as.factor)

#------------------------
# 2. EXPLORATORY DATA ANALYSIS (EDA)
#------------------------
images_folder <- "../data/images/2021-divorce/"
plot.data.distribution(data, target = "Class", folder = images_folder)
plot.data.frequency(data, folder = images_folder)

#---------------------------------------------------
# 3. GENERATE BEST DECISION TREE MODEL (20 Iterations)
#---------------------------------------------------
training_p <- 0.75
best_accuracy <- 0
best_model <- NULL
best_prediction_results <- NULL # <-- Novedad: Guardamos la mejor matriz

cat("Starting model iterations to find the best fit...\n")

for (i in 1:20) {
  # Generate data partition
  training_indexes <- createDataPartition(y = data$Class, p = training_p, list = FALSE)
  training_data <- data[training_indexes, ]
  test_data     <- data[-training_indexes, ]
  
  # Create Decision Tree Model
  model <- rpart(formula = Class ~ ., data = training_data)
  
  # Make predictions
  prediction <- predict(model, test_data, type = "class")
  
  # Calculate accuracy
  prediction_results <- table(test_data$Class, prediction)
  confusion_mat <- confusionMatrix(prediction_results)
  accuracy <- confusion_mat$overall["Accuracy"]
  
  # Update best model if accuracy improves
  if (accuracy > best_accuracy) {
    best_accuracy <- accuracy
    best_model <- model
    best_prediction_results <- prediction_results # <-- Guardamos los resultados del mejor modelo
  }
  
  # Optional: Print progress
  cat(sprintf("  Iteration %02d: Current Accuracy = %.2f%%\n", i, 100 * accuracy))
}

#---------------------------------------------------
# 4. MODEL EVALUATION & VISUALIZATION
#---------------------------------------------------
# Precision(C): penalizes False Positives (FP)
# -> Of all samples predicted as class C, how many are actually C?
precision_per_class <- diag(best_prediction_results) / colSums(best_prediction_results)

# Recall(C): penalizes False Negatives (FN)
# -> Of all real samples of class C, how many are correctly predicted?
recall_per_class <- diag(best_prediction_results) / rowSums(best_prediction_results)

# Print overall accuracy of the best model
cat(sprintf("\nBest Model Accuracy = %.2f%%\n", 100 * best_accuracy))

# Print metrics for each specific class
cat("\nMetrics per Class (Best Model):\n")
for (class_name in names(precision_per_class)) {
  # Handle potential NaN (0/0) if a class was never predicted or has no actual cases
  prec_val <- ifelse(is.nan(precision_per_class[class_name]), 0, 100 * precision_per_class[class_name])
  rec_val  <- ifelse(is.nan(recall_per_class[class_name]), 0, 100 * recall_per_class[class_name])
  
  cat(sprintf("  Class '%s' -> Precision: %6.2f%% | Recall: %6.2f%%\n", 
              class_name, prec_val, rec_val))
}

# Print attributes in order of relevance
attrs <- names(best_model$variable.importance)
cat("\nTop Attributes by Relevance:\n  ", paste(head(attrs, 5), collapse = "\n   "), "\n")

# Plot the best tree
rpart.plot(best_model,
           type = 2,
           extra = 102,
           tweak = 1.1,
           box.palette = "BuGn",
           shadow.col = "darkgray",
           main = "Divorce Prediction Decision Tree",
           sub = sprintf("Best Accuracy = %.2f%%", 100 * best_accuracy))

# Print rules
cat("\n--- Extracted Rules ---\n")
rpart.rules(best_model, 
            style = "tall", 
            cover = TRUE, 
            eq = "==", 
            when = "IF", 
            and = "&&")
