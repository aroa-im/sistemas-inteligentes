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
# Read data from CSV
filename = "../data/2023_heart-disease.csv"
data <- read.csv(file = filename, sep =",", header = TRUE)

# Transform HeartDisease into NO and YES
data$HeartDisease <- ifelse(data$HeartDisease == 0, "NO (0)", "YES (1)")

#------------------------
# 2. EXPLORATORY DATA ANALYSIS (EDA)
#------------------------
images_folder <- "../data/images/2023-heart-disease/"
# Plot the relation between the target variable and the rest of the columns
plot.data.distribution(data, target = "HeartDisease", folder=images_folder)

#----------------------------------------
# 3. TRANSFORM NUMERIC COLUMNS INTO FACTORS
#----------------------------------------
# Create 4 bims for Age (based on lowest and highest values)
bin_width <- round((max(data$Age) - min(data$Age)) / 4)
labels <- paste0(min(data$Age) + (1:4 - 1) * bin_width, " <-> ", min(data$Age) + 1:4 * bin_width)
data$Age <- cut(data$Age, breaks = 4, labels = labels)
# Create 3 bims for RestingBP (Normal, High, Very High)
data$RestingBP <- ifelse(data$RestingBP < 100, "Normal (<100)",
                         ifelse(data$RestingBP <= 150, "High (>=100 && <=150)", "Very High (>150)"))
# Create 3 bims for MaxHR (Low, Normal, High)
data$MaxHR <- ifelse(data$MaxHR < 150, "Low (<=150)",
                     ifelse(data$MaxHR <= 180, "Normal (>=150 && <=180)", "High (>180)"))
# Create two values for Cholesterol (Normal, High, Very High)
data$Cholesterol <- ifelse(data$Cholesterol <= 200, "Normal (<=200)",
                           ifelse(data$Cholesterol < 300, "High (>=200 && <=300)", "Very High (>300)"))
# Transform FastingBS into NO and YES
data$FastingBS <- ifelse(data$FastingBS == 0, "NO (0)", "YES (1)")
# Transform ExerciseAngina into NO and YES
data$ExerciseAngina <- ifelse(data$ExerciseAngina == "N", "NO", "YES")
# Create bims for Oldpeak Define labels based on lowest and highest value of each bin
data$Oldpeak <- ifelse(data$Oldpeak < 0, "Negative (<0)",
                       ifelse(data$Oldpeak <= 2, "Normal-1 (>=0 && <=2)",
                              ifelse(data$Oldpeak <= 2.5, "Normal-2 (>=2.0 && <=2.5)", "High (>2.5)")))
#---------------------
# 4. PLOT DATA FREQUENCY
#---------------------
# Plot the frequency of each attribute
plot.data.frequency(data, folder=images_folder)

#---------------------------------------------------
# 5. GENERATE BEST DECISION TREE MODEL (20 Iterations)
#---------------------------------------------------
training_p <- 0.75
best_accuracy <- 0
best_model <- NULL
best_prediction_results <- NULL

cat("Starting model iterations to find the best fit...\n")

for (i in 1:20) {
  # Generate data partition 75% training / 25% test. The result is a vector with
  # the indexes of the examples that will be used for the training of the model.
  training_indexes <- createDataPartition(y = data$HeartDisease, p = training_p, list = FALSE)
  
  # Split training and test data
  training_data <- data[training_indexes, ]
  test_data     <- data[-training_indexes, ]
  
  # Create Decision Tree Model using training data.
  model <- rpart(formula = HeartDisease ~ ., 
                 data = training_data,
                 control = rpart.control(maxdepth = 5))
  
  # Make the prediction using the model and test data
  prediction <- predict(model, test_data, type = "class")
  
  # Calculate accuracy using Confusion Matrix
  prediction_results <- table(test_data$HeartDisease, prediction)
  confusion_mat <- confusionMatrix(prediction_results)
  accuracy <- confusion_mat$overall["Accuracy"]
  
  # Update best model if accuracy is better
  if (accuracy > best_accuracy) {
    best_accuracy <- accuracy
    best_model <- model
    best_prediction_results <- prediction_results
  }
  
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

# Plot tree (this method is slow, wait until pot is completed)
rpart.plot(best_model,
           type = 2,
           extra = 102,
           tweak = 1.1,
           box.palette = "BuGn",
           shadow.col = "darkgray",
           main = "Heart Disease Decision Tree",
           sub = paste0("Accuracy = ", round(100*best_accuracy, digits = 2), "%"))

# Print the rules that represent the Decision Tree
rpart.rules(best_model,
            style="tall",
            trace = 0,
            cover = TRUE,
            eq = "==",
            when = "IF",
            and = "&&",
            extra = 4)
