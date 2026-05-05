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
# Read data from CSV (tab or space separated)
filename <- "../data/2020_covid-19.tab"
data <- read.csv(file = filename, sep = " ", header = TRUE)

# Convert all columns to categorical factors.
# Decision trees often require factors for classification tasks.
data[] <- lapply(data, as.factor)

#------------------------
# 2. EXPLORATORY DATA ANALYSIS (EDA)
#------------------------
# Folder to save images
images_folder <- "../data/images/2020-covid-19/"

# Plot the relation between the target variable and the rest of the columns
plot.data.distribution(data, target = "TARGET", folder = images_folder)

# Plot the frequency of each attribute
plot.data.frequency(data, folder = images_folder)

#---------------------------------------------------
# 3. GENERATE DECISION TREE MODEL
#---------------------------------------------------
# Percentage of training examples
training_p <- 0.75

# Generate data partition 75% training / 25% test. The result is a vector with
# the indexes of the examples that will be used for the training of the model.
training_indexes <- createDataPartition(y = data$TARGET, p = training_p, list = FALSE)

# Split training and test data
training_data <- data[training_indexes, ]
test_data     <- data[-training_indexes, ]

# Create Decision Tree Model using training data. 
# Formula 'TARGET ~ .' means use all other columns to predict the TARGET.
model <- rpart(formula = TARGET ~ ., data = training_data)

# Make predictions using the unseen test data
prediction <- predict(model, test_data, type = "class")

#---------------------------------------------------
# 4. MODEL EVALUATION & VISUALIZATION
#---------------------------------------------------
# Calculate accuracy using a Confusion Matrix
# table(Actual, Predicted) -> Rows = Actual truth, Columns = Predictions
prediction_results <- table(test_data$TARGET, prediction)
confusion_mat <- confusionMatrix(prediction_results)

# Safely extract Accuracy by its proper name
accuracy <- confusion_mat$overall["Accuracy"]

# Precision(C): penalizes False Positives (FP)
# -> Of all samples predicted as class C, how many are actually C?
precision_per_class <- diag(prediction_results) / colSums(prediction_results)
# Recall(C): penalizes False Negatives (FN)
# -> Of all real samples of class C, how many are correctly predicted?
recall_per_class <- diag(prediction_results) / rowSums(prediction_results)

# Print overall accuracy
cat(sprintf("\nModel Accuracy = %.2f%%\n", 100 * accuracy))

# Print metrics for each specific class
cat("\nMetrics per Class:\n")
for (class_name in names(precision_per_class)) {
  # Handle potential NaN (0/0) if a class was never predicted or has no actual cases
  prec_val <- ifelse(is.nan(precision_per_class[class_name]), 0, 100 * precision_per_class[class_name])
  rec_val  <- ifelse(is.nan(recall_per_class[class_name]), 0, 100 * recall_per_class[class_name])
  
  cat(sprintf("  Class '%s' -> Precision: %.2f%% | Recall: %.2f%%\n", 
              class_name, prec_val, rec_val))
}

# Print attributes in order of relevance
attrs <- names(model$variable.importance)
cat("\nAttributes ordered by relevance:\n  ", paste(attrs, collapse = "\n   "), "\n")

# Plot the tree.
rpart.plot(model,
           type = 2,
           extra = 102, # Display classification rate and % of observations
           tweak = 1.1, # Text size tweak
           box.palette = "BuGn",
           shadow.col = "darkgray",
           main = "COVID-19 Decision Tree",
           sub = sprintf("Accuracy = %.2f%%", 100 * accuracy))

# Print the readable rules that represent the Decision Tree
cat("\n--- Extracted Rules ---\n")
rpart.rules(model,
            style = "tall",
            cover = TRUE,
            eq = "==",
            when = "IF",
            and = "&&",
            extra = 4)
