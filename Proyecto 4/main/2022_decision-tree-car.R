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
filename = "../data/2022_car.csv"
data <- read.csv(file = filename, sep =",", header = TRUE)

#------------------------
# 2. EXPLORATORY DATA ANALYSIS (EDA)
#------------------------
images_folder <- "../data/images/2022-car-seller-type/"
# Plot the relation between the target variable and the rest of the columns
plot.data.distribution(data, target = "seller_type", folder=images_folder)

#----------------------------------------
# 3. TRANSFORM NUMERIC COLUMNS INTO FACTORS
#----------------------------------------
# YEAR
index1 <- data$year <= 2005
index2 <- data$year > 2005 & data$year <= 2010
index3 <- data$year > 2010 & data$year <= 2015
index4 <- data$year > 2015
data$year[index1] <- "<= 2005"
data$year[index2] <- "2006 - 2010"
data$year[index3] <- "2011 - 2015"
data$year[index4] <- "> 2015"

# SELLING PRICE
data$selling_price <- as.numeric(data$selling_price / 100)
index1 <- data$selling_price > 10000
index2 <- data$selling_price >= 7500 & data$selling_price <= 10000
index3 <- data$selling_price >= 5000 & data$selling_price < 7500
index4 <- data$selling_price < 5000 & data$selling_price >= 2000
index5 <- data$selling_price < 2500
data$selling_price[index1] <- "> 10,000"
data$selling_price[index2] <- "7,500 - 10,000"
data$selling_price[index3] <- "5,000 - 7,499"
data$selling_price[index4] <- "2,500 - 4,999"
data$selling_price[index5] <- "< 2,500"

# KM DRIVEN
index1 <- data$km_driven > 100000
index2 <- data$km_driven >= 75000 & data$km_driven <= 100000
index3 <- data$km_driven >= 50000 & data$km_driven < 75000
index4 <- data$km_driven >= 25000 & data$km_driven < 50000
index5 <- data$km_driven < 25000
data$km_driven[index1] <- "> 100,000"
data$km_driven[index2] <- "75,000 - 100.000"
data$km_driven[index3] <- "50.000 - 74,999"
data$km_driven[index4] <- "25,000 - 49,999"
data$km_driven[index5] <- "< 25,000"

# FUEL
data <- subset(data, fuel != as.factor("Electric"))

data$name = NULL

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
  training_indexes <- createDataPartition(y = data$seller_type, p = training_p, list = FALSE)
  
  # Split training and test data
  training_data <- data[training_indexes, ]  # Extract training data using training_indexes
  test_data     <- data[-training_indexes, ] # Extract data with the indexes not included in training_indexes
  
  # Create Decision Tree Model using training data.
  model <- rpart(formula = seller_type ~ ., data = training_data)
  
  # Make the prediction using the model and test data
  prediction <- predict(model, test_data, type = "class")
  
  # Calculate accuracy using Confusion Matrix
  prediction_results <- table(test_data$seller_type, prediction)
  confusion_mat <- confusionMatrix(prediction_results)
  accuracy <- confusion_mat$overall["Accuracy"]
  
  # Update best model if accuracy is better
  if (accuracy > best_accuracy) {
    best_accuracy <- accuracy
    best_model <- model
    best_prediction_results <- prediction_results
  }
  
  # Optional: Print progress
  cat(sprintf("  Iteration %02d: Current Accuracy = %.2f%%\n", i, 100 * accuracy))
}

#---------------------------------------------------
# 6. MODEL EVALUATION & VISUALIZATION
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
           main = "Car Seller Type Decision Tree",
           sub = sprintf("Best Accuracy = %.2f%%", 100 * best_accuracy))

# Print the readable rules that represent the Decision Tree
cat("\n--- Extracted Rules ---\n")
rpart.rules(best_model,
            style = "tall",
            cover = TRUE,
            eq = "==",
            when = "IF",
            and = "&&")
