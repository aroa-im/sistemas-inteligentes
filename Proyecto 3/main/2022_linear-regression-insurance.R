# -----------------------------------------------------------------------------
# This code snippet was generated for educational purposes as part of the
# Intelligent Systems course at the University of Deusto.
#
# The code is released under the Creative Commons License and is provided
# for free use and modification by the programming and development community.
#
# This script demonstrates a basic Linear Regression pipeline: data prep,
# training, evaluation, and answering business questions.
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
library(lattice)
library(ggplot2)
library(caret)

# --- WORKSPACE CLEANUP ---
cat("\014") # Clear console
if(!is.null(dev.list())) dev.off() # Clear plots
rm(list=ls()) # Clean workspace variables

# Set working directory to the script's location
tryCatch({
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}, error = function(e) message("Warning: Save the script before running rstudioapi."))

# Load custom utility functions for plotting and printing
source("linear-regression-utils.R")

#-----------------------
# 1. READ AND PREPARE DATA
#-----------------------
data <- read.csv("../data/2022_insurance.csv")

# For this basic introductory model, we will drop non-numerical variables.
# (Note: In advanced ML, we use 'One-Hot Encoding' instead of deleting them).
data$sex <- NULL
data$region <- NULL

# Convert the binary categorical variable 'smoker' into numeric (1 = yes, 0 = no)
data$smoker[data$smoker == "yes"] <- 1
data$smoker[data$smoker == "no"]  <- 0
data$smoker <- as.numeric(data$smoker)

#----------------------
# 2. PRELIMINARY ANALYSIS
#----------------------
# Generate a summary table of the dataset
print.data.summary(data)
# Check how strongly each feature correlates with the target ('charges')
print.data.correlations(data, "charges")
# Visualize the relationship between features and the target variable
plot.data.distribution(data, "charges")

#-----------------------------------
# 3. GENERATE AND ANALYZE LINEAR MODEL
#-----------------------------------
# Initialize variables to track the best model during the loop
total_avg_error <- 0
best_model      <- NULL
best_test_data  <- NULL # We must save the test data associated with the best model
min_avg_error   <- Inf  # Start with infinity so any error will be smaller
training_p      <- 0.80 # 80% for training, 20% for testing

cat("\n--- Training Models ---\n")

# Repeat the training process 10 times to find the best random split
for (i in 1:10) {
  
  # Generate data partition (80% training / 20% test). 
  # Returns the row indexes for the training set.
  training_samples <- createDataPartition(y = data$charges, p = training_p, list = FALSE)
  
  # Split the dataset using the generated indexes
  training_data <- data[training_samples, ]
  test_data     <- data[-training_samples, ]
  
  # Train the Linear Model. Formula 'charges ~ .' means use all other columns to predict charges.
  current_model <- lm(formula = charges ~ ., data = training_data)
  
  # Make predictions on the unseen test data
  prediction <- predict(current_model, test_data)
  
  # Calculate Mean Absolute Error (MAE)
  mae <- mean(abs(prediction - test_data$charges))
  print(paste0("- Mean Absolute Error of model ", i, ": ", round(mae, 4)))
  
  # If this model is the best one so far, save it
  if (mae < min_avg_error) {
    min_avg_error  <- mae
    best_model     <- current_model
    best_test_data <- test_data # Save the test set used to evaluate this specific model
  }
  
  # Accumulate error to calculate the overall average later
  total_avg_error <- total_avg_error + mae
}

cat("\n--- Training Summary ---\n")
# Print overall metrics
print(paste0("- Average MAE across all 10 runs: ", round(total_avg_error / 10, 4)))
print(paste0("- Best Model MAE: ", round(min_avg_error, 4)))

# Print statistical summary of the BEST model found
print.model.summary(best_model)

#-----------------------------------
# 4. DEEP DIVE INTO THE BEST MODEL
#-----------------------------------
cat("\n--- Analyzing the Highest Error ---\n")

# Make predictions using the best model and its corresponding test data
best_predictions <- predict(best_model, best_test_data)

# Calculate individual errors for all instances in the test set
prediction_errors <- abs(best_predictions - best_test_data$charges)

# Find the specific patient (row) where the model failed the most
index_max_error <- which.max(prediction_errors)

# Print the data of that specific patient
print(best_test_data[index_max_error, ])

# Print the error details
print(paste0("Real charges: ", round(best_test_data$charges[index_max_error], 2),
             " | Prediction: ", round(best_predictions[index_max_error], 2),
             " | Difference (Error): ", round(prediction_errors[index_max_error], 2)))

# Show the Residual Plot
plot.model.residuals(test_data$charges, best_predictions, "Medical Charges")

#------------------------------
# 5. ANSWER THEORETICAL QUESTIONS
#------------------------------
cat("\n--- Business/Theoretical Scenarios ---\n")

# Q1: If all smokers stop smoking, how much would their charges be reduced on average?
data_smokers <- data[data$smoker == 1, ]
data_smokers_quit <- data_smokers
data_smokers_quit$smoker <- 0 # Simulate them quitting

pred_still_smoking <- predict(best_model, data_smokers)
pred_quit_smoking  <- predict(best_model, data_smokers_quit)

avg_reduction <- mean(pred_still_smoking - pred_quit_smoking)
print(paste0("- Average charge reduction if smokers quit: $", round(avg_reduction, 2)))

# Q2: Who are the 3 people whose charges will increase the most in 5 years?
data_in_5_years <- data
data_in_5_years$age <- data_in_5_years$age + 5 # Add 5 years to everyone's age

# Predict future charges
pred_future_charges <- predict(best_model, data_in_5_years)

# Calculate the difference between future predictions and current real charges
increase_in_charges <- pred_future_charges - data$charges

# Get the indexes of the top 3 highest increases
top_3_indexes <- tail(order(increase_in_charges), 3)

cat("\n- Top 3 patients with highest expected increase in 5 years:\n")
for (index in top_3_indexes) {
  print(paste0("Patient Index: ", index, 
               " | Current charges: $", round(data$charges[index], 2),
               " | Predicted new charges: $", round(pred_future_charges[index], 2),
               " | Increase: $", round(increase_in_charges[index], 2)))
}
