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
data <- read.csv(file="../data/2020_world_happiness.csv", sep=",", header = TRUE)

# Remove non-numerical columns
data$Country.or.region <- NULL

# CRITICAL FIX: Remove 'Overall.rank'. 
# We want to predict the continuous 'Score'. If we leave the rank in the dataset,
# the model will "cheat" (Target Leakage) because rank is a direct reflection of the score.
data$Overall.rank <- NULL

#----------------------
# PRELIMINARY ANALYSIS
#----------------------
# Print data summary
print.data.summary(data)
# Print data correlations
print.data.correlations(data, "Score")
# Plot data distribution
plot.data.distribution(data, "Score")

#-----------------------------------
# GENERATE AND ANALYZE LINEAR MODEL
#-----------------------------------
# Percentage of training examples
training_p <- 0.8

# Generate data partition (80% training / 20% test). 
training_samples <- createDataPartition(y = data$Score, p = training_p, list = FALSE)

# Split training and test data
training_data <- data[training_samples, ]
test_data     <- data[-training_samples, ]

# Create Linear Model predicting 'Score' using all other available features
model <- lm(formula = Score ~ ., data = training_data)

# Make predictions using the test data
prediction <- predict(model, test_data)

# Calculate Mean Absolute Error (MAE)
mean_avg_error <- mean(abs(prediction - test_data$Score))

# Print Mean Absolute Error
print(paste0("- Mean Absolute Error: ", round(mean_avg_error, 4)))

# Print summary of the model
summary(model)
print.model.summary(model)

# Show the Residual Plot
plot.model.residuals(test_data$Score, prediction, "Happiness Score")
