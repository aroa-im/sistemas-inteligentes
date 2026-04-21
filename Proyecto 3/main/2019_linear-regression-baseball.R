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
library(lattice)
library(caret)
library(ggplot2)
library(gridExtra)
library(kableExtra)

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
# Load data
data <- read.keel("../data/2019_baseball.dat")

# Convert Salary from string to numeric (standard R approach)
data$Salary <- as.numeric(data$Salary)

# Remove non-numerical categorical columns for this basic model
data$Free_agency_eligibility <- NULL
data$Free_agent <- NULL
data$Arbitration_eligibility <- NULL
data$Arbitration <- NULL

#----------------------
# PRELIMINARY ANALYSIS
#----------------------
# Print data summary
print.data.summary(data)
# Print data correlations
print.data.correlations(data, "Salary")
# Plot data distribution
plot.data.distribution(data, "Salary")

#-----------------------------------
# GENERATE AND ANALYZE LINEAR MODEL
#-----------------------------------
# Percentage of training examples
training_p <- 0.8

# Generate data partition (80% training / 20% test).
training_indexes <- createDataPartition(y = data$Salary, p = training_p, list = FALSE)

# Split training and test data
training_data <- data[training_indexes, ]  
test_data     <- data[-training_indexes, ] 

# Create Linear Model using training data. Formula 'Salary ~ .' means use all other columns.
model <- lm(formula = Salary ~ ., data = training_data)

# Make predictions using the unseen test data
prediction <- predict(model, test_data)

# Calculate Mean Absolute Error (MAE)
mean_avg_error <- mean(abs(prediction - test_data$Salary))

# Print Mean Absolute Error
print(paste0("- Mean Absolute Error: ", round(mean_avg_error, 4)))

# Print summary of the model using native R and our custom utility
summary(model)
print.model.summary(model)

# Show the Residual Plot
plot.model.residuals(test_data$Salary, prediction, "Player Salary")
