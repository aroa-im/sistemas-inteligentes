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

# Install required packages
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

# Load custom functions
source("linear-regression-utils.R")

#-----------------------
# READ AND PREPARE DATA
#-----------------------
# Read data from CSV
filename = "../data/2021_loiu_2000-01_2021-03.csv"
data <- read.csv(file=filename, sep=",", header = TRUE)

# Remove non-numerical or irrelevant columns for this basic model
data$fecha <- NULL
data$indicativo <- NULL
data$nombre <- NULL
data$provincia <- NULL
data$altitud <- NULL
data$horatmin <- NULL
data$horatmax <- NULL
data$horaracha <- NULL
data$sol <- NULL
data$horaPresMax <- NULL
data$horaPresMin <- NULL

#----------------------
# PRELIMINARY ANALYSIS
#----------------------
# Print data summary
print.data.summary(data)
# Print data correlations
print.data.correlations(data, "tmed")
# Plot data distribution
plot.data.distribution(data, "tmed")

#-----------------------------------
# GENERATE AND ANALYZE LINEAR MODEL
#-----------------------------------
# Percentage of training examples
training_p <- 0.80

# Generate data partition (80% training / 20% test).
training_indexes <- createDataPartition(y = data$tmed, p = training_p, list = FALSE)

# Split training and test data
training_data <- data[training_indexes, ]
test_data     <- data[-training_indexes, ]

# Create Linear Model using training data. Formula 'tmed ~ .' means use all other columns.
model <- lm(formula = tmed ~ ., data = training_data)

# Make predictions using the test data
prediction <- predict(model, test_data)

# Calculate Mean Absolute Error (MAE)
mean_avg_error <- mean(abs(prediction - test_data$tmed))

# Print Mean Absolute Error
print(paste0("- Mean Absolute Error: ", round(mean_avg_error, 4)))

# Print summary of the model
summary(model)
print.model.summary(model)

# Show the Residual Plot
plot.model.residuals(test_data$tmed, prediction, "Average Temperature (tmed)")
