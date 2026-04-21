# -----------------------------------------------------------------------------
# Utility functions for the Linear Regression educational script.
# Provides clean tables and plots for exploratory data analysis and model evaluation.
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

# Load required packages for formatting tables and plotting
library(gridExtra)
library(kableExtra)
library(ggplot2)

# Prints a table with minimum, maximum, and mean values for each variable.
print.data.summary <- function(data) {
  
  # A much safer and R-idiomatic way to calculate stats than parsing text
  summary_df <- data.frame(
    Variable = names(data),
    Min  = sapply(data, min, na.rm = TRUE),
    Max  = sapply(data, max, na.rm = TRUE),
    Mean = sapply(data, mean, na.rm = TRUE),
    row.names = NULL
  )
  
  # Round the results for cleaner display
  summary_df$Min  <- round(summary_df$Min, 4)
  summary_df$Max  <- round(summary_df$Max, 4)
  summary_df$Mean <- round(summary_df$Mean, 4)
  
  # Render HTML table in Viewer
  print(kable(summary_df, caption = "Summary of Dataset Features",
              col.names = c("Variable", "MIN", "MAX", "MEAN")) %>%
          kable_styling(full_width = TRUE, position = "left"))
}

# Prints a table with the correlation between the target variable and the rest.
print.data.correlations <- function(data, target_variable) {
  
  # Calculate correlations for all columns against the target
  correlations <- sapply(data, function(col) round(cor(data[[target_variable]], col), 4))
  
  correlation_table <- data.frame(Variable = names(data),
                                  Correlation = correlations,
                                  row.names = NULL)
  
  # Format color: highlight strong correlations (>= 0.2 or <= -0.2) in green
  correlation_table$Color <- ifelse(abs(correlation_table$Correlation) >= 0.2, "green", "red")
  
  print(kable(correlation_table[, c("Variable", "Correlation")],
              caption = "Correlation to Target ('green' means good correlation, |r| >= 0.2)") %>%
          kable_styling(full_width = TRUE, position = "left") %>%
          column_spec(1, color = correlation_table$Color) %>%
          column_spec(2, color = correlation_table$Color))
}

# Plots the relationship of each independent variable against the target variable.
plot.data.distribution <- function(data, target_variable) {
  
  # Dynamic grid layout calculation
  num_variables <- ncol(data) - 1
  num_rows <- ceiling(sqrt(num_variables))
  num_cols <- ceiling(num_variables / num_rows)
  
  par(mfrow = c(num_rows, num_cols), mar = c(4, 4, 2, 1))
  
  # Create a scatter plot for each feature
  for (col_name in colnames(data)) {
    if (col_name != target_variable) {
      
      # We use suppressWarnings to silence the mathematical warnings (LOESS) 
      # when trying to draw trend lines on discrete/binary variables (like 'smoker').
      suppressWarnings({
        # Standard ML convention: Feature on X-axis, Target on Y-axis
        scatter.smooth(x = data[[col_name]],
                       y = data[[target_variable]],
                       xlab = col_name,
                       ylab = target_variable,
                       main = paste(col_name, "vs", target_variable),
                       col = "lightgreen",
                       lpars = list(col = "red", lwd = 2)) # Highlight the smooth line in red
      })
      
    }
  }
}

# Prints a formatted table with the model's coefficients and p-values.
print.model.summary <- function(model) {
  
  model_summary <- summary(model)
  
  # Print Adjusted R-squared (explains how much variance the model captures)
  print(paste0("- Adjusted R-squared: ", round(model_summary$adj.r.squared, 4)))
  
  # Extract coefficients and p-values safely
  coefficients_table <- data.frame(
    Variable    = rownames(model_summary$coefficients),
    Coefficient = round(model_summary$coefficients[, "Estimate"], 4),
    p_value     = round(model_summary$coefficients[, "Pr(>|t|)"], 4),
    row.names   = NULL
  )
  
  # Highlight statistically significant variables (p-value < 0.05) in green
  coefficients_table$Color <- ifelse(coefficients_table$p_value < 0.05, "green", "red")
  
  print(kable(coefficients_table[, c("Variable", "Coefficient", "p_value")],
              caption = "Model Coefficients ('green' means statistically significant, p < 0.05)") %>%
          kable_styling(full_width = TRUE, position = "left") %>%
          column_spec(1, color = coefficients_table$Color) %>%
          column_spec(2, color = coefficients_table$Color) %>%
          column_spec(3, color = coefficients_table$Color))
}

# -----------------------------------------------------------------------------
# NEW ADVANCED FUNCTIONS ADDED
# -----------------------------------------------------------------------------

# Plots the residuals (errors) of the model's predictions.
plot.model.residuals <- function(real_values, predictions, target_name = "Target") {
  
  # Create a temporary dataframe for plotting
  plot_data <- data.frame(
    Real = real_values,
    Residuals = real_values - predictions
  )
  
  # Create the residual plot using ggplot2
  p <- ggplot(plot_data, aes(x = Real, y = Residuals)) +
    geom_point(color = "purple", alpha = 0.6, size = 2) +
    geom_hline(yintercept = 0, color = "red", linewidth = 1.2, linetype = "dashed") +
    labs(title = paste("Residual Plot for", target_name),
         subtitle = "Are the errors randomly distributed?",
         x = paste("Real", target_name),
         y = "Prediction Error (Residuals)",
         caption = "Points far from the red dashed line are bad predictions.") +
    theme_minimal()
  
  print(p)
}

# Custom read.keel function (Replaces the obsolete RKEEL package)
# Reads a KEEL .dat file and loads the data into a standard data.frame.
read.keel <- function(file_path) {
  lines <- readLines(file_path)
  
  data_start <- grep("@data", lines, ignore.case = TRUE)
  attr_lines <- lines[grep("@attribute", lines, ignore.case = TRUE)]
  
  col_names <- sapply(strsplit(attr_lines, "\\s+"), function(x) x[2])
  data_lines <- lines[(data_start + 1):length(lines)]
  
  df <- read.csv(text = data_lines, header = FALSE, strip.white = TRUE)
  colnames(df) <- col_names
  
  return(df)
}