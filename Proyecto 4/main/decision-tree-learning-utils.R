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
library(ggplot2)

# Creates a scatter plot of each attribute except the target and stores each
# plot as an image.
plot.data.distribution <- function(data, target, folder = "../data/images/") {
  
  # Ensure the directory exists to avoid errors when saving
  if (!dir.exists(folder)) {
    dir.create(folder, recursive = TRUE)
  }
  
  # Get the names of attributes except the "target"
  attributes <- setdiff(names(data), target)
  
  # Create a plot for each attribute and save it as a PNG image
  for (attribute in attributes) {
    plot <- ggplot(data, aes(x = .data[[attribute]], y = .data[[target]])) +
      geom_point(color = "dodgerblue", alpha = 0.7, size = 2) +
      theme_minimal() +
      labs(title = paste(attribute, "vs", target),
           x = attribute, 
           y = target)
    
    filename <- paste0(folder, "scatter-plot-", target, "-vs-", attribute, ".png")
    ggsave(filename, plot, width = 6, height = 4, dpi = 300)
  }
}

# Creates a bar chart with frequencies of each attribute and stores it as an image.
plot.data.frequency <- function(data, folder = "../data/images/") {
  
  # Ensure the directory exists to avoid errors when saving
  if (!dir.exists(folder)) {
    dir.create(folder, recursive = TRUE)
  }
  
  # Get the names of attributes
  attributes <- names(data)
  
  # Create a plot for each attribute and save it as a PNG image
  for (attribute in attributes) {
    plot <- ggplot(data, aes(x = .data[[attribute]], fill = .data[[attribute]])) +
      geom_bar() +
      geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.5, size = 4) +
      theme_minimal() +
      labs(title = paste("Frequency Distribution of", attribute),
           x = attribute, 
           y = "Frequency") +
      theme(legend.position = "none") # Hide redundant legend
    
    filename <- paste0(folder, "frequency-", attribute, ".png")
    ggsave(filename, plot, width = 8, height = 4, dpi = 300)
  }
}
