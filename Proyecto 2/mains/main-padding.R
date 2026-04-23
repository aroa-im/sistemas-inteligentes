# =========================================================================
# Main Script Template - Local Search Algorithms
# =========================================================================
# This script shows the typical workflow to test LOCAL SEARCH algorithms.
#
# Key features:
#   - Runs algorithms multiple times (stochastic nature).
#   - Re-initializes the problem in each run to get random starting states.
#   - Collects all runs into a single results structure for analysis.
# -------------------------------------------------------------------------
#
# Authors / Maintainers:
#   - Roberto Carballedo
#   - Fernando Boto
#   - Enrique Onieva
#
# Last updated: February 2026
# Code generated & revised using: Gemini Pro.
#
# Educational use only — University of Deusto
# =========================================================================

# -------------------------------------------------------------------------
# 1) Clear environment and console
# -------------------------------------------------------------------------
rm(list = ls()) # Clear all variables
cat("\014")     # Clear console (RStudio)
graphics.off()  # Close all plots

# -------------------------------------------------------------------------
# 2) Working directory (IMPORTANT)
# -------------------------------------------------------------------------
if (requireNamespace("rstudioapi", quietly = TRUE)) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}

# -------------------------------------------------------------------------
# 3) Load algorithm utilities
# -------------------------------------------------------------------------
source("../algorithms/blind/expand-node.R")

# -------------------------------------------------------------------------
# 4) Load Local Search Algorithms
# -------------------------------------------------------------------------
# BASE ALGORITHM:
source("../algorithms/informed/hill-climbing-search.R")
#source("../algorithms/informed/stochastic-hill-climbing.R")
source("../algorithms/informed/random-restart-hill-climbing.R")
#source("../algorithms/informed/stochastic-random-restart.R")
#source("../algorithms/informed/beam-search.R")

# -------------------------------------------------------------------------
# 5) Load result analysis utilities
# -------------------------------------------------------------------------
source("../algorithms/results-analysis/analyze-results.R")

# -------------------------------------------------------------------------
# 6) Load the problem definition
# -------------------------------------------------------------------------
source("../problems/padding-problem.R")

# -------------------------------------------------------------------------
# 7) solve.instance()
# -------------------------------------------------------------------------
# PURPOSE:
#   Executes Local Search algorithms multiple times on a problem instance.
#
# PARAMETERS:
#   file           : path to the instance file
#   times          : NUMBER OF RUNS per algorithm
#   max_iterations : maximum steps allowed before stopping
#   count_print    : iterations interval to print progress
#   export_csv     : TRUE/FALSE, export summary table to CSV
#   csv_dir        : output directory for CSV file
#   verbose        : TRUE/FALSE, prints extra main script info
#   ...            : extra parameters for initialize.problem()
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# 7) solve.instance()
# -------------------------------------------------------------------------
solve.instance <- function(file,
                           times = 10,
                           max_iterations = 500,
                           count_print = 100,
                           export_csv = TRUE,
                           csv_dir = "../results",
                           verbose = TRUE,
                           ...) {
  
  # 1. Create results folder if needed
  if (export_csv && !dir.exists(csv_dir)) {
    dir.create(csv_dir, recursive = TRUE)
  }
  
  # 2. Initial Setup (Metadata only)
  temp_prob <- initialize.problem(file = file, ...)
  instance_name <- basename(file)
  
  # Prepare the header text to be used ONLY at the end
  header_text <- sprintf(
    "============================================================\n📂 LOCAL SEARCH on:  %s\n🧩 Problem Name:     %s\n🔁 Executions:       %d\n============================================================",
    instance_name, temp_prob$name, times
  )
  
  # Silent start (only a small indicator if verbose is TRUE)
  if (verbose) {
    cat(sprintf("\n⏳ Running all algorithms on '%s'. Please wait...\n", instance_name))
  }
  
  results_list <- list()
  summary_lines <- c()
  
  # =======================================================================
  # HELPER FUNCTION: Runs an algorithm silently and stores its summary
  # =======================================================================
  run_algorithm <- function(algo_func, custom_name, ...) {
    best_cost <- Inf
    worst_cost <- -Inf
    total_cost <- 0
    total_runtime <- 0
    
    for (i in 1:times) {
      # 1. We ALWAYS initialize a problem instance for internal metrics or basic algos
      problem_instance <- initialize.problem(file = file, ...)
      
      # 2. DECISION LOGIC: 
      # Check if the algorithm function uses 'file' or 'problem' as its first argument
      args_names <- names(formals(algo_func))
      
      if ("file" %in% args_names) {
        # For algorithms that manage their own restarts (like Random Restart)
        res <- algo_func(file = file, max_iterations = max_iterations, count_print = count_print, ...)
      } else {
        # For basic algorithms (Hill Climbing, Stochastic, etc.)
        res <- algo_func(problem = problem_instance, max_iterations = max_iterations, count_print = count_print, ...)
      }
      
      res$name <- custom_name
      current_cost <- res$node_final$evaluation
      
      # Track metrics
      if (!is.null(current_cost)) {
        if (current_cost < best_cost)  best_cost  <- current_cost
        if (current_cost > worst_cost) worst_cost <- current_cost
        total_cost <- total_cost + current_cost
      }
      
      total_runtime <- total_runtime + res$runtime
      results_list[[length(results_list) + 1]] <<- res
    }
    
    # Summary line formatting
    avg_runtime <- total_runtime / times
    mean_cost   <- total_cost / times
    line <- sprintf("▶️ %-25s | 🏆 Best: %8.2f | 📉 Worst: %8.2f | 📊 Mean: %8.2f | ⏱️ Avg. Time: %.3f s", 
                    custom_name, best_cost, worst_cost, mean_cost, avg_runtime)
    summary_lines <<- c(summary_lines, line)
  }
  
  # ---------------------------------------------------------
  # 3. Run Algorithms (Calls stay clean)
  # ---------------------------------------------------------
  
  # Basic Algorithms (They receive 'problem')
  # run_algorithm(hill.climbing.search, "Hill Climbing")
  #run_algorithm(stochastic.hill.climbing, "Stochastic HC")
  
  # Meta-heuristics (They receive 'file' automatically thanks to the fix above)
  run_algorithm(random.restart.hill.climbing, "Random Restart HC (5)", restarts = 5)
  #run_algorithm(random.restart.hill.climbing, "Random Restart HC (15)", restarts = 15)
  #run_algorithm(random.restart.hill.climbing, "Random Restart HC (15)", restarts = 30)
  
  # -- Stochastic Random Restart --
  #run_algorithm(stochastic.random.restart, "Stochastic RR HC (5)", restarts = 5)
  #run_algorithm(stochastic.random.restart, "Stochastic RR HC (10)", restarts = 15)
  #run_algorithm(stochastic.random.restart, "Stochastic RR HC (15)", restarts = 30)
  
  # -- Local Beam Search --
  #run_algorithm(local.beam.search, "Beam Search (3 beams)", beams = 3)
  #run_algorithm(local.beam.search, "Beam Search (5 beams)", beams = 5)
  #run_algorithm(local.beam.search, "Beam Search (10 beams)", beams = 10)
  
  # 4. Print the FULL summary at the very end
  # -------------------------------------------------------------
  if (verbose) {
    cat("\n", header_text, "\n", sep="")
    cat("📊 FINAL RESULTS SUMMARY\n")
    cat("============================================================\n")
    for (line in summary_lines) {
      cat(line, "\n")
    }
    cat("============================================================\n\n")
  }
  
  # 5. Analyze results
  # ---------------------------------------------------------
  results <- local.analyze.results(
    results = results_list,
    problem = temp_prob, 
    verbose = verbose,
    export_csv = export_csv,
    csv_dir = csv_dir
  )
  
  # 6. Visual Output (HTML Table)
  # ---------------------------------------------------------
  if (requireNamespace("kableExtra", quietly = TRUE) && requireNamespace("knitr", quietly = TRUE)) {
    print(kableExtra::kable_material(kableExtra::kbl(results, caption = paste("Local Search Stats:", temp_prob$name)),
                                     c("striped", "hover", "condensed")))
  }
}

# -------------------------------------------------------------------------
# 8) Experiment block
# -------------------------------------------------------------------------
#try(solve.instance(file = "../data/padding/basic.txt"))
#try(solve.instance(file = "../data/padding/trade.txt"))
try(solve.instance(file = "../data/padding/trap.txt"))
