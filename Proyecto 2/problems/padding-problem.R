# =========================================================================
# Problem Formulation: Batch Padding Optimization
# =========================================================================
# Problem description
# -------------------
# Modern tensor processing hardware (e.g., GPUs used for LLM inference)
# requires inputs within a batch to have the same sequence length.
# When requests of different lengths are processed together, the
# shorter ones are padded with dummy tokens so that all sequences
# match the longest one in the batch.
#
# Objective
# ---------
# Assign N requests into batches of size B to minimize:
#   total_cost = total_padding + lambda * total_latency
#
# State representation
# --------------------
# A vector of size N where state[i] = batch identifier assigned to request i.
#
# Actions
# -------
# swap_i_j: exchanges the batches of request i and request j.
#
# =========================================================================
# Authors / Maintainers:
#   - Fernando Boto
#   - Roberto Carballedo
#
# Code generated using ChatGPT & revised with Gemini Pro.
# Last updated: March 2026
# Educational use only — University of Deusto
# =========================================================================

# =========================================================================
# initialize.problem(...)
# =========================================================================
initialize.problem <- function(file, ...) {
  
  # Read the problem instance from file
  data <- read.table(file)
  
  # First row contains global parameters
  N <- as.integer(data[1, 1])        # number of requests
  B <- as.integer(data[1, 2])        # batch size
  lambda <- as.numeric(data[1, 3])   # weight of latency
  
  # Remaining rows contain request information
  requests <- data[-1, ]
  colnames(requests) <- c("id", "length", "time")
  
  # Initial state: create batches with exactly B requests each
  state_initial <- rep(1:(N/B), each = B)
  
  # Shuffle requests to generate a random initial solution
  state_initial <- sample(state_initial)
  
  problem <- list()
  
  problem$name <- sprintf("Padding [%s] (N=%d, B=%d)", basename(file), N, B)
  problem$state_initial <- state_initial
  problem$state_final <- NULL   # every state is considered final
  
  # ------------------------------------------------------------
  # combn(N, 2) generates a matrix with all unique pairs (i, j)
  # ------------------------------------------------------------
  pairs <- combn(N, 2)
  actions <- paste("swap", pairs[1, ], pairs[2, ], sep = "_")
  
  # Using an atomic vector with the name of each action
  problem$actions_possible <- actions 
  
  # Additional problem parameters
  problem$N <- N                # number of requests
  problem$B <- B                # batch size
  problem$lambda <- lambda      # weight of latency in the objective function
  problem$requests <- requests  # data frame with request information
  
  return(problem)
}

# =========================================================================
# is.applicable(state, action, problem)
# =========================================================================
is.applicable <- function(state, action, problem) {
  # Parse indices from the action string
  parts <- strsplit(action, "_")[[1]]
  i <- as.integer(parts[2])
  j <- as.integer(parts[3])
  
  # An action is applicable if the two requests belong to different batches
  return(state[i] != state[j])
}

# =========================================================================
# effect(state, action, problem)
# =========================================================================
effect <- function(state, action, problem) {
  # Parse indices from the action string
  parts <- strsplit(action, "_")[[1]]
  i <- as.integer(parts[2])
  j <- as.integer(parts[3])
  
  # Create a copy of the state to modify
  new_state <- state
  # Swap the batch assignments of requests i and j
  new_state[c(i, j)] <- new_state[c(j, i)]
  
  return(new_state)
}

# =========================================================================
# is.final.state(state, final_state, problem)
# =========================================================================
is.final.state <- function(state, final_state, problem) {
  return(TRUE)
}

# =========================================================================
# to.string(state, problem)
# =========================================================================
to.string <- function(state, problem) {
  return(paste(state, collapse = "-"))
}

# =========================================================================
# get.cost(action, state, problem)
# =========================================================================
get.cost <- function(action, state, problem) {
  return(1)
}

# =========================================================================
# get.evaluation(state, problem)
# =========================================================================
get.evaluation <- function(state, problem) {
  lengths <- problem$requests$length
  times   <- problem$requests$time
  lambda  <- problem$lambda
  
  # ------------------------------------------------------------
  # Vectorized Evaluation using tapply.
  # It returns a vector of values for each batch, which we can then sum up.
  # ------------------------------------------------------------
  
  # Number of items per batch (should be equal to B, but we compute it for generality)
  batch_sizes <- tapply(lengths, state, length)
  
  # Padding computation: (max_len * size) - sum(lengths)
  max_len_per_batch <- tapply(lengths, state, max)
  sum_len_per_batch <- tapply(lengths, state, sum)
  padding_per_batch <- (batch_sizes * max_len_per_batch) - sum_len_per_batch
  
  # Latency computation: (max_time * size) - sum(times)
  max_time_per_batch <- tapply(times, state, max)
  sum_time_per_batch <- tapply(times, state, sum)
  latency_per_batch  <- (batch_sizes * max_time_per_batch) - sum_time_per_batch
  
  # Aggregate totals
  total_padding <- sum(padding_per_batch)
  total_latency <- sum(latency_per_batch)
  
  # Final objective value to minimize
  cost <- total_padding + (lambda * total_latency)
  
  return(cost)
}