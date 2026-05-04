local.beam.search <- function(problem,
                              max_iterations = 100,
                              count_print = 5,
                              beams = 0) {
  name_method      <- "Beam Search"
  state_initial    <- problem$state_initial
  actions_possible <- problem$actions_possible
  
  # Inicializar beams
  vector <- vector("list", beams)
  for (i in 1:beams) {
    random_state <- sample(state_initial)
    vector[[i]] <- list(state = random_state,
                        evaluation = get.evaluation(random_state, problem))
  }
  
  # Ordenar inicialmente para que vector[[1]] sea el mejor
  evals_init <- sapply(vector, function(x) x$evaluation)
  vector <- vector[order(evals_init)]
  
  print(paste0("* START: ", name_method), quote = FALSE)
  start_time <- Sys.time()
  
  rep_iteration <- numeric(max_iterations)
  rep_eval_current <- numeric(max_iterations)
  
  count <- 1
  end_reason <- NULL
  
  while (count <= max_iterations) {
    node_current <- vector[[1]]
    
    # 1. Check Goal
    if (!is.null(problem$state_final) && is.final.state(node_current$state, problem$state_final, problem)) {
      end_reason <- "Solution"
      break
    }
    
    if (count %% count_print == 0) {
      print(paste0("Iteration: ", count, ", best eval=", node_current$evaluation), quote = FALSE)
    }

    # Registrar la mejor evaluación actual
    rep_iteration[count] <- count
    rep_eval_current[count] <- node_current$evaluation
    
    sucesores <- list()
    
    # 2. Expandir cada beam
    for (v in vector) {
      successor_nodes <- local.expand.node(v, actions_possible, problem)
      
      if (length(successor_nodes) > 0) {
        evals <- sapply(successor_nodes, function(x) x$evaluation)
        best_idx <- which.min(evals)
        sucesores <- append(sucesores, list(successor_nodes[[best_idx]]))
      }
    }
    
    # Dead-end check (No successors generated)
    if (length(sucesores) == 0) {
      end_reason <- "Dead_End"
      break
    }
    
    # 3. Find Best Successor
    evals_suc <- sapply(sucesores, function(x) x$evaluation)
    best_suc_idx <- which.min(evals_suc)
    node_best_successor <- sucesores[[best_suc_idx]]
    
    # 4. Move Decision (Strict improvement)
    if (node_best_successor$evaluation < node_current$evaluation) {
      # Selección elitista
      candidatos <- c(vector, sucesores)
      evals_cand <- sapply(candidatos, function(x) x$evaluation)
      order_idx <- order(evals_cand)
      vector <- candidatos[order_idx[1:beams]]
    } else {
      # Stop: Local Maximum or Plateau reached
      end_reason <- "Local_Best"
      break
    }
    
    count <- count + 1
  }
  
  if (is.null(end_reason)) {
    end_reason <- "Iterations"
  }
  
  end_time <- Sys.time()
  
  valid_idx <- rep_iteration > 0
  report <- data.frame(Iteration    = rep_iteration[valid_idx],
                       Eval_Current = rep_eval_current[valid_idx])
  
  result <- list()
  result$name       <- name_method
  result$runtime    <- end_time - start_time
  result$state_initial <- state_initial
  result$node_final <- vector[[1]]
  result$report     <- report
  result$end_reason <- end_reason
  
  if (end_reason == "Solution") {
    print("Solution found (Global Optimum)!", quote = FALSE)
  } else if (end_reason == "Local_Best") {
    print("Local best found (Optimization stopped).", quote = FALSE)
  } else {
    print(paste0("Stopped: ", end_reason), quote = FALSE)
  }
  
  print(paste0(
    "Final State: ",
    to.string(state = vector[[1]]$state, problem = problem)
  ), quote = FALSE)
  print(paste0("* END: ", name_method), quote = FALSE)
  
  return(result)
}