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
  
  print(paste0("* START: ", name_method), quote = FALSE)
  start_time <- Sys.time()
  
  count <- 1
  
  while (count <= max_iterations) {
    sucesores <- list()
    
    # Expandir cada beam
    for (v in vector) {
      successor_nodes <- local.expand.node(v, actions_possible, problem)
      
      if (length(successor_nodes) > 0) {
        evals <- sapply(successor_nodes, function(x)
          x$evaluation)
        best_idx <- which.min(evals)
        sucesores <- append(sucesores, list(successor_nodes[[best_idx]]))
      }
    }
    
    # Selección elitista
    candidatos <- c(vector, sucesores)
    evals <- sapply(candidatos, function(x)
      x$evaluation)
    order_idx <- order(evals)
    
    vector <- candidatos[order_idx[1:beams]]
    
    if (count %% count_print == 0) {
      print(paste0("Iteration: ", count, ", best eval=", vector[[1]]$evaluation),
            quote = FALSE)
    }
    
    count <- count + 1
  }
  
  end_time <- Sys.time()
  
  result <- list()
  result$name       <- name_method
  result$runtime    <- end_time - start_time
  result$state_initial <- state_initial
  result$node_final <- vector[[1]]
  result$end_reason <- "Iterations"
  
  print(paste0(
    "Final State: ",
    to.string(state = vector[[1]]$state, problem = problem)
  ), quote = FALSE)
  print(paste0("* END: ", name_method), quote = FALSE)
  
  return(result)
}