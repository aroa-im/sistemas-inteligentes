# =========================================================================

stochastic.random.restart <- function(file,
                                         max_iterations = 100,
                                         count_print = 5,
                                         restarts = 0) {
  name_method <- "Stochastic random restart"
  
  
  contador <- 0
  maximoGlobal <- NULL
  
  while (contador < restarts) {
    problem <- initialize.problem(file)
    result_Stochastic_hill_climbing <- stochastic.hill.climbing(problem, max_iterations, count_print)
    evaluation_Stochastic_hill_climbing <- result_Stochastic_hill_climbing$node_final$evaluation
    
    if (is.null(maximoGlobal)) {
      maximoGlobal <- result_Stochastic_hill_climbing
    } else if (maximoGlobal$node_final$evaluation > evaluation_Stochastic_hill_climbing) {
      maximoGlobal <- result_Stochastic_hill_climbing
    }
    
    contador <- contador + 1
    
  }
  return(maximoGlobal)
}