# =========================================================================

random.restart.hill.climbing <- function(file,
                                         max_iterations = 100,
                                         count_print = 5,
                                         restarts = 0) {
  name_method <- "Random Restart Hill Climbing"
  
  
  contador <- 0
  maximoGlobal <- NULL
  
  while (contador < restarts) {
    problem <- initialize.problem(file)
    result_hill_climbing <- hill.climbing.search(problem, max_iterations, count_print)
    evaluation_hill_climbing <- result_hill_climbing$node_final$evaluation
    
    if (is.null(maximoGlobal)) {
      print("Asignado por ser nulo")
      print(result_hill_climbing$state_initial)
      maximoGlobal <- result_hill_climbing
      print(maximoGlobal$node_final$state)
    } else if (maximoGlobal$node_final$evaluation > evaluation_hill_climbing) {
      print("Actualizacion por mas optimo")
      print(maximoGlobal$node_final$state)
      maximoGlobal <- result_hill_climbing
      print(maximoGlobal$node_final$state)
    }
    
    contador <- contador + 1
    
  }
  return(maximoGlobal)
}