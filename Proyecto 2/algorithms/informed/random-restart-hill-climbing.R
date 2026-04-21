# =========================================================================

random.restart.hill.climbing <- function(file,
                                 max_iterations = 100,
                                 count_print = 5, restarts= 0) {
  
  name_method <- "Random Restart Hill Climbing"
  
  problem <- initialize.problem(file)
  contador <- 0
  
  while (contador < restarts) {
    contador<- contador +1
    print(contador)
  }
  
  
}