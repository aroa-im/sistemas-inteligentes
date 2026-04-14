# =========================================================================
# Problem Template
# =========================================================================
# This file defines the problem interface required by the search algorithms.
#
# Each problem must implement the functions below using the EXACT same
# headers (do NOT modify them). The search algorithms are generic. They do
# not "know" the problem domain, so they rely on this interface as a contract.
#   - is.applicable()  : checks whether an action can be applied
#   - effect()         : returns the successor state
#   - is.final.state() : checks whether a state satisfies the goal condition
#   - to.string()      : unique state identifier (used in Graph Search)
#   - get.cost()       : step cost (used by UCS / A*)
#   - get.evaluation() : heuristic h(n) (used by Greedy / A*)
#
# -------------------------------------------------------------------------
# IMPORTANT DESIGN NOTES
# -------------------------------------------------------------------------
# 1) Actions representation
#    - Recommended (simple): actions_possible as an atomic vector
#        Example: c("left","right","up","down")
#      This makes the code simpler because actions are used directly by
#      is.applicable(), effect(), get.cost() and get.evaluation().
#
#    - Advanced: actions_possible as a data.frame/matrix (one row per action)
#      Use this only when each action needs multiple attributes (e.g., name + cost).
#        Example: data.frame(move=c("left","right"), cost=c(1,2))
#
# 2) State representation (critical for Graph Search correctness)
#    - The state MUST include ALL variables that affect future behaviour:
#        * which actions are applicable
#        * the successor state
#        * the action cost
#        * the heuristic evaluation
#    - If the state is incomplete, Graph Search may prune incorrectly and
#      return wrong results (suboptimal solution or even no solution found).
#
# 3) Determinism requirement
#    - Given the same (state, action), the functions must always return
#      the same output.
#    - Randomness must be handled outside these functions (e.g., inside
#      initialize.problem()).
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
calculate.position <- function(state, action, problem) {
  new_state <- state
  
  # Diferenciales de movimiento normal
  delta <- list(
    N = c(-1, 0),
    S = c(1, 0),
    E = c(0, 1),
    O = c(0, -1),
    NE = c(-1, 1),
    NO = c(-1, -1),
    SE = c(1, 1),
    SO = c(1, -1)
  )
  
  # Movimiento normal
  if (!is.null(delta[[action]])) {
    new_state$pos <- new_state$pos + delta[[action]]
    return(new_state)
  }
  
  # Construir la estructura por provider
  rag_by_provider <- list()
  for (link in problem$rag_links) {
    provider <- link$provider
    if (is.null(rag_by_provider[[provider]])) {
      rag_by_provider[[provider]] <- list(link)
    } else {
      rag_by_provider[[provider]] <- c(rag_by_provider[[provider]], list(link))
    }
  }
  
  # Si existe el provider, buscar si alguna link coincide con la posición actual
  links <- rag_by_provider[[action]]
  if (!is.null(links)) {
    link_match <- NULL
    for (l in links) {
      if (all(l$from == state$pos)) {
        link_match <- l
        break
      }
    }
    if (!is.null(link_match)) {
      new_state$pos <- link_match$to
    }
  }
  
  return(new_state)
}
# =========================================================================

# =========================================================================
# PURPOSE:
#   Builds and returns the "problem" object, which contains everything the
#   algorithms need to run on this specific instance.
#
# INPUT:
#   ... : optional parameters passed from the main script.
#         Typical examples:
#           - file / filename : instance file to load
#
# OUTPUT:
#   problem (list) with compulsory fields:
#     - problem$name
#     - problem$state_initial
#     - problem$state_final
#     - problem$actions_possible
#
# NOTES:
#   - Search algorithms DO NOT call this function. The main script does.
#   - actions_possible is usually a simple vector of actions (recommended),
#     but can also be a data.frame/matrix if actions need multiple attributes.
# =========================================================================
initialize.problem <- function(file) {
  # Comprobar que el archivo existe
  if (!file.exists(file)) {
    stop(paste0("File not found: ", file))
  }
  
  # Inicializar lista donde se guardará todo el problema
  problem <- list()
  
  # Leer archivo línea por línea
  # warn = FALSE evita el warning de "incomplete final line"
  lines <- readLines(file, warn = FALSE)
  
  # Eliminar espacios al inicio y final de cada línea
  lines <- trimws(lines)
  
  # Eliminar líneas vacías
  lines <- lines[lines != ""]
  
  # Variable para saber en qué sección estamos ([PARAMS], [MAP], etc.)
  section <- ""
  
  # Variables auxiliares para ir guardando la info del fichero
  GEN_LAT <- NULL
  SWITCH_LAT <- NULL
  EUR_PER_SEC <- NULL
  providers <- list()
  map_lines <- c()
  rag_links <- list()
  
  # Recorrer cada línea del archivo
  for (line in lines) {
    # Detectar si la línea es una cabecera de sección
    # Ejemplo: [PARAMS], [MAP], [RAG_LINKS]
    if (grepl("^\\[.*\\]$", line)) {
      section <- line
      next  # Pasar a la siguiente línea
    }
    
    # ---------------- PARAMS ----------------
    # Leer parámetros generales del problema
    if (section == "[PARAMS]") {
      # Latencia de generación
      if (startsWith(line, "GEN_LAT")) {
        GEN_LAT <- as.numeric(sub("GEN_LAT=", "", line))
      }
      
      # Latencia de cambio de proveedor
      else if (startsWith(line, "SWITCH_LAT")) {
        SWITCH_LAT <- as.numeric(sub("SWITCH_LAT=", "", line))
      }
      
      # Coste por segundo
      else if (startsWith(line, "EUR_PER_SEC")) {
        EUR_PER_SEC <- as.numeric(sub("EUR_PER_SEC=", "", line))
      }
      
      # Definición de proveedores
      # Formato: PROVIDER=Nombre;LAT=x;COST=y
      else if (startsWith(line, "PROVIDER")) {
        parts <- strsplit(sub("PROVIDER=", "", line), ";")[[1]]
        
        name <- parts[1]  # Nombre del proveedor
        lat  <- as.numeric(sub("LAT=", "", parts[2]))   # Latencia
        cost <- as.numeric(sub("COST=", "", parts[3]))  # Coste
        
        # Guardar en lista con nombre como clave
        providers[[name]] <- list(lat = lat, cost = cost)
      }
    }
    
    # ---------------- MAP ----------------
    # Leer el mapa del laberinto
    else if (section == "[MAP]") {
      # Ignorar líneas de dimensiones (no forman parte del mapa)
      if (startsWith(line, "ROWS") || startsWith(line, "COLS"))
        next
      
      # Eliminar espacios/tabs y guardar línea limpia
      line_clean <- gsub("\\s+", "", line)
      if (line_clean != "")
        map_lines <- c(map_lines, line_clean)
    }
    
    # ---------------- RAG_LINKS ----------------
    # Leer conexiones especiales entre puntos del mapa
    else if (section == "[RAG_LINKS]") {
      parts <- strsplit(line, ";")[[1]]
      
      rag_links[[length(rag_links) + 1]] <- list(
        provider = parts[1],
        # proveedor asociado
        from = as.numeric(strsplit(parts[2], ",")[[1]]),
        # origen (fila, col)
        to   = as.numeric(strsplit(parts[3], ",")[[1]])   # destino (fila, col)
      )
    }
  }
  
  # Convertir el mapa de texto a matriz
  # Cada celda será ".", "#", "S" o "G"
  map_matrix <- do.call(rbind, strsplit(map_lines, ""))
  
  # Obtener dimensiones del mapa
  n_rows <- nrow(map_matrix)
  n_columns <- ncol(map_matrix)
  
  # Buscar posición inicial (S)
  state_initial <- which(map_matrix == "S", arr.ind = TRUE)
  state_initial <- as.numeric(state_initial)  # convertir a vector
  
  # Buscar posición objetivo (G)
  state_final   <- which(map_matrix == "G", arr.ind = TRUE)
  state_final <- as.numeric(state_final)  # convertir a vector
  
  # ---------------------------------------------------------
  # COMPULSORY ATTRIBUTES (must be defined in each problem)
  # ---------------------------------------------------------
  problem$name              <- paste0("Laberinto - [", basename(file), "]")
  
  # Estado inicial (posición de S)
  problem$state_initial     <- list(pos = as.numeric(state_initial), mode = "GEN")
  
  # Estado final (posición de G)
  problem$state_final       <- as.numeric(state_final)
  
  # Estado actual (posición actual)
  problem$current_state       <- problem$state_initial  # ahora coincide siempre
  
  # Acciones posibles del agente
  rag_names <- sapply(rag_links, function(x)
    x$provider)
  problem$actions_possible <- c("N", "S", "E", "O", "NE", "NO", "SE", "SO", rag_names)
  
  # ---------------------------------------------------------
  # OPTIONAL / ADDITIONAL ATTRIBUTES
  # ---------------------------------------------------------
  # Any extra information needed by the problem can be stored here:
  # problem$map, problem$obstacles, problem$params, ...
  
  # Guardar mapa completo
  problem$map <- map_matrix
  
  # Guardar parámetros globales
  problem$params <- list(GEN_LAT = GEN_LAT,
                         SWITCH_LAT = SWITCH_LAT,
                         EUR_PER_SEC = EUR_PER_SEC)
  
  # Guardar proveedores
  problem$providers <- providers
  
  # Guardar enlaces especiales
  problem$rag_links <- rag_links
  
  # Guardar dimensiones
  problem$n_columns <- n_columns
  problem$n_rows <- n_rows
  
  # ---------------------------------------------------------
  # IMPORTANT: STATE REPRESENTATION (Graph Search correctness)
  # ---------------------------------------------------------
  # The state MUST contain ALL variables that affect:
  #   - which actions are applicable
  #   - the successor state
  #   - action costs
  #   - heuristic evaluation
  # Otherwise, Graph Search may incorrectly treat different states as equal.
  
  return(problem)
}

# =========================================================================
# is.applicable(state, action, problem)
# =========================================================================
# PURPOSE:
#   Returns TRUE if the action can be legally applied in the given state.
#
# INPUT:
#   state   : current state
#   action  : one action (usually a single value, e.g. "left", 3, etc.)
#   problem : problem definition
#
# OUTPUT:
#   TRUE  -> action can be applied
#   FALSE -> action is not allowed in this state
#
# IMPORTANT:
#   This function MUST be deterministic:
#   given the same (state, action), it must always return the same result.
# =========================================================================
is.applicable <- function(state, action, problem) {
  result <- FALSE
  pos <- state$pos
  map <- problem$map
  new_pos <- calculate.position(state, action, problem)$pos
  if (new_pos[1] > 0 &&
      new_pos[1] <= problem$n_rows &&
      new_pos[2] > 0 &&
      new_pos[2] <= problem$n_columns &&
      map[new_pos[1], new_pos[2]] != "#" &&
      !all(new_pos == state$pos)) {
    result <- TRUE
  }
  
  return(result)
}

# =========================================================================
# effect(state, action, problem)
# =========================================================================
# PURPOSE:
#   Applies the action to the state and returns the successor state.
#
# INPUT:
#   state   : current state
#   action  : action to apply
#   problem : problem definition
#
# OUTPUT:
#   successor state (same representation type as 'state')
#
# IMPORTANT:
#   - This function must NOT modify the original state in-place. Return a NEW state object instead.
#   - It must also be deterministic.
# =========================================================================
effect <- function(state, action, problem) {
  result <- state
  all_actions <- problem$actions_possible
  directions <- c("N", "S", "E", "O", "NE", "NO", "SE", "SO")
  rag_actions <-  all_actions[(1 + length(directions)):length(all_actions)]
  
  # cambiar todo (sobreescribe todo)
  result <- calculate.position(state, action, problem)
  
  # cambiar el modo
  if (action %in% directions) {
    result$mode <- "GEN"
  } else if (action %in% rag_actions) {
    result$mode <- action
  }
  return(result)
}

# =========================================================================
# is.final.state(state, final_state, problem)
# =========================================================================
# PURPOSE:
#   Returns TRUE if the given state is a goal state.
#
# INPUT:
#   state       : current state
#   final_state : goal description (may be NULL in some problems)
#   problem     : problem definition
#
# OUTPUT:
#   TRUE  -> state satisfies goal condition
#   FALSE -> not a goal state
#
# NOTE:
#   Many problems do not have a predefined final_state object; instead,
#   the goal condition is checked directly using the state.
# =========================================================================
is.final.state <- function(state, final_state, problem) {
  result <- FALSE
  
  if (all(state$pos == final_state)) {
    result <- TRUE
  }
  
  return(result)
}

# =========================================================================
# to.string(state, problem)
# =========================================================================
# PURPOSE:
#   Converts a state into a unique string representation.
#
# OUTPUT:
#   A string that uniquely identifies the state.
#
# WHY THIS MATTERS:
#   - Used for printing states and debugging.
#   - Used by Graph Search to detect repeated states.
#
# IMPORTANT:
#   The string MUST include all information that defines the identity of the
#   state. If the state is incomplete in this string, Graph Search may prune
#   incorrectly and return wrong results.
# =========================================================================
to.string <- function(state, problem) {
  return(paste(state, collapse = ","))
}

# =========================================================================
# get.cost(action, state, problem)
# =========================================================================
# PURPOSE:
#   Returns the step cost of applying an action in the given state.
#
# INPUT:
#   action  : the action being applied
#   state   : the current state (before applying the action)
#   problem : problem definition
#
# OUTPUT:
#   Numeric cost of the action (NOT accumulated).
#
# IMPORTANT:
#   Costs must be NON-NEGATIVE for UCS and A* to behave correctly.
# =========================================================================
get.cost <- function(action, state, problem) {
  # TODO STUDENT:
  # Return the step cost of applying the action in this state.
  provider_data <-  problem$providers[[action]]
  provider_cost <- provider_data$cost
  provider_lat <- provider_data$lat
  
  params <- problem$params
  eur_per_sec <- params$EUR_PER_SEC
  gen_lat <- params$GEN_LAT
  switch_lat <- params$SWITCH_LAT
  
  
  all_actions <- problem$actions_possible
  directions <- c("N", "S", "E", "O", "NE", "NO", "SE", "SO")
  rag_actions <-  all_actions[(1 + length(directions)):length(all_actions)]
  actual_mode_is_rag_action <- state$mode %in% rag_actions
  action_is_rag_action <- action %in% rag_actions
  
  coste <- NULL
  
  if (state$mode == "GEN" && action %in% directions) {
    # gen -> gen
    coste <- gen_lat * eur_per_sec
  } else if ((actual_mode_is_rag_action &&
              (state$mode != action) &&
              action_is_rag_action) ||
             ((state$mode == "GEN") &&
              action_is_rag_action)) {
    # Rags distintos + gen a rag
    coste <- provider_cost + (switch_lat * eur_per_sec) + (provider_lat *
                                                             eur_per_sec)
  } else if ((actual_mode_is_rag_action) &&
             (state$mode == action)) {
    #De RAG a mismo RAG
    coste <- provider_lat * eur_per_sec
  } else if (actual_mode_is_rag_action &&
             action %in% directions) {
    # rag -> gen
    coste <- (switch_lat * eur_per_sec) + (gen_lat * eur_per_sec)
  }
  
  return(coste)
}

# =========================================================================
# get.evaluation(state, problem)
# =========================================================================
# PURPOSE:
#   Heuristic function h(n) used by informed search algorithms (Greedy, A*, etc.).
#
# INPUT:
#   state   : current state
#   problem : problem definition
#
# OUTPUT:
#   A numeric estimate of how far the state is from the goal.
#
# NOTES:
#   - For A* to be optimal, the heuristic should be admissible
#     (never overestimates the true remaining cost).
#   - Returning 0 is always admissible and makes A* behave like UCS.
# =========================================================================
get.evaluation <- function(state, problem) {
  # Heurística: Distancia de Chebyshev (máximo entre las diferencias en X e Y)
  dx <- abs(state$pos[1] - problem$state_final[1])
  dy <- abs(state$pos[2] - problem$state_final[2])
  
  return(max(dx, dy))
}

# Mapa
# print(res$current_state$pos)
# print(res$current_state$pos[1]) #x
# print(res$current_state$pos[2]) #y


#res <- initialize.problem("../mains/04-cost-trap.txt")
#print(res)
#print(to.string(res$current_state, res))
#print(is.applicable(res$current_state, "N", res))
#res$current_state$mode <- "GEN"
#new_state <- is.applicable(res$current_state, "VectorDB_ECO", res)
#new_state
#print(is.final.state(res$current_state, res$state_final, res))
#print(get.evaluation(res$current_state, res))

# print("Empieza prueba")
# res$current_state
# action <- "N"
# print(get.cost(action, res$current_state, res))
# if(is.applicable(res$current_state, action, res)) {
#
#   res$current_state <- effect(res$current_state, action, res)
# }
# res$current_state
# is.final.state(res$current_state, res$state_final, res)
#
# res$current_state
# action <- "VectorDB_ECO"
# print(get.cost(action, res$current_state, res))
# if(is.applicable(res$current_state, action, res)) {
#
#   res$current_state <- effect(res$current_state, action, res)
# }
# res$current_state
# is.final.state(res$current_state, res$state_final, res)

# res$current_state
# action <- "VectorDB_PRO"
# print(get.cost(action, res$current_state, res))
# if(is.applicable(res$current_state, action, res)) {
#
#   res$current_state <- effect(res$current_state, action, res)
# }
# res$current_state
# is.final.state(res$current_state, res$state_final, res)
