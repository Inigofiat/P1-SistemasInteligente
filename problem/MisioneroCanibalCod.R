# =========================================================================
# You must implement the different functions according to your problem.
# You cannot modify the function headers because they are used by the
# search algorithms. If you modify any headers the algorithms may not work.
# =========================================================================

# This function must return a list with the information needed to solve the problem.
# (Depending on the problem, it should receive or not parameters)
initialize.problem <- function(numCanibales, numMisioneros, capacidadBarca) {
  # Initialize the problem object as an empty list.
  problem <- list()
  
  # This attributes are compulsory
  problem$name <- paste0("MisionerosYCanibales")
  
  problem$state_initial <- c(numCanibales, numMisioneros, "Izquierda", capacidadBarca)
  problem$state_final <- c(0, 0, "Derecha", capacidadBarca)
  
  problem$numCanibales <- numCanibales
  problem$numMisioneros <- numMisioneros
  problem$capacidadBarca <- capacidadBarca
  
  # Initialize the possible actions
  acciones <- list()
  for (m in 0:min(numMisioneros, capacidadBarca)) {
    for (c in 0:min(numCanibales, capacidadBarca - m)) {
      if (m + c > 0 && m + c <= capacidadBarca) {
        acciones[[length(acciones) + 1]] <- c(m, c, 1)  
        acciones[[length(acciones) + 1]] <- c(m, c, -1)
      }
    }
  }
  
  
  problem$actions_possible <- do.call(rbind, acciones)
  
  # Return the problem list with all attributes
  return(problem)
}


# Analyzes if an action can be applied in the received state.


is.applicable <- function (state, action, problem) {
  
  result <- FALSE # Default value is FALSE.
  
  canibalesIzq <- as.numeric(state[1])
  misionerosIzq <- as.numeric(state[2])
  ladoBarca <- state[3]
  capacidadBarca <- as.numeric(state[4])
  
  #misionerosMovimiento <- action[1]
  #canibalesMovimiento <- action[2]
  #direccion <- action[3]
  
  canibalesDer <-  problem$numCanibales - canibalesIzq
  misionerosDer <-  problem$numMisioneros - misionerosIzq
  
  if (is.na(action[1]) || is.na(action[2]) || is.na(action[3])) {
    return(FALSE)
  }
  
  if(ladoBarca=="Izquierda" && action[3] == -1){
    if(action[1] > misionerosIzq || action[2] > canibalesIzq){
      return(FALSE)
    }
    numCanibalIzqFin <- canibalesIzq - action[2]
    numCanibalDchaFin <- canibalesDer + action[2]
    numMisioneroIzqFin <- misionerosIzq - action[1]
    numMisioneroDchaFin <- misionerosDer + action[1]
    
    if ((numMisioneroIzqFin > 0 && numCanibalIzqFin > numMisioneroIzqFin) || 
        (numMisioneroDchaFin > 0 && numCanibalDchaFin > numMisioneroDchaFin)) {
      return(FALSE)
    }
    
    return(TRUE)
  }else{
    if(action[1] > misionerosDer || action[2] > canibalesDer){
      return(FALSE)
    }
    numCanibalIzqFin <- canibalesIzq + action[2]
    numCanibalDchaFin <- canibalesDer - action[2]
    numMisioneroIzqFin <- misionerosIzq + action[1]
    numMisioneroDchaFin <- misionerosDer - action[1]
    
    if ((numMisioneroIzqFin > 0 && numCanibalIzqFin > numMisioneroIzqFin) || 
        (numMisioneroDchaFin > 0 && numCanibalDchaFin > numMisioneroDchaFin)) {
      return(FALSE)
    }
    
    return(TRUE)
  }
  
  return(result)
}

# Returns the state resulting on applying the action over the state
effect <- function (state, action, problem) {
  result <- state # Default value is the current state.
  
  canibalesIzq <- as.numeric(state[1])
  misionerosIzq <- as.numeric(state[2])
  ladoBarca <- state[3]
  capacidadBarca <- problem$capacidadBarca    
  
  #misionerosMovimiento <- action[1]
  #canibalesMovimiento <- action[2]
  
  if(ladoBarca == "Izquierda" && action[3] == -1){
    canibalesIzq <- canibalesIzq - action[2]
    misionerosIzq <- misionerosIzq - action[1]
    ladoBarca <- "Derecha"
  }else{
    canibalesIzq <- canibalesIzq + action[2]
    misionerosIzq <- misionerosIzq + action[1]
    ladoBarca <- "Izquierda"
  }
  
  return(c(canibalesIzq, misionerosIzq, ladoBarca))
  
}

# Analyzes if a state is final or not
is.final.state <- function (state, final_state, problem) {
  canibalesIzq <- as.numeric(state[1])
  misionerosIzq <- as.numeric(state[2])
  ladoBarca <- state[3]
  
  return(canibalesIzq == 0 && misionerosIzq == 0 && ladoBarca == "Derecha")
}


# Transforms a state into a string
to.string = function (state, problem=NULL) {
  canibalesIzq <- as.numeric(state[1])
  misionerosIzq <- as.numeric(state[2])
  ladoBarca <- state[3]
  
  canibalesDer <- problem$numCanibales - canibalesIzq
  misionerosDer <- problem$numMisioneros - misionerosIzq

  return(paste("(", canibalesIzq, "C,", misionerosIzq, "M)-[", ladoBarca, "]-(", canibalesDer, "C,", misionerosDer, "M)", sep=""))
}

# Returns the cost of applying an action over a state
get.cost <- function (action, state, problem) {
  
  canibalesIzq <- as.numeric(state[1])
  misionerosIzq <- as.numeric(state[2])
  
  #misionerosMovimiento <- action[1]
  #canibalesMovimiento <- action[2]
  
  
  #??????????
  coste <- 15 + (1 + (action[1] * 0.10) + (action[2]*0.05))
  coste <- 15 + (action[1] + action[2])
  
  
  return(coste) # Default value is 1.
}

# Heuristic function used by Informed Search Algorithms
#FUNCIÓN HEURÍSTICA: suma de las distancias de cada una de las casillas (excluyendo la que se encuentra vacía)
get.evaluation <- function(state, problem) {
  # <INSERT YOUR CODE HERE TO RETURN THE RESULT OF THE EVALUATION FUNCTION>
  
  return(1) # Default value is 1.
}
