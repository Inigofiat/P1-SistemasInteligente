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
  
  #state[0] -> canibalesIzq
  #state[1] -> misionerossIzq
  #state[2] -> ladoBarca
  #state[3] -> capacidadBarca
  
  canibalesIzq <- as.numeric(state[0])
  misionerosIzq <- as.numeric(state[1])
  ladoBarca <- state[2]
  capacidadBarca <- as.numeric(state[3])
  print(canibalesIzq)
  
  #misionerosMovimiento <- action[1]
  #canibalesMovimiento <- action[2]
  #direccion <- action[3]

  canibalesDer <-  problem$numCanibales - canibalesIzq
  misionerosDer <-  problem$numMisioneros - misionerosIzq
  
  if (!any(ladoBarca == c("Derecha", "Izquierda"))) {
    return(FALSE)
  }
  if(action[1] + action[2] > capacidadBarca | action[1] + action[2] < 1){
    return(FALSE)
  }
  
  if(ladoBarca=="Izquierda"){
    if(action[1]>misionerosIzq | action[2]>misionerosIzq){
      return(FALSE)
    }
    numCanibalIzqFin <- misionerosIzq - action[2]
    numCanibalDchaFin <- canibalesDer + action[2]
    numMisioneroIzqFin <- misionerosIzq - action[1]
    numMisioneroDchaFin <- misionerosDer + action[1]
    
    if((numCanibalIzqFin > 0 & numCanibalIzqFin > numMisioneroIzqFin) || (numCanibalDchaFin > 0 & numCanibalDchaFin > numMisioneroDchaFin)){
      return(FALSE)
    }
    return(TRUE)
  }else{
    
    numCanibalIzqFin <- canibalesIzq + action[1]
    numCanibalDchaFin <- canibalesIzq - action[1]
    numMisioneroIzqFin <- misionerosIzq + action[2]
    numMisioneroDchaFin <- misionerosIzq - action[2]
    
    if ((numMisioneroIzqFin > 0 & numCanibalIzqFin > numMisioneroIzqFin) || 
        (numMisioneroDchaFin > 0 & numCanibalDchaFin > numMisioneroDchaFin)) {
      return(FALSE)
    }
    
    return(TRUE)
  }
  
  return(result)
}

# Returns the state resulting on applying the action over the state
effect <- function (state, action, problem) {
  result <- state # Default value is the current state.
  #state[0] -> canibalesIzq
  #state[1] -> misionerosIzq
  #state[2] -> ladoBarca
  #state[3] -> capacidadBarca
  
  canibalesIzq <- as.numeric(state[0])
  misionerosIzq <- as.numeric(state[1])
  ladoBarca <- state[2]
  capacidadBarca <- as.numeric(state[3])
  
  #misionerosMovimiento <- action[1]
  #canibalesMovimiento <- action[2]
  
  if(ladoBarca == "Izquierda"){
    result[0] <- canibalesIzq - action[2]
    result[1] <- misionerosIzq - action[1]
    result[2] <- "Derecha"
    result[3] <- capacidadBarca - (action[1] + action[2])
  }else{
    result[0] <- canibalesIzq + action[2]
    result[1] <- misionerosIzq + action[1]
    result[2] <- "Izquierda"
    result[3] <- capacidadBarca - (action[1] + action[2]) #???
  }
  
  return(result)
}

# Analyzes if a state is final or not
is.final.state <- function (state, final_satate, problem) {
  return(all(state == final_satate))
  
}

# Transforms a state into a string
to.string = function (state, problem) {
  return(paste("(", state[1], "C,", state[2], "M)-[", state[5], "]-(", state[3], "C,", state[4], "M)", sep=""))
}

# Returns the cost of applying an action over a state
get.cost <- function (action, state, problem) {
  # <INSERT YOUR CODE HERE TO RETURN THE COST OF APPLYING THE ACTION ON THE STATE>
  
  return(1) # Default value is 1.
}

# Heuristic function used by Informed Search Algorithms
#FUNCIÓN HEURÍSTICA: suma de las distancias de cada una de las casillas (excluyendo la que se encuentra vacía)
get.evaluation <- function(state, problem) {
  # <INSERT YOUR CODE HERE TO RETURN THE RESULT OF THE EVALUATION FUNCTION>
  
  return(1) # Default value is 1.
}

