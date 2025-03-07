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
  
  problem$state_initial <- c(numCanibales, numMisioneros, 0,0, "Izquierda")
  
  problem$state_final <- c(0,0,numCanibales, numMisioneros, "Derecha")
  
  # Initialize the possible actions
  acciones <- list()
  for (canibal in 0:capacidadBarca) {
    for (misionero in 0:capacidadBarca) {
      if (canibal + misionero > 0 && canibal + misionero <= capacidadBarca) {
        acciones[[length(acciones)+1]] <- c(canibal, misionero)
      }
    }
  }
  
  problem$acciones <- acciones
  problem$numCanibales <- numCanibales
  problem$numMisioneros <- numMisioneros
  problem$capacidadBarca <- capacidadBarca
  
  
  
  # Actions possible as a data frame (direction could be interpreted as action)
  problem$actions_possible <- c(acciones, stringsAsFactors = FALSE)
  
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
  
  
  #state[4] -> canibalesDch
  #state[5] -> misionerosDch
  
  
  #misionerosMovimiento <- action[1]
  #canibalesMovimiento <- action[2]
  
  if(state[0] == 0 & state[1] == 0){
    result<-TRUE
  }else{
  }
  
  if (!any(state[2] == c("Derecha", "Izquierda"))) {
    return(FALSE)
  }
  
  if(action[1] + action[2] > state[3] | action[1] + action[2] < 1){
    return(FALSE)
  }
  
  if(state[2]=="Izquierda"){
    if(action[1]>state[1] | action[2]>state[0]){
      return(FALSE)
    }
    numCanibalIzqFin <- state[0] - action[2]
    numCanibalDchaFin <- state[0] + action[2]
    numMisioneroIzqFin <- state[1] - action[1]
    numMisioneroDchaFin <- state[1] + action[1]
    
    if((numCanibalIzqFin > 0 & numCanibalIzqFin > numMisioneroIzqFin) || (numCanibalDchaFin > 0 & numCanibalDchaFin > numMisioneroDchaFin)){
      return(FALSE)
    }
    return(TRUE)
  }else{
    #if(action[2]>state[4] | action[2]>state[5]){
    # return(FALSE)
    #}
    
    numCanibalIzqFin <- state[0] + action[1]
    numCanibalDchaFin <- state[0] - action[1]
    numMisioneroIzqFin <- state[1] + action[2]
    numMisioneroDchaFin <- state[1] - action[2]
    
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
  
  #misionerosMovimiento <- action[1]
  #canibalesMovimiento <- action[2]
  
  if(state[2] == "Izquierda"){
    result[0] <- state[0] - action[2]
    result[1] <- state[1] - action[1]
    result[2] <- "Derecha"
    result[3] <- state[3] - (action[1] + action[2])
  }else{
    result[0] <- state[0] + action[2]
    result[1] <- state[1] + action[1]
    result[2] <- "Izquierda"
    result[3] <- state[3] - (action[1] + action[2]) #???
  }
  
  return(result)
}

# Analyzes if a state is final or not
is.final.state <- function (state, final_satate, problem) {
  result <- FALSE # Default value is FALSE.
  if(state[0] == final_satate[0] & state[1]==final_satate[1]){
    return(TRUE)
  }
  
  return(result)
}

# Transforms a state into a string
to.string = function (state, problem) {
  # <INSERT YOUR CODE HERE TO GENERATE A STRING THAT REPRESENTS THE STATE>
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

