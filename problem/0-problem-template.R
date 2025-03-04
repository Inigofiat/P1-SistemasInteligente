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
  
  problem$state_initial <- c(numCanibales, numMisioneros, "Izquierda")
  
  problem$state_final <- c(0,0,"Derecha")
  
  # Initialize the possible actions
  acciones <- c()
  for (canibal in 0:capacidadBarca) {
    for (misionero in 0:capacidadBarca) {
      if (canibal + misionero > 0 && canibal + misionero <= capacidadBarca) {
        acciones <- c(acciones, paste(c(rep("Canibal", canibal), rep("Misionero", misionero)), collapse = " "))
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
  
  if(state[0] == 0 && state[1] == 0){
    result<-TRUE
  }else{
    #capacidadBarca - 1 (para saber si alguien se ha montado/subido en la barca)
    #Verificar capacidadBarca
    #Si se montan en la barca -> canibalesIzq y/o misionerosIzq --> - num de misioneros/canibales subidos
    #capacidadBarca + 1 (para saber si alguien se ha bajado de la barca)
    #Verificar que misioneros no sean superados en num por canibales, a excepción de que todos los canibales esten en un lado y los misioneros en otro
    
    #recorrer lista de acciones, y ...
  }
  
  
  
  return(result)
}

# Returns the state resulting on applying the action over the state
effect <- function (state, action, problem) {
  result <- state # Default value is the current state.

  # <INSERT YOUR CODE HERE TO MODIFY CURRENT STATE>

  return(result)
}

# Analyzes if a state is final or not
is.final.state <- function (state, final_satate, problem) {
  result <- FALSE # Default value is FALSE.

  # <INSERT YOUR CODE HERE TO CHECK WHETHER A STATE IS FINAL OR NOT>

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

