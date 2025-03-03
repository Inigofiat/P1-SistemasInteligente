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
  
  # Initial state of the problem (number of Canibales, Misioneros, and position of the boat)
  problem$state_initial <- data.frame(Canibal = numCanibales, Misionero = numMisioneros, Barca = "Izquierda")
  
  # Final state (goal state where all are on the right side)
  problem$state_final <- data.frame(Canibal = 0, Misionero = 0, Barca = "Derecha")
  
  # Initialize the possible actions
  acciones <- c()
  for (canibal in 0:capacidadBarca) {
    for (misionero in 0:capacidadBarca) {
      if (canibal + misionero > 0 && canibal + misionero <= capacidadBarca) {
        acciones <- c(acciones, paste(c(rep("Canibal", canibal), rep("Misionero", misionero)), collapse = " "))
      }
    }
  }
  
  # Assign actions to the problem object
  problem$acciones <- acciones
  
  # Actions possible as a data frame (direction could be interpreted as action)
  problem$actions_possible <- data.frame(direction = acciones, stringsAsFactors = FALSE)
  
  # Return the problem list with all attributes
  return(problem)
}


# Analyzes if an action can be applied in the received state.


is.applicable <- function (state, action, problem) {
  result <- FALSE # Default value is FALSE.
  
  
  
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

