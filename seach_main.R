# This is used to configure the enviroment
# *second instruction can provide error if path contains accents or special characters
rm(list=ls())
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("/Users/vitiok/University/IS/assignment_1")
setwd(getwd())
source("search_functions.R")

# load library for permutation function
library(gtools)

min_rods  <- 3
min_disks <- 3
max_rods  <- 10
max_disks <- 10

# check if value is in specified range
between <- function(value, min=0, max=20) {
  if(value < min || value > max) {
    return(FALSE)
  }
  
  return(TRUE)
}

# take input from keyboard
input_value <- function(text, min=0, max=20){ 
  n <- readline(prompt=text)
  n <- as.integer(n)
  if (is.na(n)){
    return(n <- input_value(text, min, max))
  }
  if(!between(n, min, max)) {
    return(n <- input_value(text, min, max))
  }
  
  return(n)
}


# input nr of disks and roads
# nrDisks <- input_value("Enter nr of Disks: ", min_disks, max_disks)
# nrRods  <- input_value("Enter nr of Rods: ", min_rods, max_rods)
# initialRod <- input_value("Enter initial rod position: ", 1, nrRods)
# finalRod <- input_value("Enter final rod position: ", 1, nrRods)

nrDisks <- 3
nrRods <- 3
initialRod <- 1
finalRod <- 3

# create initial and final state
initialState <- vector(mode = "integer", length = nrRods)
finalState   <- vector(mode = "integer", length = nrRods)
finalState   <- replace(finalState, initialState == 0, finalRod)
initialState <- replace(initialState, initialState == 0, initialRod)

possibleActions <- permutations(nrRods, 2, c(1:nrRods), set = TRUE)




#first possible action
cat(possibleActions[1])

# Check if is Applicable works for possible actions
# WORKS == TRUE
cat(isApplicable(c(2, 2, 2), possibleActions[3,]))
if (isApplicable(c(2, 2, 2), possibleActions[3,])) {
  newState = effect(c(2, 2, 2), possibleActions[3,])
}

# Check if effect function work
state = effect(newState, c(2,1))
print(state)

# Check isFinalState function
# WORKS == TRUE
isFinalState(c(2, 2, 2), c(2, 2, 2))

# Creation of the frontier with only the initial node
node = list()
node$state = initialState
node$actions = c(0,0)
node$deep = 0
frontier = list(node)

# Count is  used to avoid to fill memory (for bigger problems must be changed)
count = 1
countlimit = 1000000
  
  

  # While final state not found
  while (!isFinalState(node$state, finalState) & count<countlimit){
    # Break if frontier is empty
    if (length(frontier)==0){
      break
    }
    
    # Extract first node of the frontier
    firstnode = frontier[[1]]
    frontier[[1]] = NULL
    
    # If final state found, break and return results
    if (isFinalState(firstnode$state, finalState)){
      print("Final State Found")
      break
    }
    
    
    # For each one of the possible actions
    for (i in 1:nrow(possibleActions)){
      action = as.numeric(possibleActions[i,])
      state  = firstnode$state
      # If possible, it is applied and new node stored in frontier
      if (isApplicable(firstnode$state,action)){
        newnode = list()
        newstate = state
        newnode$state = effect(state,action)
        newnode$actions = rbind(firstnode$actions,action)
        newnode$deep = firstnode$deep+1
        frontier = append(frontier,list(newnode))
      }
    }
    count = count+1
  }
  
  # Show the obtained (or not) final solution
  if (count == countlimit | length(frontier)==0){
    print("Maximum Number of iterations reached. No solution found")
  } else{
    print("Solution found!!")
    print(firstnode$actions)
  }
  

  
