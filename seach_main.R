# This is used to configure the enviroment
# *second instruction can provide error if path contains accents or special characters
rm(list=ls())

# IMPORTANT: 
# Set working directory properly on your own machine
setwd("/Users/vitiok/University/IS/assignment_1")
# Load search functions implementation
source("search_functions.R")

# Load library for permutation function
library(gtools)

kMinRods  <- 3
kMinDisks <- 3
kMaxRods  <- 10
kMaxDisks <- 10

# Variables used to analyze algorithm
expendedNodes = 0


# check if value is in specified range
Between <- function(value, min=0, max=20) {
  if(value < min || value > max) {
    return(FALSE)
  }
  
  return(TRUE)
}

# take input from keyboard
InputValue <- function(text, min=0, max=20) { 
  n <- readline(prompt = text)
  n <- as.integer(n)
  
  if (is.na(n)) {
    return(n <- InputValue(text, min, max))
  }
  if(!Between(n, min, max)) {
    return(n <- InputValue(text, min, max))
  }
  
  return(n)
}


# input nr of disks and roads
# nrDisks <- InputValue("Enter nr of Disks: ", kMinDisks, kMaxDisks)
# nrRods  <- InputValue("Enter nr of Rods: ", kMinRods, kMaxRods)
# initialRod <- InputValue("Enter initial rod position: ", 1, nrRods)
# finalRod <- InputValue("Enter final rod position: ", 1, nrRods)

nrDisks <- 4
nrRods <- 4
initialRod <- 1
finalRod <- 4

# create initial and final state
initialState <- vector(mode = "integer", length = nrDisks)
finalState   <- vector(mode = "integer", length = nrDisks)
finalState   <- replace(finalState, initialState == 0, finalRod)
initialState <- replace(initialState, initialState == 0, initialRod)

finalState
initialState

IsFinalState(c(3, 3, 3, 3), c(3, 3, 3, 3))

# Find all possible actions
possibleActions <- permutations(nrRods, 2, c(1:nrRods), set = TRUE)

# Creation of the frontier with only the initial node
node = list()
node$state = initialState
node$actions = c(0, 0)
node$deep = 0
frontier = list(node)

# Count is  used to avoid to fill memory (for bigger problems must be changed)
count = 1
countLimit = 20000
  
  

  # While final state not found
  while (!IsFinalState(node$state, finalState) & count < countLimit) {
    # Break if frontier is empty
    if (length(frontier) == 0) {
      break
    }
    
    # Extract first node of the frontier
    firstNode = frontier[[1]]
    frontier[[1]] = NULL
    
    # If final state found, break and return results
    if (IsFinalState(firstNode$state, finalState)) {
      print("Final State Found")
      break
    }
    
    
    # For each one of the possible actions
    for (i in 1:nrow(possibleActions)) {
      action = as.numeric(possibleActions[i, ])
      state  = firstNode$state
      # If possible, it is applied and new node stored in frontier
      if (IsApplicable(firstNode$state, action)) {
        newNode = list()
        newState = state
        newNode$state = Effect(state, action)
        newNode$actions = rbind(firstNode$actions, action)
        newNode$deep = firstNode$deep + 1
        if (!is.element(newNode, frontier)) {
          frontier = append(frontier, list(newNode))
          print("new node")
        }
      }
    }
    count = count + 1
    print(count)
    print(newNode$state)
  }
  
  # Show the obtained (or not) final solution
  if (count == countLimit | length(frontier) == 0) {
    print("Maximum Number of iterations reached. No solution found")
  } else {
    print("Solution found!!")
    print(firstNode$actions)
  }
  

  
