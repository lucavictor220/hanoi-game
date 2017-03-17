# This is used to configure the enviroment
# *second instruction can provide error if path contains accents or special characters
rm(list=ls())
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#setwd("/Users/vitiok/University/IS/assignment_1")
setwd(getwd())
#clesource("search_functions.R")

print(getwd())

library(gtools)

# Configuration of the problem
ndisks = 3 # 10
nrods  = 3 # 5
initialrod = 1
finalrod   = 3


# ==== My code ==== 




read_value <- function(info){ 
  n <- readline(prompt=info)
  n <- as.integer(n)
  if (is.na(n) || n < 3){
    return(n <- read_value(info))
  } 
  return(n)
}

# input nr of disks and roads
# issue => have to validate input so that it receives only integers > 2 
nrDisks <- read_value("Enter nr of Disks: ")
nrRods <- read_value("Enter nr of Rods: ")





# create initial and final state
initialState <- vector(mode = "integer", length = nrRods)
finalState   <- vector(mode = "integer", length = nrRods)
finalState   <- replace(finalState, initialState == 0, nrRods)
initialState <- replace(initialState, initialState == 0, 1)

possibleActions = permutations(nrRods, 2, c(1:nrRods), repeats.allowed=FALSE)

class(possibleActions)
cat(possibleActions)



# ==== My code ==== 

# Definition of initial and final state
initialstate = c(1,1,1) # (1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
finalstate   = c(3,3,3) # (5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5), where 5 is the nrods

# Definition of all possible actions
possibleactions = data.frame(orig=1,dest=2)
possibleactions = rbind(possibleactions,c(1,3))
possibleactions = rbind(possibleactions,c(2,3))
possibleactions = rbind(possibleactions,c(2,1))
possibleactions = rbind(possibleactions,c(3,1))
possibleactions = rbind(possibleactions,c(3,2))



# Creation of the frontier with only the initial node
node = list()
node$state = initialstate
node$actions = c(0,0)
node$deep = 0
frontier = list(node)

# Count is  used to avoid to fill memory (for bigger problems must be changed)
count = 1
countlimit = 100
  
  

  # While final state not found
  while (!isFinalState(node$state,finalstate) & count<countlimit){
    # Break if frontier is empty
    if (length(frontier)==0){
      break
    }
    
    # Extract first node of the frontier
    firstnode = frontier[[1]]
    frontier[[1]] = NULL
    
    # If final state found, break and return results
    if (isFinalState(firstnode$state)){
      print("Final State Found")
      break
    }
    
    
    # For each one of the possible actions
    for (i in 1:nrow(possibleactions)){
      action = as.numeric(possibleactions[1,])
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
  

  
