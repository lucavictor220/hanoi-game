IsApplicable <- function (state, action) {
  lastDiskTo <- 0
  lastDiskFrom <- 0
  for (i in 1:length(state)) {
    if (state[i] == action[1]) {
      lastDiskFrom <- i
    }
    if (state[i] == action[2]) {
      lastDiskTo <- i
    }
  }
  if(lastDiskTo < lastDiskFrom) {
    return(TRUE)
  }
  
  return(FALSE)
}

Effect <- function (state, action) {
  newState <- state
  for (i in length(state):1) {
    if (state[i] == action[1]) {
        break
    }
  }
  newState[i] <- action[2]
  
  return(newState)
}

IsFinalState <- function (state, finalstate) {
  for(i in 1:length(state)) {
    if(state[i] != finalstate[i]) {
      return(FALSE)
    }
  }
  
  return(TRUE)
}

ToString <- function (state) {
  print("Hello world")
}

#finds the leftmost node with the maximum depth
#the node the DFS needs to expand 
findNodeFrontier = function (frontier) {
  max = 0
  indexMaxDepth = 1
  
  for (i in 1:length(frontier)) {
    if (max < frontier[[i]]$deep) {
      max = frontier[[i]]$deep
      indexMaxDepth = i
    }
  }
  
  return(indexMaxDepth) 
}

NodeIsVisited = function(previousState, action, frontier) {
  state = Effect(previousState, action)
  
  if (length(frontier) == 0) {
    return(FALSE)
  }
  
  for (i in 1:length(frontier)) {
    if (sum(frontier[[i]]$state == state) == length(state)) {
      return (TRUE)
    } 
  }
  
  return(FALSE)
}

