isApplicable = function (state,action){
  lastDiskTo <- 0
  lastDiskFrom <- 0
  for (i in state) {
    if (state[i] == action[1]) {
      lastDiskFrom = i
    }
    if (state[i] == action[2]) {
      lastDiskTo = i
    }
  }
  if(lastDiskTo < lastDiskFrom) {
    return(TRUE)
  }
  return(FALSE)
}

effect = function (state,action){
  newstate = state
  return(newstate)
}

isFinalState = function (state, finalstate) {
  for(i in state) {
    if(state[i] != finalstate[i]) {
      return(FALSE)
    }
  }
  return(TRUE)
}

toString = function (state){
  print("Hello world")
}