isApplicable = function (state,action){
  return(TRUE)
}

effect = function (state,action){
  newstate = state
  return(newstate)
}

isFinalState = function (state,finalstate){
  return(FALSE)
}

toString = function (state){
  print("Hello world")
}