stateOfProcess_update<-function(e=globalenv()){
  
  seed <- get0(".Random.seed", envir = globalenv(), ifnotfound = NULL)
  counter<-get0("counter", envir = e, ifnotfound = NULL)
  randomSeeds<-get0("randomSeeds", envir = e, ifnotfound = NULL)
  
  try({
    if (!is.null(randomSeeds)){
      print("=====================================")
      temp<-cbind(randomSeeds, seed) 
      assign("randomSeeds", temp, envir = e)
      temp<-counter+1
      assign("counter", temp, envir =  e)
      print("Variables of the hidden environment of the package have just been updated")
     }#chiude if
  })#chiude try
  
  assign("seed", seed, envir = e)
}