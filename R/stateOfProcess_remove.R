stateOfProcess_remove<-function(e=globalenv()){
  
  seed <- get0("seed", envir = e, ifnotfound = NULL)
  counter<-get0("counter", envir = e, ifnotfound = NULL)
  randomSeeds<-get0("randomSeeds", envir = e, ifnotfound = NULL) 
  confMatrices<-get0("confMatrices", envir = e, ifnotfound = NULL)  
  rm(seed, envir=e)
  rm(counter, envir=e)
  rm( randomSeeds, envir=e)
  rm( confMatrices, envir=e)
   
}
