stateOfProcess_print<-function(e=globalenv()){
  
  counter<-get0("counter", envir = e, ifnotfound = NULL)
  print("current state of the process:")
  print(paste("counter=", counter))
  randomSeeds<-get0("randomSeeds", envir = e, ifnotfound = NULL)
  M<-(dim(randomSeeds)[2]-1) #la prima colonna e' dummy
  if (M==counter){ print("database of random seeds was updated") }
  seed <- get0("seed", envir = e, ifnotfound = NULL) 
  print(paste("sum of .Random.seed global variable:", sum( seed)))
  
}