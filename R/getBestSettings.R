getBestSettings<-function(trainset,testset,bestWay, pwd=NULL,
                          e=globalenv()){
  
  # registro i random numbers causa l'eventuale
  # euristica nella costruzione di random forests
  suppressWarnings(
    try(stateOfProcess_remove(e=e))
  )
  stateOfProcess_create(e=e)
  
  ### LANCIA CALCOLO PARALLELO
  flag<-"testCP"
  flag<-"launchCP"
  levels<-seqsOfValues(flag)
 
  x<-list(levels=levels, trainset=trainset, testset=testset, e=e )
  names(x)
  results<-calcoloParallelo(fun=envelopeGridSearch,x=x, 
                            way=bestWay, pwd=pwd
                            )
  sol<-results[[1]]
  
  #get bestSeeds
  bestSeeds<-NULL
  bestConfMatrix<-NULL
  
  tryCatch({ 
    randomSeeds<-get0("randomSeeds", envir = e, ifnotfound = NULL)
    randomSeeds<-randomSeeds[,-1]  #la prima colonna e' dummy
    index<-match(x=sol$minfun, table=sol$values)
    bestSeeds<-as.vector(randomSeeds[,index])
    confMatrices<-get0("confMatrices", envir =e, ifnotfound = NULL)
    bestConfMatrix<-confMatrices[[index]]
    },
    error = function(err){
      print("not possible to retrieve random seeds for best random forest")
    })#chiude tryCatch
      
  #faccio spazio nella memoria
  #suppressWarnings(
  #  try( stateOfProcess_remove(e=e))
  #)
  
  lista<-list(sol,bestSeeds,bestConfMatrix)
  return(lista)
  
} 
