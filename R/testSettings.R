testSettings<-function(x, data, newdata, e=globalenv()) {
 
  lista<-settingsConditionalTrees(pval=x$pval, testtype=x$testtype, 
                                  teststat=x$teststat, ntrees=x$ntrees)
  controls<-lista[[1]]
  fcontrols<-lista[[2]]
  #pval<-lista[[3]]
  #mincriterion<-lista[[4]]
  rm(lista)
  
  inverso<-1000
  tryCatch( {
    seed<-NULL
    #aggiorno variabili globali part 1
    if (exists("randomSeeds",envir=e) && exists("counter",envir=e)) {
      #update state of process
      stateOfProcess_update(e=e)
      #print state of process
      stateOfProcess_print(e=e)
      seed <- get0("seed", envir = e, ifnotfound = NULL) 
      }
   
    suppressWarnings( 
      lista<-randomForest(fcontrols=fcontrols,data=data, 
                          newdata=newdata, seed=seed)
    )
    CM<-lista[[1]]
    rForest<-lista[[2]]
    rm(lista)
      
    #aggiorno variabili globali part 2
    if (exists("confMatrices",envir=e) ) {
      confMatrices<-get0("confMatrices", envir =e, ifnotfound = NULL)
      temp<-list.append(confMatrices,confMatrix=CM) 
      assign("confMatrices", temp, envir = e)
      }

    
    #print parameters
    Accuracy<-CM$overall['Accuracy']
    print(paste("pval:",x$pval))
    print(paste("testtype:",x$testtype))
    print(paste("teststat:",x$teststat))
    print(paste("number of trees in the forest:",x$ntrees))
    
    #valore ritornato
    print(paste("Accuracy:", Accuracy))
    inverso<-1/Accuracy
    attributes(inverso)$names<-"Inverso"
    },
  error = function(err){
    print("Random forest cannot be built with such iperparameters")
  })#chiude tryCatch
  
  return(inverso)
}
