getTreeAndForest<-function(outDir,trainset,testset,
                           w,h,r,nameOfTree, titleOfTree, colori, pwd=NULL,
                           e=globalenv()){
  
  ##############################################################################
  #### FIND OPTIMAL PARAMETER SETTING
  ##############################################################################
  
  print("=====================================")
  print("SEARCH FOR FASTEST METHOD")
  bestWay<-bestCPForTreeSettings(trainset,testset, pwd=pwd, e=e)
  print("=============================================")
  print(paste("Fastest for calculation is:", bestWay))
  
  print("SEARCH OF BEST SETTINGS")
  lista<-getBestSettings(trainset,testset,bestWay, pwd=pwd, e=e)
  sol<-lista[[1]]
  bestSeeds<-lista[[2]]
  bestConfMatrix<-lista[[3]]
  rm(lista)
  
  #retrieve best settings
  bestAccuracy<-1/sol$minfun
  bestPval<-sol$minlevels$pval
  bestTesttype<-sol$minlevels$testtype
  bestTeststat<-sol$minlevels$teststat
  bestNtrees<-sol$minlevels$ntrees
  printBestSettings(bestAccuracy,bestPval, bestTesttype,
                              bestTeststat, bestNtrees,bestConfMatrix,
                              outDir)
  
  ### SET BEST PARS
  print("=====================================")
  print("Set best parameters for the construction of best random forest")
  lista<-settingsConditionalTrees(pval=bestPval, testtype=bestTesttype, 
                                  teststat=bestTeststat,ntrees=bestNtrees)
  controls<-lista[[1]]
  fcontrols<-lista[[2]]
  pval<-lista[[3]] 
  mincriterion<-lista[[4]] 
  rm(lista)
  
  
  ##############################################################################
  #### SINGLE (CONDITIONAL) TREE AND FOREST WITH BEST SETTINGS
  ##############################################################################
  
  # BEST TREE FROM TRAINSET
  print("=====================================")
  print("Single tree with best settings")
  #on trainset
  lista<-singleConditionalTree(controls,data=trainset,
                               newdata=testset, seed=bestSeeds)
  CM<-lista[[1]]
  cTree<-lista[[2]]
  rm(lista)
  
  print("Results for single conditional tree:")
  print(CM)
  Accuracy<-CM$overall['Accuracy']
  print(paste("Accuracy:", Accuracy))
  
  try( {
    filename<-paste(nameOfTree,".tiff",sep="")
    filePath<-file.path(outDir,filename)
    generaPleasantTree(cTree,w,h,r, colori, filePath, titleOfTree)
    } )
  
  # BEST FOREST FROM TRAINSET
  print("=====================================")
  print("Random forest with best settings")
  lista<-randomForest(fcontrols,data=trainset, newdata=testset, 
                      seed=bestSeeds)
  CM<-lista[[1]]
  rForest<-lista[[2]] 
  rm(lista)
  print("Results for random forest with best settings:")
  print(CM)
  Accuracy<-CM$overall['Accuracy']
  print(paste("Accuracy:", Accuracy))
  
  lista<-list(rForest,pval,mincriterion,cTree)
  return(lista)
}