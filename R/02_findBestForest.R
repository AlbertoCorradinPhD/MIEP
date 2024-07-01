findBestForest<-function(X, outDir, w,h,r, colori, pwd=NULL,
                         e=globalenv()){
  
  ### TRAINING AND TEST SETS 
  lista<-splitTrainTestsets(X,train = 0.7, valid = 0.1, test = 0.2)
  trainset<-lista[[1]]
  testset<-lista[[2]]
  rm(lista)
  
  #### CALCULATE BEST FOREST
  nameOfTree<-"cTreeFromTrainset"
  titleOfTree<-"Conditional inference tree from best settings"
  lista<-getTreeAndForest(outDir,trainset,testset,w,h,r,
                          nameOfTree, titleOfTree, colori, pwd,
                          e=e)
  rForest<-lista[[1]]
  pval<-lista[[2]]
  mincriterion<-lista[[3]]
  cTree<-lista[[4]]
  rm(lista)
  #to print a tree of the forest
  #aTree<-prettytree(rForest@ensemble[[1]], names(rForest@data@get("input")))
  
  bestForest<-list(rForest=rForest,pval=pval,mincriterion=mincriterion,
                   trainset=trainset,testset=testset )
  filePath<-file.path(outDir,"bestForest.rds")
  saveRDS(object=bestForest, file=filePath)
  bestTree<-list(cTree=cTree, trainset=trainset,testset=testset )
  filePath<-file.path(outDir,"bestTree.rds")
  saveRDS(object=bestTree, file=filePath)
 
  return(bestForest)

}
