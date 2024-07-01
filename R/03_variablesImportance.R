variablesImportance<-function(bestForest,w,h,r, colori,
                               nperm,conditional,OOB,
                              outDir, pwd){

  
  rForest<-bestForest$rForest
  pval<-bestForest$pval
  mincriterion<-bestForest$mincriterion
  trainset<-bestForest$trainset
  testset<-bestForest$testset
  rm(bestForest)
  
  dir.create(outDir, showWarnings = FALSE, recursive = TRUE)
  
  
  selectedFeatures<-getVariableImportance(nperm, conditional, OOB,
                                          rForest, pval,mincriterion,
                                          outDir,w=5000,h,r, pwd)
  if (is.null( selectedFeatures) ) {
    return(NULL)
  }
  
  print("most important features:")
  print(selectedFeatures)

  #selectedFeatures<-read.table(file=file.choose(), header = TRUE, sep = "\t")$feature[1:50]
  
  #### TREE WITH TOP FEATURES ONLY
  print("=========================================================")
  print("Let's build best possible tree with top features")
  trainset_top<-subset(x=trainset, select=c("classi",selectedFeatures))
  testset_top<-subset(x=testset, select=c("classi",selectedFeatures))
  nameOfTree<-"cTreeFromTopFeatures"
  titleOfTree<-"Conditional inference tree from top features"
  lista<-getTreeAndForest(outDir,trainset=trainset_top,
                          testset=testset_top,w,h,r,nameOfTree, titleOfTree, colori)

  rForest<-lista[[1]]
  pval<-lista[[2]]
  mincriterion<-lista[[3]]
  cTree<-lista[[4]]
  rm(lista)
  
  bestForest<-list(rForest=rForest,pval=pval,mincriterion=mincriterion,
                   outDir=outDir,trainset=trainset_top,
                   testset=testset_top  )
  filePath<-file.path(outDir,"bestForestFromTopFeatures.rds")
  saveRDS(object=bestForest, file=filePath)
  
  bestTree<-list(cTree=cTree, outDir=outDir,trainset=trainset_top,
                 testset=testset_top  )
  filePath<-file.path(outDir,"bestTreeFromTopFeatures.rds")
  saveRDS(object=bestTree, file=filePath)
  
  return(selectedFeatures)
}