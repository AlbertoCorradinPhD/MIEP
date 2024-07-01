calcVarImportance<-function(nperm,conditional, OOB,rForest,
                            pval,mincriterion, bestWay, pwd){
 
  
  print("=====================================")
  print("CALCOLO VARIABLE IMPORTANCE")
  #true complete run
  x<-list(nperm=nperm, conditional=conditional, OOB=OOB, 
          rForest=rForest, 
          pval=pval, mincriterion=mincriterion)
  names(x)
  results<-calcoloParallelo(fun=envelopeVarImportance,x=x, 
                            way=bestWay,  pwd)
  varImportance<-results[[1]]
  #str(varImportance)
  rm(results)
  
  
  return( varImportance)
  
}