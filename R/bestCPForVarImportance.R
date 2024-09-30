bestCPForVarImportance<-function(rForest, pval,mincriterion, pwd,
                                 conditional=FALSE, OOB=FALSE, nperm=100){
  
  #mediante breve test verifico qual e' il metodo piu' veloce per il gridSearch
  #atto a trovare il settings migliore per la random forest
  
  ### TEST CALCOLO PARALLELO
  print("Looking for fastest method of computation")
  #short case for test
  x<-list(nperm=nperm, conditional=conditional, OOB=OOB, 
          rForest=rForest, 
          pval=pval, mincriterion=mincriterion)
  names(x)
  bestWay<-calcoloParalleloFindBest( funzione=envelopeVarImportance, x=x, pwd)
  return(bestWay)
}