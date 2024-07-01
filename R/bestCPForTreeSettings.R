bestCPForTreeSettings<-function(trainset,testset, pwd=NULL,
                                e=globalenv()){
  
  #mediante breve test verifico qual e' il metodo piu' veloce per il gridSearch
  #atto a trovare il settings migliore per la random forest
  
  ### FIND BEST WAY FOR CALCOLO PARALLELO
  print("Looking for fastest method of computation")
  flag<-"testCP"
  flag<-"findBest"
  levels<-seqsOfValues(flag)
  
  #testa calcolo parallelo
  x<-list(levels=levels, trainset=trainset, testset=testset, e=e)
  names(x)
  bestWay<-calcoloParalleloFindBest( funzione=envelopeGridSearch, 
                                    x=x, pwd=pwd)
  return(bestWay)
}