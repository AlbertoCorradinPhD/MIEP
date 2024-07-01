creaResultsObj<-function(nameOfContrast,DeSeqObj){
  
  initial_alpha<-0.99 # mi tengo tutti i risultati, senza scremare alcunché
  resultsObj<- results(DeSeqObj, name=nameOfContrast,alpha = initial_alpha)
  return(resultsObj)
}