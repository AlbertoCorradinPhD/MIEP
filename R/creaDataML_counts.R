creaDataML_counts<-function(exprData, Labels ){
  
  
  #############################################################################
  ### CREA LA TRASPOSTA 
  #########################################################################
  
  nomiFeatures<-exprData$ID_REF
  X<-as.matrix(exprData[,-1:-2])
  trasposta<-as.data.frame(t(X)) #trasposta samples/features
  dim(trasposta)
  names(trasposta)<-nomiFeatures
  trasposta$labels <-Labels
  
  #controllo duplicati
  checkDuplicates(nomiFeatures, trasposta)
  
  #creo dataML_counts
  dataML_counts<-subset(trasposta, select=c("labels",nomiFeatures)) #qualora occorra escludere delle colonne
  names(dataML_counts)

  return(dataML_counts)
}

