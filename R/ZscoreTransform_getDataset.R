ZscoreTransform_getDataset<-function(dataML){
  
  ######################## CREA DATASET DI CONTE CORRETTE PER I BASALI #######################
  
  nomi_campioni<-row.names(dataML)
  labels<-dataML$labels
  trasposta<-as.data.frame(t(sapply(dataML[,-1], as.numeric))) #trasposta samples/features
  dim(trasposta)
  names(trasposta)<-nomi_campioni
  exprData_ZscoreTransformed<-cbind(as.data.frame(row.names(trasposta)),trasposta)
  names(exprData_ZscoreTransformed)[1]<-"ID_REF"
  #sistema i duplicati
  pattern<-"_seq[1-9]"
  exprData_ZscoreTransformed$ID_REF<-str_replace_all(exprData_ZscoreTransformed$ID_REF,pattern , "")
  return(exprData_ZscoreTransformed)
}