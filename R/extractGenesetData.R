#' @importFrom dplyr distinct

fromDEs<-function(dataML, geneList) {
  
  #estrai data subset
  nomiFeatures<-names(dataML)
  boolean<- names(dataML) %in% geneList
  print("number of considered genes:")
  numberOfFeatures<-sum(boolean)
  print(numberOfFeatures)
  dataML<-cbind(dataML["labels"],dataML[,boolean])
  nomiFeatures<-nomiFeatures[boolean]
  names(dataML)<-c(names(dataML)[1],nomiFeatures)
  lista<-list(dataML,numberOfFeatures)
  return (lista)
}

fromNormalizedCounts<-function(geneList, Labels, inDir,outDir){
  
  ### READ DATASET
  filename<-file.path(inDir,"normalizedCounts_withSymbol.tsv")
  exprData<- read.table(filename,header = TRUE,sep = "\t") 
  exprData<-distinct(exprData,  .keep_all = FALSE)
  names(exprData)
  #estrai data subset
  boolean<- exprData$ID_REF %in% geneList
  print("number of considered genes:")
  numberOfFeatures<-sum(boolean)
  print(numberOfFeatures)
  
  exprData_sub<-exprData[boolean,]
  filename<-file.path(outDir,"exprData_subset.tsv")
  write.table(exprData_sub, file=filename, sep="\t", row.names=FALSE, quote=FALSE)
  
  dataML_counts<-creaDataML_counts(exprData=exprData_sub, Labels )
  
  lista<-list(dataML_counts,numberOfFeatures)
  return( lista)
  
}