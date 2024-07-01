saveResultsOfFilter<-function(DESeqDir, DeSeqObj,nomeFiltro,keep, df=NULL){
  
  #sequences to test
  filename<-paste("numberOfSeqsToTestFor_",nomeFiltro,".txt",sep="")
  filePath<-file.path(DESeqDir,filename)
  write(x="number of sequences undergoing statistical test:", file = filePath, 
        ncolumns = 1, append = FALSE, sep = "\t") 
  write(x=sum(keep), file = filePath, ncolumns = 1, append = TRUE, sep = "\t")
  
  #write data frame
  if(is.null(df)) {
    df<-data.frame(EnsembleID=names(keep),batch=keep)
  } else  {
    df<-cbind(names(keep),df,keep)
    names(df)[1]<-"EnsembleID"
  }
  filename<-paste("resultsOf_",nomeFiltro,".tsv",sep="")
  filePath<-file.path(DESeqDir,filename)
  write.table(x=df, file = filePath, append = FALSE, quote = FALSE, sep = "\t",
              row.names = FALSE, col.names = TRUE)
  
  #save discarded sequences
  DeSeqObj_discarded<- DeSeqObj[!keep,]
  filePath<-file.path(DESeqDir,"dds_discarded.rds")
  saveRDS(DeSeqObj_discarded, file = filePath)
  
  # save kept sequences
  DeSeqObj<-DeSeqObj[keep,]
  print("store DeSeq object in rds file")
  filePath<-file.path(DESeqDir,"dds_filtered.rds")
  saveRDS(DeSeqObj, file = filePath)
  return(DeSeqObj)
}
