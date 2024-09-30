batchFilter<-function(DeSeqObj,bF_threshold,DESeqDir){
  
  #filtraggio sui numeri complessivi del batch
  minNOfReads<-dim(DeSeqObj)[2]*bF_threshold
  filePath<-file.path(DESeqDir,"minNumberOfReadsRequiredPerSequence.txt")
  frase<-"minimal number of reads' counts per sequences required:"
  write(x=frase, file = filePath, ncolumns = 1, append = FALSE, sep = "\t")
  write(x=minNOfReads, file = filePath, ncolumns = 1, append = TRUE, sep = "\t")
  keep<-filterOnBatch(DeSeqObj=DeSeqObj, minNOfReads)

  ### SAVE RESULTS ON THE DESEQ OBJECT
  nomeFiltro<-"filterOnBatch"
  DeSeqObj<-saveResultsOfFilter(DESeqDir,DeSeqObj,nomeFiltro,keep)
  return(DeSeqObj)
  
}
