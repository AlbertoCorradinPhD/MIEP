masking<-function(DESeqDir,DeSeqObj){
  #leggo oggetto DESeq2 filtrato
  #print("DESeq object da sottoporre a test statistico:")
  filePath<-file.path(DESeqDir,"dds_filtered.rds")
  dds_filtered<-readRDS(file =filePath)
  matrice<-counts(dds_filtered)
  #str(matrice)
  seqsOfInterest<-attributes(matrice)$dimnames[[1]]
  #estraggo dal mio oggetto DESeq le sequenze d'interesse
  dds<-subsettingDeSeqObj(DeSeqObj,seqsOfInterest)

  return(dds)

}