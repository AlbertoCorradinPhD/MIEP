calcsOnDESeqObj<-function(DeSeqObj,filename1,filename2){
  #NB: non si va ad alterare l'oggetto DESeq
  
  #write down size factors 
  dds_temp<-calcSizeFactors(DeSeqObj,filename=filename1)
  
  #write down normalized counts 
  normCounts<-normalizeCounts(DeSeqObj=dds_temp, filename=filename2)
  rm(dds_temp)
  
}