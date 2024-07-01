subsettingDeSeqObj<-function(DeSeqObj,seqsOfInterest){
  dds<-DeSeqObj[seqsOfInterest,]
  return(dds)

}