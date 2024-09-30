printDeSeqObj<-function(dds){
  
  print(dds)
  #print(mcols(dds))
  print(as.data.frame(colData(dds)))
  print("dimensions of the DESeq object:")
  print(dim(dds))
  print("size factors (NULL before statistical test):")
  print(sizeFactors(dds))
  print(paste("test design:",design(dds)[1],design(dds)[2]))
  #print(paste("current control if test by cellLine:",levels(dds$cellLine)[1]))
  print(paste("current samples to be used as control:",levels(dds$condition)[1]))
}

