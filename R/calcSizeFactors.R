calcSizeFactors<-function(DeSeqObj,filename){
  # estimates the size factors using the "median ratio method" described by 
  # Equation 5 in Anders and Huber (2010).
  dds<-estimateSizeFactors(DeSeqObj)
  sizeFactors<-sizeFactors(dds)
  write(x=names(sizeFactors), file = filename, ncolumns = length(sizeFactors), append = FALSE, sep = "\t")
  write(x=sizeFactors, file = filename, ncolumns = length(sizeFactors), append = TRUE, sep = "\t")
  return(dds)
}  
