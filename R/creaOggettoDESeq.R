creaOggettoDESeq<-function(rawCounts, coldata){
  
  #crea oggetto di DeSeq2
  dds <- DESeqDataSetFromMatrix(countData = rawCounts,
                                colData = coldata,
                                design = ~ 1) #basic object
  
  return(dds)
}