normalizeCounts<-function(DeSeqObj, filename){

  ##### WRITE DOWN NORMALIZED COUNTS ###
  normCounts<-round(counts(DeSeqObj, normalized=TRUE,replaced = FALSE),0) #matrice
  #head(normCounts,20)
  normCounts<-cbind(as.data.frame(row.names(normCounts)), normCounts)
  names(normCounts)[1]<-"ensembleID"
  write.table(x=normCounts, file = filename, append = FALSE, quote = FALSE, sep = "\t",
              eol = "\n", na = "NA", dec = ".", row.names = FALSE,
              col.names = TRUE)
  
  return (normCounts)

}