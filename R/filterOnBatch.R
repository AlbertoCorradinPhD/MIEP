
filterOnBatch<-function(DeSeqObj, minNOfReads){
  ######## FILTERING ON THE NUMBER OF SEQUENCES IN THE BATCH  #############
  #selection
  keep <- rowSums(counts(DeSeqObj)) >minNOfReads #boolean
  return(keep)
  
} 