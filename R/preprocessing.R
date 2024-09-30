preprocessing<-function(originalCounts, coldata, flag_cellLine,DeSeqObj, 
                        bF_threshold, cF_threshold,DESeqDir){
  
  print("preprocessing chapter")
  if (flag_cellLine=="Filter" ){ #& !flag_singleCondition
    dummy_dds<-DeSeqObj
    #numero minimo di conte per sequenza in campione
    filtering(originalCounts, coldata,DeSeqObj=dummy_dds, bF_threshold,cF_threshold,
       DESeqDir)
    rm(dummy_dds)
    
  } 
  
  ### MASKING DELL'OGGETTO DESEQ BASED ON THE FILTERING STEP
  # leggo oggetto DESeq2 filtrato. Estraggo dall'oggetto DESeq 
  # pulito e non normalizzato le sequenze che hanno passato i filtri
  dds<-masking(DESeqDir,DeSeqObj)
  print("preprocessing accomplished")
  return(dds)

}