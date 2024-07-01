#implementati i test per trovare i differenzialmente espressi
statisticalTest <- function(DeSeqObj,flag_MFtest) {

  if (flag_MFtest){
    # multiple variable design
    print("multiple variables design. Wald statistical test")
    #DeSeqObj <- DESeq(DeSeqObj, test="LRT", reduced=reducedModel,minReplicatesForReplace = 3)
    DeSeqObj <- DESeq(DeSeqObj, test="Wald", minReplicatesForReplace = 3)
    } else {
    # single variable design
    print("single variable design. Wald statistical test")
    DeSeqObj <- DESeq(DeSeqObj, test="Wald",minReplicatesForReplace = 3)
  } 
  
  return(DeSeqObj)
}