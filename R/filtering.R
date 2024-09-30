filtering<-function(originalCounts, coldata,DeSeqObj,
                    bF_threshold,cF_threshold,
                     DESeqDir){
  
  
  ### DEPICT SITUATION BEFORE FILTERING
  filename1<-file.path(DESeqDir,"sizeFactors_beforeFiltering.tsv")
  filename2<-file.path(DESeqDir,"normalizedCounts_beforeFiltering.tsv")
  print("estimated size factors, before filtering:")
  calcsOnDESeqObj(DeSeqObj,filename1,filename2)
  
  #############################################################################
  #### FILTERING CHAPTER 
  ############################################################################
  print("filtering chapter")
  print("number of sequences before filtering:")
  print(dim(DeSeqObj)[1]) #NB: è ancora il database originario
  
  
  ### FILTER ON PHENOTYPES
  if (cF_threshold>0){
    DeSeqObj<-cribiFilter(originalCounts, coldata, DeSeqObj,cF_threshold, DESeqDir)
    print("number of sequences after 'pheno filter':")
    print(dim(DeSeqObj)[1]) #NB: non è più il database originario
  }
  
  ### FILTER ON BATCH 
  if (bF_threshold>0) {
    DeSeqObj<-batchFilter(DeSeqObj,bF_threshold,DESeqDir)
    print("number of sequences after 'batch filter':")
    print(dim(DeSeqObj)[1]) #NB: non è più il database originario
  } #chiude if
  
  
  ### DEPICT SITUATION AFTER FILTERING
  filename1<-file.path(DESeqDir,"sizeFactors_afterFiltering.tsv")
  filename2<-file.path(DESeqDir,"normalizedCounts_afterFiltering.tsv")
  print("estimated size factors, after filtering:")
  calcsOnDESeqObj(DeSeqObj,filename1,filename2)
  
} 

