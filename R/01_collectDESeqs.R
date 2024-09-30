
collectDEseqs<-function(DEids, DEseqs, inDir, cellLines, confronti, 
                        outDir, caseOfInterest=NULL) {
  
  
  ###############################################################################
  ########  IMPORT DIFFERENTIALLY EXPRESSED FEATURES
  ###############################################################################
  
  
  DEids_smashed<-list()
  DEseqs_smashed<-list()
  for (c in 1:length(cellLines)){
    linea<-cellLines[c]
    selectedIDs<-c()
    selectedSeqs<-c()
    for (i in 1:length(confronti)) {
      confronto<-confronti[i]
      inputIDs<-DEids[[confronto]][[linea]]
      inputSeqs<-DEseqs[[confronto]][[linea]]
      #check
      #L<-length(inputIDs)
      #print(paste("number of items: ",L))
      #unione dei differenzialmente espressi nei vari confronti
      selectedIDs<-unique(union(selectedIDs,inputIDs)) 
      selectedSeqs<-unique(union(selectedSeqs,inputSeqs))
    }#chiude inner loop
    DEids_smashed[[c]]<-selectedIDs
    DEseqs_smashed[[c]]<-selectedSeqs
  }#chiude outer loop
  #print("IDs:")
  attr(DEids_smashed, "names") <- cellLines
  #print(attributes(DEids_smashed))
  
  #print("Ensemble sequences:")
  attr(DEseqs_smashed, "names") <- cellLines
  #print(attributes(DEseqs_smashed))
  
  
  ###############################################################################
  ########  SELECT SUBSET OF DIFFERENTIALLY EXPRESSED FEATURES
  ###############################################################################
  boolean<-grepl(pattern="union", x=outDir, ignore.case = FALSE)
  if (!boolean) {
    selectedIDs<-unique(DEids_smashed[[caseOfInterest]])
    selectedSeqs<-unique(DEseqs_smashed[[caseOfInterest]])
  } else  {
   selectedIDs<-unique(unlist(DEids_smashed))
   selectedSeqs<-unique(unlist(DEseqs_smashed))
 }
  #check
  #print("number of selected sequences:")
  #L<-length(selectedIDs)
  #print(paste("number of features: ",L))
  
  
  ########## CREATE EXPRESSION DATA FILE ##############
  
  filePath<-file.path(inDir,"normalizedCounts_withSymbol.tsv")
  normalizedCounts<- read.table( filePath, header=TRUE, skip=0, sep="\t")
  #names(normalizedCounts)
  normalizedCounts<-data.table(normalizedCounts)
  exprData<-normalizedCounts[normalizedCounts$ID_REF %in% selectedIDs & 
                               normalizedCounts$ensembleID %in% selectedSeqs]
  
  filePath<-file.path(outDir,"exprData.tsv")
  write.table(exprData, filePath, sep="\t", row.names=FALSE, quote=FALSE)

 
}
