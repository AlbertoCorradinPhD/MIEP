selectDEsequencesWithName<-function(inDir,  DESeqDir, cellLines, confronti){
  
  ########## IMPORT FILE NORMALIZED COUNTS WITH BOTH NAMES AND ENSEMBLE ID ##############
  filePath<-file.path(inDir,"normalizedCounts_withSymbol.tsv") #conte normalizzate con ensembleID
  normalizedCounts<- read.table( filePath, header=TRUE, skip=0, sep="\t") 
  names(normalizedCounts)
  
  
  ########## IMPORT FILE WITH ONLY SIGNIFICANT SEQUENCES FROM DESEQ ##############
  outDir<-file.path(inDir,"DifferentiallyExpressedSeqs")
  dir.create(outDir, showWarnings = FALSE, recursive = TRUE)
  
  for (c in 1:length(cellLines)){
    linea<-cellLines[c]
    foldername<-paste("test-",linea, sep="")
    testTablesDir<-file.path(DESeqDir,foldername,"testTables")
    for(j in 1: length(confronti)){
      confronto<-confronti[j]
      table<-importTestTable_DEonly(testTablesDir,confronto)
      normalizedCounts_DEsequences<-subset(x=normalizedCounts, 
                            normalizedCounts$ensembleID %in% table$ensembleID)
      names(normalizedCounts_DEsequences)
      #print(paste("cell line:",linea,"- test:", confronto))
      #print("differentially expressed sequences without HUGO symbol:")
      #print(setdiff(table$ensembleID,normalizedCounts$ensembleID))
      #print("==================================================")
      
      filename<-paste(confronto,"-",linea,".tsv",sep="")
      filePath<-file.path(outDir,filename)
      write.table(x=normalizedCounts_DEsequences, file = filePath, append = FALSE, 
                  quote = FALSE, sep = "\t", eol = "\n", na = "NA", dec = ".", 
                  row.names =FALSE, col.names = TRUE)
    }
    
  }
  
  return(outDir)
}
