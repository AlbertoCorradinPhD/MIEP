callTopGO<-function(scoreTable, outDir,GO2gene, ntop, titleForGmtFile,
                    resDir, e=globalenv()){
  
  #creo cartella di destinazione
  dir.create(outDir, showWarnings = FALSE, recursive = TRUE)
  #filePath<-file.path(outDir,"scoreTable.tsv")
  #write.table(x=scoreTable[order(scoreTable$importance, decreasing=TRUE),], 
  #            file = filePath, append = FALSE, quote = FALSE, sep = "\t",
  #            col.names = FALSE)
  print("======================================")
  print(paste("GO terms enrichment test named:",titleForGmtFile))
  lista<-enrichmentCore(scoreTable, GO2gene, outDir,  e=e)
  GOobject<-lista[[1]]
  resultsTogether<-lista[[2]]
  rm(lista)
  
  suppressWarnings(boolean<-!any(is.null(GOobject),is.null(resultsTogether)))
  if (boolean) {
    # procedo solo con Fisher test per evitare incomprensioni
    releaseResults(GOobject,resultsTogether=resultsTogether["Fisher"],outDir, ntop, titleForGmtFile,
                resDir)
    }
}