#' @importFrom gridExtra grid.arrange

collectTestResults<-function(DESeqDir, cellLines, confronti, outDir, consideredCase) {

  filePath<-file.path(outDir,"exprData.tsv")
  exprData<-read.table(file=filePath, header = TRUE, sep = "\t")
  
  ###############################################################################
  ###  CREATE TEST TABLE SEQUENCES OF SELECTED SEQUENCES
  ###############################################################################
  
  newTable<-exprData[,c(1,2)]
  table1<-newTable
  table2<-newTable
  #head(newTable$ensembleID)
  
  for (l in 1:length(cellLines)){# apri loop cellLines
    linea<-cellLines[l]
    for (c in 1:length(confronti)){ # apri loop confronti
      confronto<-confronti[c]

      #raccogli i dati
      newdata<- generaTestTablePerGeneset(inDir=DESeqDir, linea, confronto, 
                                          myTable=newTable )
      
      #genera tabella, formato pdf
      if (consideredCase=="union" || consideredCase==linea) {
        suppressWarnings(genesetFormattedTable(linea,confronto,outDir,newdata))
        #aggiorno le tabelle per statistiche
        names(newdata)[3]<-paste(linea, confronto, names(newdata)[3])
        names(newdata)[4]<-paste(linea, confronto, names(newdata)[4])
        names(newdata)[5]<-paste(linea, confronto, names(newdata)[5])
        names(newdata)[6]<-paste(linea, confronto, names(newdata)[6])
        table1<-cbind(table1, newdata[,c(3,4)])
        table2<-cbind(table2, newdata[,c(5,6)])
      } #chiudo if
    } #chiudo loop confronti
  }   # chiudo loop cellLine
  
  ### BOXPLOT PADJ
  suppressWarnings(boxplotAdjs(inDir=outDir, outDir,table1, table2, consideredCase, cellLines, confronti))
  
  ### BOXPLOT logFC
  suppressWarnings(boxplotLogFC(inDir=outDir, outDir,table1, table2, consideredCase, cellLines, confronti))
  
}


  

