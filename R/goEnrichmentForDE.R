#' @importFrom XML readHTMLTable
#' @importFrom stringr str_trim

goEnrichmentForDE<-function( resDir, exprSet,
                             cellLines,GO2gene,ntop, confronti,
                             e=globalenv()) {

  for (c in 1:length(cellLines)) {
    linea<-cellLines[c]
    #print(paste("cell line under examination:", linea))
    foldername<-paste("DEs_",linea,sep="")
    linDir<-file.path(resDir,foldername)
    outDir<-file.path(linDir,"GOenrichment_basedOnDEs")
    dir.create(outDir, showWarnings = FALSE, recursive = TRUE)
    
    
    #######################################################################
    ### DEs PER SINGOLA CONDITION
    #######################################################################
    
    for (j in 1:length(confronti)){
      confronto<-confronti[j]
      filename<-paste("FoldChanges&padjs_",linea,"-", confronto,".html", sep="")
      filePath<-file.path(linDir,"featuresSelection",filename)
      #filePath<-file.choose()
      testTable<-readHTMLTable(doc=filePath, header = TRUE)[[1]]
      names(testTable)<-str_trim(names(testTable))
      testTable<-testTable[testTable$'log2FoldChange if DE'!="Not DE for this test",]
      if (dim(testTable)[1]==0) next
      testTable$log2FoldChange<-as.numeric(testTable$log2FoldChange)
      
      # UP-REGULATED GENES
      boolean<-testTable$log2FoldChange>0
      selectedGenes<-testTable$ID_REF[boolean]
      scoreTable<-generaScoreTable(object="DE",exprSet,selectedGenes)
      titolo<-paste(linea,"_",confronto,"_UP", sep="")
      try(
        callTopGO(scoreTable, outDir=file.path(outDir,confronto,"_UP"),
                  GO2gene,ntop, titleForGmtFile=titolo,resDir,
                  e=e)
      )
      # DOWN-REGULATED GENES
      boolean<-testTable$log2FoldChange<0
      selectedGenes<-testTable$ID_REF[boolean]
      scoreTable<-generaScoreTable(object="DE",exprSet,selectedGenes)
      titolo<-paste(linea,"_",confronto,"_DOWN", sep="")
      try(
        callTopGO(scoreTable, outDir=file.path(outDir,confronto,"_DOWN"),
                  GO2gene,ntop, titleForGmtFile=titolo,resDir,
                  e=e)
      )
      
      # ALL DE GENES
      selectedGenes<-testTable$ID_REF
      scoreTable<-generaScoreTable(object="DE",exprSet,selectedGenes)
      titolo<-paste(linea,"_",confronto,"_BOTH", sep="")
      try(
        callTopGO(scoreTable, outDir=file.path(outDir,confronto,"_BOTH"),
                  GO2gene,ntop, titleForGmtFile=titolo,resDir,
                  e=e)
      )
      
    }#close ciclo trattamenti
    
  }#close for cycle delle linee
}
