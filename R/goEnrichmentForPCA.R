goEnrichmentForPCA<-function( resDir,  exprSet,
                              cellLines,GO2gene,ntop, N,
                              e=globalenv()) {
  
  for (c in 1:length(cellLines)) {
    linea<-cellLines[c]
    print(paste("cell line under examination:", linea))
    foldername<-paste("DEs_",linea,sep="")
    linDir<-file.path(resDir,foldername)
    outDir<-file.path(linDir,"GOenrichment_basedOnPCA")
    dir.create(outDir, showWarnings = FALSE, recursive = TRUE)
    
    for (pc in 1:N){
      filename<-paste("PC",pc,"_contributions.tsv",sep="")
      filePath<-file.path(linDir,"dimensionalityReduction","PCA",
                          "variablesImportance", filename)
      tryCatch({      
        selectedGenes<- read.table(filePath,header = TRUE,sep = "\t")$feature
        importance<-read.table(filePath,header = TRUE,sep = "\t")$importance
        scoreTable<-generaScoreTable(object="PCA",exprSet,selectedGenes, importance) 
        
        },  error = function(err){
          frase<-paste("I could not find file with values of importance for PC",
                       pc,"- cell line",linea)
          print(frase)
        })#chiudo tryCatch

      titolo<-paste(linea,"_pc",pc,sep="")
      foldername<-paste("byPC", pc, sep="")
      filePath<-file.path(outDir, foldername)
      try(
        callTopGO(scoreTable, outDir=filePath,
                  GO2gene,ntop, titleForGmtFile=titolo, resDir,
                  e=e)
      )
    } # close for cycle principal component
    
  }#close for cycle delle linee
}



