goEnrichmentForLists<-function( resDir,  dataDir, 
                                listsOfInterest,exprSet,GO2gene, ntop,
                                e=globalenv()) {

  
  outDir<-file.path(resDir,"GOenrichment_basedOnLists")
  dir.create(outDir, showWarnings = FALSE, recursive = TRUE)
  
  for (c in 1:length(listsOfInterest)) {
    listFile<-listsOfInterest[c]
    listName<-sub(pattern="\\.txt", replacement="", x=listFile)
    listName<-sub(pattern="\\.tsv", replacement="", x=listName)
    print("list under examination:")
    print(listName)
    
    #######################################################################
    ### FOLLOWING LIST 
    #######################################################################
    
    filePath<-file.path(dataDir, listFile)
    selectedGenes<-NULL
    #gestione lista mancante
    tryCatch( 
      selectedGenes<- unique(scan(file = filePath, what = character(), 
            sep = "\n",	strip.white = TRUE, quiet = FALSE, 
            blank.lines.skip = TRUE)),
      error = function(err) {
        print(paste("gene lists",listName,"not found"))
      })
    if (is.null(selectedGenes) || length(selectedGenes)<10 ) {
      print("the number of uploaded significant genes is too limited. Next list")
      next
      }
    
    scoreTable<-generaScoreTable(object="lists",exprSet,selectedGenes)
    titolo<-paste("in_list_", listName, sep="")
    try(
      callTopGO(scoreTable, outDir=file.path(outDir,listName),
                GO2gene,ntop, titleForGmtFile=titolo,resDir,
                e=e)
    )
      
    }#close for loop delle liste
  
}



