#' @import topGO
#' @importFrom Biobase  ExpressionSet annotation exprs featureNames sampleNames
#'   multiassign


myTopGO<-function(resDir,  dataDir, outDir, exprSet, cellLines, 
                  confronti, e=globalenv()){
  
  
  #### GENE ONTOLOGY
  GO2gene<-ontologyCall(exprSet,  outDir )
  
  
  ### SELECT OBJECT OF ENRICHMENT ANALYSIS
  repeat {
    #choose object of gene analysis
    myLabel<-("insert the object of this analysis (PCA, DE, lists)")
    input<-NULL
    while (is.null(input)){
      input<-shinyTextInput(myLabel)
    }
    if (input %in% c("PCA", "DE","lists")) break
  }
  object<-input #variabile globale per funzioni 
  
  #SETTINGS FOR THE CREATION OF GENE UNIVERSE
  lista<-settingsGeneUniverse(object)
  makeGlobalPerc(myVar=lista[[1]],e=e) # variabile globale poiche' utilizzata implicitamente in funzioni
  ntop<-lista[[2]] 
  rm(lista)
  #check
  #where("perc")
  
  selectGoEnrichment(object, resDir, dataDir, exprSet,
                     cellLines, confronti, GO2gene, ntop,
                     e=e)
  print("end of procedure")

}
