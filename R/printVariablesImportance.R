printVariablesImportance<-function(outDir,  resDir, res.pca, N, 
                                   inDir,linea, e=globalenv() ){
  
  dir.create(outDir, showWarnings = FALSE, recursive = TRUE)
  repeat{
    myLabel<-paste("insert gene set of interest for:",linea)
    input<-NULL
    while (is.null(input)){
      input<-shinyTextInput(myLabel)
    }
    genesetOfInterest<-input
    if (input=="") {
      genesetOfInterest<-NULL
    }
    pcaVariablesImportance(res.pca, outDir=outDir, N, inDir, 
                          resDir, linea,
                         geneset=genesetOfInterest,
                         e=e)
    #check
    myLabel<-paste("Was chosen gene set right for",linea, "? No is default.")
    input<-NULL
    while (is.null(input)){
      input<-shinyBooleanInput(myLabel)
    }
    ans<-input
    if (ans) {break}
  }#close repeat loop
}