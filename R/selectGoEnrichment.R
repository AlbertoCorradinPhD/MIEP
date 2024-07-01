selectGoEnrichment<-function(object, resDir,  dataDir, exprSet,cellLines,
                              confronti,GO2gene, ntop, e=globalenv()){
  
  #######################################################################################
  ### IDENTIFY THE CASE
  #######################################################################################
 
  if (object=="lists") {
    tryCatch({
      filePath<-file.path(dataDir,"listsOfInterest.txt")
      listsOfInterest<-unique(scan(file = filePath,sep = "\n",what = "character"))
      print("lists of interest:")
      print(listsOfInterest)
      goEnrichmentForLists( resDir,  dataDir, listsOfInterest,
                          exprSet,GO2gene, ntop, e=e)},
      error = function(err){
       print("no file listsOfInterest.txt found. I pass")
      })#chiudo tryCatch
  } 
  
  if (object=="DE") {
    goEnrichmentForDE( resDir, exprSet,cellLines,
                       GO2gene, ntop, confronti,e=e)
  }  
  
  if (object=="PCA") {
    #numero di componenti principali da considerare
    myLabel<-("insert number of principal components of concern")
    input<-NULL
    while (is.null(input)){
      input<-shinyNumInput(myLabel, value=1)
    }
    N<-input
    goEnrichmentForPCA(resDir,  exprSet,cellLines,
                       GO2gene, ntop, N, e=e) 
  } 
  
}