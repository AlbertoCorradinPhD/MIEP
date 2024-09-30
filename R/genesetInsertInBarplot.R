genesetInsertInBarplot<-function( resDir, 
                                 geneset=NULL, e=globalenv()){
  
  suppressWarnings( geneList<-genesetScan(geneset, resDir, e=e) )
   
  legenda<-NULL
  
  if (!is.null(geneList)) {
    legenda<-paste("in", geneset,"?")
    legenda<-parseName(parola=legenda)
  }
  
  lista<-list(legenda,geneList)
  return(lista)
}
