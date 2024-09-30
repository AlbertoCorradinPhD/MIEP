

createGeneOntology<-function(exprSet, outDir) {

  ### GO TERMS RETRIEVAL 
  #salvo risultati in folder "annotation_run"
  GO_table<-GOterms_retrieval(exprSet=exprSet)
  #class(GO_table)
  filePath<-file.path(outDir,"GO_table.rds")
  saveRDS(GO_table, file = filePath)
  
  
  ### CREATE MAPS GENES2GO E GO2GENES 
  lista<-selectBP_GOterms(GO_table, outDir)
  names(lista)
  return(lista)
}