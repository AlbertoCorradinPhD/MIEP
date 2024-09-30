ontologyCall<-function(exprSet,  outDir){
  
  GO2gene<-NULL
 
  #ontology: to do or not?
  myLabel<-("Should gene ontology information be collected? (Tick at first round)")
  input<-NULL
  while (is.null(input)){
    input<-shinyBooleanInput(myLabel)
  }
  ontology<-input
  if (ontology)  {
    ### CREATE FILES OF GENE ONTOLOGY
    lista<-createGeneOntology(exprSet, outDir)
    gene2GO<-lista[[1]]
    filePath<-file.path(outDir,"gene2GO.rds")
    saveRDS(gene2GO, file= filePath)
    GO2gene<-lista[[2]]
    filePath<-file.path(outDir,"GO2gene.rds")
    saveRDS(GO2gene, file =filePath)
    GOBP_table<-lista[[3]]
    filePath<-file.path(outDir,"GOBP_table.rds")
    saveRDS(GOBP_table, file =filePath)
    print("gene ontology construction accomplished")
  } else  {
    ### READ FILES OF GENE ONTOLOGY
    filePath<-file.path(outDir,"GO2gene.rds")
    GO2gene<-readRDS(filePath)
    print("reading phase accomplished")
  }
  
  return(GO2gene)
  
}
