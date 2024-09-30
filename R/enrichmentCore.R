

enrichmentCore<-function(scoreTable, GO2gene, outDir, 
                         e=globalenv()) {
  
  ########################################################################
  ### DEFINE GENE UNIVERSE
  #######################################################################
  perc <- get0("perc", envir = e, ifnotfound = NULL) 
  #print(paste("percentage to select threshold for significant genes:",perc))
  lista<-createGeneUniverse(scoreTable=scoreTable, perc, outDir, e=e)
  makeGlobalThresholdSig(myVar=lista[[1]], e=e) # variabile globale poiche' utilizzata implicitamente in funzioni
  geneUniverse<-lista[[2]]
  rm(lista)
  thresholdSig <- get0("thresholdSig", envir = e, ifnotfound = NULL) 
  #print(paste("threshold dei significativi: ",thresholdSig))
  
  N_sig<-printGeneUniverse(geneUniverse)
  if (N_sig<10){
    print("pochi geni significativi. Termino esecuzione")
    return(list(NULL,NULL))
  }
  
  ########################################################################
  ### CREO OGGETTO topGO
  #######################################################################
  
  
  ### CREA OGGETTO topGO
  
  #crea un oggetto avendo eseguto un test statistico effettuato altrove   
  GOobject <- new("topGOdata",description = "fromExpressionSet", ontology = "BP",
                  allGenes = geneUniverse, geneSel = topGenes,  nodeSize = 5, #pruning parameter
                  #l'universo è composto da una lista di geni. Tra questi alcuni 
                  #vengono selezionati via topDiffGenes
                  annot =annFUN.GO2genes, GO2genes=GO2gene) 
  print("topGO object was created:")
  print(GOobject)
  #annFun.db è la funzione utilizzata per l'arricchimento: ad ogni GO 
  #term associa un vettore di geni, tra quelli dati in input
  
  
  
  ########################################################################
  ### RUN ENRICHMENT TESTS
  #######################################################################
  #Fisher’s exact test which is based on gene counts, and a Kolmogorov-Smirnov like test 
  #which computes enrichment based on gene scores. We can use both these tests since each gene has a
  #score (representing how differentially expressed a gene is) and by the means of topDiffGenes functions the
  #genes are categorized into differentially expressed or not differentially expressed genes.
  
  # AVAILABLE TESTS AND ALGORITHMS
  #elencare i test disponibili
  #whichTests()
  #whichAlgorithms()
  
  
  lista<-runEnrichmentTests(sampleGOdata=GOobject,outDir)
  resultsTogether<-lista[[1]]
  resultsNames<-lista[[2]]
  rm(lista)
  attributes(resultsTogether)$names<-resultsNames
  
  printTestResults(results=resultsTogether,sampleGOdata=GOobject,GO2gene,outDir) 
    
  correlazioni(results=resultsTogether,sampleGOdata=GOobject,outDir)
  
  lista<-list(GOobject, resultsTogether )
  return(lista)
}  
 
