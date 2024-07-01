exploringGraph<-function(wantedNodes,sampleGOdata,resDir,
                         titleForGmtFile, wantedTerms, outDir, resName) {
  
  ####### EXPLORING THE GRAPH
  for (n in 1:length(wantedNodes)){
    nodo<-wantedNodes[n]
    term<-wantedTerms[n]
    #print(nodo)
    #num.ann.genes <- countGenesInTerm(object = sampleGOdata,whichGO = nodo) ## the number of annotated genes
    #print(paste("number of annotated genes:",num.ann.genes))
    ann.genes <- genesInTerm(sampleGOdata, nodo) ## get the annotations
    #head(ann.genes)
    #str(ann.genes)
    boolean<- ann.genes[[1]] %in% sigGenes(sampleGOdata)
    significantGenes<-ann.genes[[1]][boolean]
    #check
    #print(paste("numero geni significativi:",sum(boolean)))
    #print(ann.genes[[1]][boolean]  )
    
    ### CREATE .GMT FILE
    #rimpiazzo gli spazi con trattini
    while (grepl(pattern=" ", x=term)){
      term<-gsub(pattern=" ", replacement="_", x=term) 
    }
    
    title<-paste(titleForGmtFile,"_",term,sep="")
    description<-paste(">", nodo,"from GO enrichment test")
    name<-title
    contenuto<-genesetCreaGMTfiles(name,title, 
                                 description, genes=significantGenes,
                                 outDir=outDir)
    if (resName=="Fisher") {#registro solo per Fisher test
      genesetRegister(name,resDir,contenuto)
   }
  }# chiude ciclo wantedNodes

}
