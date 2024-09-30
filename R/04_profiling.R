

profiling<-function(inDir,  DESeqDir, cellLines, confronti){

 
  outDir<-file.path(inDir,"profiling")
  dir.create(outDir, showWarnings = FALSE, recursive = TRUE)
  
  lista<-creaOggettiDEs(inDir,cellLines, confronti)
  DEids<-lista[[1]]
  DEseqs<-lista[[2]]
  
  filePath<-file.path(outDir,"DEids.rds")
  saveRDS(DEids, file = filePath)
  filePath<-file.path(outDir,"DEseqs.rds")
  saveRDS(DEseqs, file = filePath)
  
  
  ############################################################################
  ### SCRIVI IN FORMATO TSV
  ############################################################################
  
  allDEs<-unique(unlist(DEids))
  N<-length(allDEs)
  DEs<-data.frame(padding=rep("",length(allDEs)))
  nomiColonne<-c()
  for (i in 1:length(attributes(DEids)$names)) {
    nome1<-attributes(DEids)$names[i]
    for (j in 1: length(attributes(DEids[[i]])$names)){
      nome2<-attributes(DEids[[i]])$names[j]
      nome<-paste(nome1, nome2, sep="_")
      nomiColonne<-c(nomiColonne, nome)
      padding<-rep("",(N-length(DEids[[i]][[j]])))
      nuovaColonna<-c(DEids[[i]][[j]], padding)
      DEs<-cbind(DEs,nuovaColonna)
    }#close inner loop
  }#close outer loop
  DEs<-DEs[,-1]
  names(DEs)<-nomiColonne
  filePath<-file.path(outDir,"profiles.tsv")
  write.table(x=DEs, file = filePath, append = FALSE, quote = FALSE, sep = "\t", row.names = FALSE)
  rm(DEs)
  rm(allDEs)
  
  ############################################################################
  ### GENERA SCACCHIERA
  ############################################################################
  
  generaScacchiera( DESeqDir, cellLines, confronti, inDir, outDir, DEids)

  return(outDir)
}
