myAnnotations<-function(resDir,  dataDir, DESeqDir, cellLines, confronti){
  
  
  ################################################################
  # ANNOTATIONS CHAPTER 
  ################################################################
  
  
  ### RETRIEVE ANNOTATIONS FROM DATABASE AND CLEAN THEM
  inDir<-dataDir
  outDir<-file.path(resDir,"normalizedCounts")
  lista<-list(inDir1=outDir)
  dir.create(outDir, showWarnings = FALSE, recursive = TRUE)
  annotationsBiomaRt(inDir,outDir)
  
  ### KEEP ONLY SEQUENCES WITH A NAME
  selectSequencesWithName(inDir=outDir,outDir,DESeqDir)
  
  
  ### SELECT DE SEQUENCES WITH A NAME
  outDir<-selectDEsequencesWithName(inDir=outDir,  DESeqDir, cellLines, confronti)
  
  ### PROFILING
  outDir<-profiling(inDir=outDir,  DESeqDir, cellLines, confronti)
  lista<-list.append(lista,inDir2=outDir)
  
  return(lista)
}
