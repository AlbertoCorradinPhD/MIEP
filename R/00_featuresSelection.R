

myFeaturesSelection<-function(resDir,  inDir1, inDir2, DESeqDir, 
                              cellLines, confronti, coldata){
  
  splitMethod<-classificationMethod(coldata,input="both")[[1]]

  
  ####################################################################
  #### CICLO
  ################################################################
  consideredCases<-c("union",cellLines)
  
  for (z in 1: length(consideredCases)) {
    linea<-consideredCases[z]
    #print(paste("caso analizzato:",linea))
    foldername<-paste("DEs_",linea,sep="")
    linDir<-file.path(resDir, foldername)
    outDir<-file.path(linDir,"featuresSelection")
    dir.create(outDir, showWarnings = FALSE, recursive = TRUE)
    
    ### COLLECT DIFFERENTIALLY EXPRESSED GENES
    #print(paste("raccogli geni differenzialmente espressi"))
    filePath<-file.path(inDir2,"DEids.rds")
    DEids<-readRDS(filePath)
    filePath<-file.path(inDir2,"DEseqs.rds")
    DEseqs<-readRDS(filePath)
    collectDEseqs(DEids, DEseqs, inDir=inDir1, cellLines, confronti, 
                  outDir, linea)
    
    ### COLLECT TEST RESULTS
    #print(paste("raccogli risultati dei test statistici"))
    collectTestResults(DESeqDir, cellLines, confronti, outDir, consideredCase=linea)
    
    
    ### MAKE DATA ML
    #print(paste("applica Z-score trasform"))
    Labels<-classificationMethod(coldata,input="condition")[[1]]
    makeDataML( inDir=outDir, coldata, Labels, linea)
    
    ### HEATMAPS CONTE 
    #print(paste("genera heatmaps conte"))
    naturalDir<-file.path(outDir,"heatmaps","beforeBiasCorrection","clusteredColumns")
    dir.create(naturalDir, showWarnings = FALSE, recursive = TRUE)
    orderedDir<-file.path(outDir,"heatmaps","beforeBiasCorrection","prearrangedOrderForColumns")
    dir.create(orderedDir, showWarnings = FALSE, recursive = TRUE)
    #cluster columns
    generaHeatmaps(inDir=outDir,  outDir=naturalDir,splitMethod,
                   rowS=NULL, cluster_columns = TRUE, linea)
    #don't cluster columns
    generaHeatmaps(inDir=outDir, outDir= orderedDir,splitMethod,
                   rowS=NULL, cluster_columns = FALSE, linea)
    
    ### HEATMAPS CONTE ZSCORE TRANSFORMED
    #print(paste("genera heatmaps valori Z-score trasformed"))
    naturalDir<-file.path(outDir,"heatmaps","afterBiasCorrection","clusteredColumns")
    dir.create(naturalDir, showWarnings = FALSE, recursive = TRUE)
    orderedDir<-file.path(outDir,"heatmaps","afterBiasCorrection","prearrangedOrderForColumns")
    dir.create(orderedDir, showWarnings = FALSE, recursive = TRUE)
    rowS<-NULL #valore iniziale
    repeat {
      generaHeatmaps( inDir=outDir, outDir=naturalDir, splitMethod, 
                      rowS=rowS, cluster_columns=TRUE, linea)
      generaHeatmaps( inDir=outDir, outDir= orderedDir, splitMethod, 
                      rowS=rowS, cluster_columns=FALSE, linea)
      lista<-changeRowS(rowS,linea)
      ans<-lista[[1]]
      rowS<-lista[[2]]
      if (ans){break}
    }#chiude repeat 
    
    ### DENDROGRAMS
    if (!is.null(rowS))  {
      #print(paste("genera corrispondenti dendrogrammi"))
      generaDendrograms(inDir=inDir1,
                      outDir, splitMethod, rowS, linea)
    }#chiude if 
  }# chiude loop cell lines
}
