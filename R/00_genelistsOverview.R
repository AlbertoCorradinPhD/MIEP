

myOverview<-function(resDir, DESeqDir, genesetsList, cellLines,  
                     confronti, coldata, flag, e=globalenv()){
  
  
  
  linDir<-file.path(resDir,"DEs_union")
  overviewDir<-file.path(resDir,"genelistsOverview")
  dir.create(overviewDir, showWarnings = FALSE, recursive = TRUE)
  splitMethod<-classificationMethod(coldata,input="both")[[1]]
  lista<-settingsXvolcanoPlot()
  yLimit<-lista[[1]]
  xLimit<-lista[[2]]
  rm(lista)
  conditions<-unique(coldata$condition)
  
  for (k in 1:length(genesetsList)){
    geneset<-genesetsList[k]
    
    suppressWarnings(
      geneList<-genesetScan(geneset, resDir, e=e)
    )
    if (is.null(geneList)) next
    
    #altrimenti procedi
    parsedName<-parseName(geneset)
    print(paste("geneset upon consideration:",parsedName))
    #create  outDir
    outDir<-file.path(overviewDir,geneset)
    outDir<-parseName(outDir, isDir=TRUE)
    dir.create(outDir, showWarnings = FALSE, recursive = TRUE)
    
    
    ### VOLCANO PLOTS
    #print("genera Volcano plots")
    inDir<-file.path(resDir,"normalizedCounts")
    suppressWarnings(
      inDir<-generaVolcanoPlots( geneList, yLimit, xLimit, cellLines, confronti,
                                  inDir,outDir,DESeqDir)
    )
    rankedDotplots(parsedName, cellLines, confronti, inDir)
    
    #print("genera heatmaps for fold changes")
    suppressWarnings(
      generaHeatmapsFC(geneList, geneset,inDir, outDir,  splitMethod, rowS=NULL)
    )
    
    ### HEATMAPS GENESET CONTE
    #print("genera heatmap geneset")
    inDir<-file.path(resDir,"normalizedCounts")
    Labels<-classificationMethod(coldata,input="condition")[[1]]
    generaHeatmapsGeneset(geneList, geneset,Labels,inDir, outDir,  splitMethod)
    
    ### HEATMAPS Zscore TRANSFORMED - DE_only
    #print("genera heatmap geni differenzialmente espressi, Zscore transformed data")
    inDir<-file.path(linDir,"featuresSelection")
    generaHeatmapZscoreTransformed(geneList, geneset, inDir, outDir,  
                                   splitMethod, rowS=NULL, cluster_columns=FALSE)
    
    
    ### GENERA ISTOGRAMMI
    if (flag) {
      print("genera istogrammi")
      inDir<-file.path(resDir,"normalizedCounts")
      generaIstogrammi(geneList, inDir, outDir,  coldata)
    }
    
    
  }#chiude loop genesets


}
