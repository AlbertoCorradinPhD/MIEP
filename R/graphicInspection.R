graphicInspection <- function(outDir,DeSeqObj, 
                              resultsObj,testName, nameOfContrast, 
                              padj_max,FC_min,
                              flag_shrinkage="none") {
  
  w<-5000
  h<-5000
  r<-500
  
  ### STATISTICS
  plotStatistics(outDir,resultsObj,testName,w,h,r)
  
  ###########################################################################
  ### MA PLOTS
  ###########################################################################
  
  ### define plot limits
  v1<-min(resultsObj@listData$baseMean)
  if (!is.finite(v1) | v1<1) {v1<-1}
  v2<-max(resultsObj@listData$baseMean)
  xlim_MA<- c(v1,v2)
  v<-max(abs(resultsObj@listData$log2FoldChange))
  if (!is.finite(v)) {v<-15}
  ylim_MA<-c(-v,v)
  try(MAplot(resultsObj,outDir, w,h,r,xlim_MA,ylim_MA,flag_shrinkage))
 
  
  ###########  COMPARISON OF SHRINKAGE METHODS ###########
  if (flag_shrinkage=="none"){
    try(MAplot_shrComparison(outDir,DeSeqObj,resultsObj,nameOfContrast,w,h,r,xlim_MA,ylim_MA))
  }
  
  ###########################################################################
  ######### VOLCANO PLOTS
  ###########################################################################
  #flag_Yaxis<-customYaxis()
  
  main<- parseName(testName)
  data<-data.frame(resultsObj@listData)
  titolo<-"volcanoPlot.tiff"
  filePath<-file.path(outDir,titolo)
  volcanoPlot(filePath, main, data, FC_min, padj_max,w,h,r)
              
  ### WITH BOUNDARIES
  titolo<-"volcanoPlotWithBoundaries.tiff"
  filePath<-file.path(outDir,titolo)
  volcanoPlot(filePath, main, data, FC_min, padj_max,w,h,r,boundaries=TRUE)

  
  }
