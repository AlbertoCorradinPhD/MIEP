generaHeatmaps<-function(inDir, outDir,splitMethod, 
                         rowS, cluster_columns, caseOfInterest) {
 
  filePath<-file.path(inDir,"dataML_counts.rds")
  dataML<-readRDS(file=filePath)
  numberOfFeatures<-dim(dataML)[2]-1
  
  ###############################################################################
  ##### SETTINGS
  ###############################################################################
  
  
  #graphical settings
  listaPars<-settingsXheatmaps(numberOfFeatures)
  w<-listaPars[[1]]
  h<-listaPars[[2]]
  res<-listaPars[[3]]
  f_row<-listaPars[[4]]
  f_col<-listaPars[[5]]
  ft_col<-listaPars[[6]]
  ft_row<-listaPars[[7]]
  fl<- listaPars[[8]]
  metric<-listaPars[[9]]
  clusteringMethod<-listaPars[[10]]
  rm(listaPars)
  
  #PREPARA LA MATRICE DI DATI
  X<-prepareMatrixForHeatmap(dataML,splitMethod)
  
  #########################################################################
  ### HEATMAPS
  #########################################################################
  
  titolo<-paste("Differentially expressed features in:", caseOfInterest)
  legenda<-"Log2(counts)"
  
  #heatmap classic
  filePath<-file.path(outDir,"heatmap_classic.tiff")
  generaHeatmap(filePath, M=log2(X+1),w,h,res, f_row,f_col,ft_col,ft_row,fl,
          metric,clusteringMethod,splitMethod,rowS,
          cluster_columns, titolo,legenda)

  
  #heatmap custom colors
  col_fun<-customColors_counts()
  filePath<-file.path(outDir,"heatmap_customColors.tiff")
  generaHeatmap(filePath, M=log2(X+1),w,h,res, f_row,f_col,ft_col,ft_row,fl,
          metric,clusteringMethod,splitMethod,rowS, 
          cluster_columns, titolo,legenda, col_fun=col_fun)
  
  #heatmap decili
  #col_fun<-deciliColors(M=log2(X+1))
  #filePath<-file.path(outDir,"heatmap_deciliColors.tiff")
  #generaHeatmap(filePath, M=log2(X+1),w,h,res, f_row,f_col,ft_col,ft_row,fl,
  #        metric,clusteringMethod,splitMethod,rowS, titolo,legenda, 
  #        col_fun=col_fun)
  
  
}



