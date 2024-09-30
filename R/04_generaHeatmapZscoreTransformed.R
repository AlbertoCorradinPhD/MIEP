generaHeatmapZscoreTransformed<-function(geneList, geneset, inDir, outDir,   
                                         splitMethod, rowS, cluster_columns){
  
  
  filePath<-file.path(inDir,"dataML_ZscoreTransformed.rds")
  dataML_ZscoreTransformed<- readRDS(filePath) 
  
  #estrai subset di dati
  lista<- fromDEs(dataML=dataML_ZscoreTransformed, geneList)
  dataML<- lista[[1]]
  numberOfFeatures<- lista[[2]]
  rm(lista)
  
  #escape case
  if (numberOfFeatures<1){
    print("nessun gene del geneset tra i differenzialmente espressi")
    return()
  }
  
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
  
  
  titolo<-paste("Differentially expressed features in:", geneset)
  titolo<-parseName(parola=titolo)
  legenda<-"Levels of expression"
  
  
  #cambia outDir
  outDir<-file.path(outDir,"heatmaps")
  dir.create(outDir, showWarnings = FALSE, recursive = TRUE)
  
  #cambia outDir
  outDir<-file.path(outDir,"DEonly_afterBiasCorrection")
  dir.create(outDir, showWarnings = FALSE, recursive = TRUE)
  
  #heatmap classic
  filePath<-file.path(outDir,"heatmapDEonly_classic.tiff")
  generaHeatmap(filePath, M=X,w,h,res, f_row,f_col,ft_col,ft_row,fl,
          metric,clusteringMethod,splitMethod,rowS, 
          cluster_columns, titolo,legenda)
  
  
  #heatmap custom colors
  col_fun<-customColors_ZscoreTransformed()
  filePath<-file.path(outDir,"heatmapDEonly_customColors.tiff")
  generaHeatmap(filePath, M=X,w,h,res, f_row,f_col,ft_col,ft_row,fl,metric,clusteringMethod,
          splitMethod, rowS, cluster_columns,
          titolo, legenda, col_fun=col_fun)#evito split colonne
  

}