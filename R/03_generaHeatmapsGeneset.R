generaHeatmapsGeneset<-function(geneList, geneset,Labels, inDir, outDir,  
                                         splitMethod, rowS=NULL){
  
  #cambia outDir
  outDir<-file.path(outDir,"heatmaps")
  dir.create(outDir, showWarnings = FALSE, recursive = TRUE)

  #estrai subset di dati
  lista<- fromNormalizedCounts(geneList, Labels, inDir, outDir)
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
  
  
  titolo<-paste("Heatmap of counts in geneset:", geneset)
  titolo<-parseName(parola=titolo)
 

  
  if ( (max(X)-min(X)) >1e4) {
    M<-log2(X+1)
    col_fun<-customColors_counts()
    legenda<-"log2(counts)"
    
    #heatmap custom colors
    filePath<-file.path(outDir,"heatmap_customColors.tiff")
    generaHeatmap(filePath, M=M,w,h,res, f_row,f_col,ft_col,ft_row,fl,
            metric,clusteringMethod,splitMethod,rowS, cluster_columns=FALSE,
            titolo,legenda, col_fun=col_fun)
  } else {
    M<-X
    col_fun<-customColors_counts()
    legenda<-"Counts"
    
    #heatmap decili
    col_fun<-deciliColors(M=M)
    filePath<-file.path(outDir,"heatmap_deciliColors.tiff")
    generaHeatmap(filePath, M=M,w,h,res, f_row,f_col,ft_col,ft_row,fl,
            metric,clusteringMethod,splitMethod,rowS, cluster_columns=FALSE,
            titolo,legenda, col_fun=col_fun)
    }
 
  #heatmap classic
  filePath<-file.path(outDir,"heatmap_classic.tiff")
  generaHeatmap(filePath,M=M,w,h,res, f_row,f_col,ft_col,ft_row,fl,
          metric,clusteringMethod,splitMethod,rowS, cluster_columns=FALSE, 
          titolo,legenda)
 

}