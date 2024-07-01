#' @importFrom dendextend set rect.dendrogram cutree

generaDendrograms<-function(inDir, outDir, splitMethod, rowS, linea) {
  
  filePath<-file.path(outDir,"dataML_ZscoreTransformed.rds")
  dataML<-readRDS(file=filePath)
  numberOfFeatures<-dim(dataML)[2]-1
  
  
  outDir<-file.path(outDir,"heatmaps","afterBiasCorrection","dendros")
  dir.create(outDir, showWarnings = FALSE, recursive = TRUE)
  
  
  ###############################################################################
  ##### SETTINGS
  ###############################################################################
  
  #graphical settings
  listaPars<-settingsXdendros(numberOfFeatures)
  w<-listaPars[[1]]
  h<-listaPars[[2]]
  res<-listaPars[[3]]
  cex<-listaPars[[4]]
  f_row<-listaPars[[5]]
  metric<-listaPars[[6]]
  clusteringMethod<-listaPars[[7]]
  rm(listaPars)
  
  
  #PREPARA LA MATRICE DI DATI
  X<-prepareMatrixForHeatmap(dataML,splitMethod)
  
  #########################################################################
  ### DENDROGRAMS
  #########################################################################
  
  # N_clusters
  N_clusters<-rowS
  #print("number of clusters:")
  #print(N_clusters)
  
  ############# BASIC DENDROGRAM ######################################

  dend<-dendroBasic(X,metric,clusteringMethod,w,h,res, cex,outDir)
  
  ############## HIGHLIGHT CLUSTERS ######################################
  
  #COLOR PALETTE
  colori<- colorPalette()
  #print(colori)
  
  set.seed(666)
  clustersColors<-sample(colori,N_clusters)
  #print(clustersColors)
  shadedCluster<-1 #valore iniziale
  repeat {
    dendroClusters(dend,metric,clusteringMethod,w,h,res,f_row,
                 clustersColors,N_clusters,shadedCluster, outDir)
    lista<-changeShadedCluster(shadedCluster,linea)
    ans<-lista[[1]]
    shadedCluster<-lista[[2]]
    if (ans){break}
  }
  
  
  ################################################################################
  ### CLUSTERS IDENTIFICATION 
  ################################################################################
  
  cutoff<-25 #numero minimo di geni nel cluster
  clustersIdentification(dend,inDir,outDir,cutoff, N_clusters)

  
}
  

