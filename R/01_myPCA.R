myPCA<-function(inDir, outDir,  coloriAsFactor, pointsizeAsFactor, shapeAsFactor,
              w, h, res, legenda, coldata){
  
  filePath<-file.path(inDir,"dataML_ZscoreTransformed.rds")
  dataML<- readRDS(filePath) ## dataML
  names(dataML)
  X<-dataML[,-1]
  row.names(X)<-rownames(dataML)
  
  ###############################################################################
  ###### CALCULATION OF STANDARD PCA
  ###############################################################################
  res.pca <- prcomp(x=X, scale =FALSE, center=FALSE, tol=0)
  
  ##### PLOT VARIANCES AND EIGENVALUES ################
  
  #variances
  p<-variancesPlot(res.pca, outDir, w,h,res)
  explainedVariances<-p$data$eig
  sum<-0
  for (N in 1:length(explainedVariances)){
    sum<-sum+explainedVariances[N]
    if (sum>70 ) {break}
  }
  #caso particolare
  if (N<2) N<-2
  
  #eigenvalues
  p<-eigenvaluesPlot(res.pca, outDir, w,h,res)
  
  ###### RECORD PCA MAPPING
  #print(paste("print pca mapping"))
  mappingDir<-file.path(outDir,"mapping")
  dir.create(mappingDir, showWarnings = FALSE, recursive = TRUE)
  pcaMapping(res.pca, outDir=mappingDir)
  
  ############################################################################
  ###  PLOTS IN 2D 
  ############################################################################
  print(paste("plots in 2D"))
  plotsDir<-file.path(outDir,"plots2D")
  dir.create(plotsDir, showWarnings = FALSE, recursive = TRUE)
  
  suppressWarnings({
    ### SAMPLES
    print(paste("plots of samples"))
    for (i in 1:(N-1)) {
      for (j in (i+1):N) {
        p<-samplesPlot(res.pca,plotsDir, w,h,res,i,j, coloriAsFactor, 
                       shapeAsFactor,legenda, explainedVariances, coldata)
      }
    }
    
    ### PCA VARIABLES
    print(paste("plots of variables"))
    for (i in 1:(N-1)) {
      for (j in (i+1):N) {
        p<-variablesPlot(res.pca,plotsDir, w,h,res,i,j) #chiama custom colors
      }
    }
    
    ### PCA BIPLOTS
    print(paste("biplots"))
    for (i in 1:(N-1)) {
      for (j in (i+1):N) {
        p<-biPlot(res.pca,plotsDir, w,h,res,i,j, coloriAsFactor, pointsizeAsFactor,legenda)
      }
    }
 
  })
  ############################################################################
  ### STANDARD PCA IN 3 DIMENSIONI
  ############################################################################
  print(paste("plot in 3D"))
  res.ind <- get_pca_ind(res.pca)
  coordinates<-res.ind$coord 
  
  dimensioni<-3
  filePath<-file.path(outDir,"PCA3D.tiff")
  suppressWarnings({
    p<-ggplotStandard(filePath, dimensioni, w,h,res, coloriAsFactor,
                    shapeAsFactor, pointsizeAsFactor, legenda, 
                    data=as.data.frame(coordinates)
                    )
  })
  lista<-list(res.pca, N)
  return(lista)
} 