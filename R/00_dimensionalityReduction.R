#' @importFrom e1071 svm tune.svm
#' @importFrom factoextra fviz_eig fviz_pca_biplot fviz_pca_ind fviz_pca_var
#'   get_eigenvalue  get_pca_ind get_pca_var

myDimensionalityReduction<-function(resDir,  cellLines, confronti, 
                                    coldata, e=globalenv()){
  
  ### SETTINGS
  lista<-settingsXggplots(coldata,cellLines)
  coloriAsFactor<-lista[[1]]
  pointsizeAsFactor<-lista[[2]]
  shapeAsFactor<-lista[[3]]
  w<-lista[[4]]
  h<-lista[[5]]
  res<-lista[[6]]
  legenda<-lista[[7]]
  rm(lista)
  
  ####################################################################
  #### CICLO
  ################################################################
  consideredCases<-c("union",cellLines)
  for (z in 1: length(consideredCases)) {
    linea<-consideredCases[z]
    #print(paste("caso analizzato:",linea))
    foldername<-paste("DEs_",linea,sep="")
    linDir<-file.path(resDir,foldername)
    inDir<-file.path(linDir,"featuresSelection")
    
    ### PCA
    print("principal component analysis")
    outDir<-file.path(linDir,"dimensionalityReduction","PCA")
    dir.create(outDir, showWarnings = FALSE, recursive = TRUE)
    lista<-myPCA(inDir, outDir,  coloriAsFactor, pointsizeAsFactor, shapeAsFactor,
                 w, h, res, legenda, coldata)
    
    ###### CONTRIBUTIONS TO SINGLE PCA COMPONENTS
    res.pca<-lista[[1]]
    N<-lista[[2]]
    rm(lista)
    print("print variables importance")
    printVariablesImportance(outDir=file.path(outDir,"variablesImportance"),
                               resDir, res.pca, N, inDir,linea,
                             e=e)
    
    ### tSNE
    print("tSNE dimensionality reduction")
    outDir<-file.path(linDir,"dimensionalityReduction","tSNE")
    dir.create(outDir, showWarnings = FALSE, recursive = TRUE)
    tSNE2D<- myTsne(inDir, outDir,  coloriAsFactor, pointsizeAsFactor, shapeAsFactor,
                    w, h, res,legenda)
    
    ### SUPPORT VECTOR MACHINE
    print(paste("SVM based on tSNE results"))
    SVMbasedOnTsne(tSNE2D, outDir,  coloriAsFactor, pointsizeAsFactor, shapeAsFactor,
                   w, h, res,legenda)
    
    ### UMAP
    print("umap dimensionality reduction")
    outDir<-file.path(linDir,"dimensionalityReduction","UMAP",sep="")
    dir.create(outDir, showWarnings = FALSE, recursive = TRUE)
    myUmap(inDir, outDir,  coloriAsFactor, pointsizeAsFactor, shapeAsFactor,
           w, h, res,legenda)
    
  }#chiude ciclo linee

}
