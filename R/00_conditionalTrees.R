#' @importFrom party ctree_control cforest_control cforest ctree varimp 
#' @importFrom party node_inner node_barplot edge_simple
#' @importFrom caret confusionMatrix
#' @import doParallel
#' @import parallel
#' @import pbapply

myRandomForests<-function(resDir,  cellLines, confronti,
                          metodoDiClassificazione, classi,
                          e=globalenv()){
  
  # read data
  linDir<-file.path(resDir,"DEs_union")
  inDir<-file.path(linDir,"featuresSelection")
  filePath<-file.path(inDir,"dataML_ZscoreTransformed.rds")
  dataML<-readRDS(file=filePath)
  # create outDir
  outDir<-file.path(linDir,"conditionalRandomForests")
  dir.create(outDir, showWarnings = FALSE, recursive = TRUE)
  
  #plot settings
  h<-5000
  r<-500
  
  ### EVENTUALE subsetting
  #if (FALSE) {
  #  sottoclassi<-c( "treated_CEM","untreated_CEM")
  #  boolean<-classi %in% sottoclassi 
  #  geneset<- NULL#"topFeaturesClassicByCustom"    per il secondo passaggio
  #  dataML<-subsetting(dataML, boolean, resDir,  geneset, e=e) 
  #  classi<-classi[boolean]
  #  metodoDiClassificazione<-"CEM"
  #} 
  
  ### DATA PREPARATION
  print("Prepare data")
  lista<-dataPreparation(dataML, outDir,pwd=NULL, 
                         metodoDiClassificazione, classi)
  X <-lista[[1]]
  metodoDiClassificazione<-lista[[2]]
  outDir<-lista[[3]]
  w<-lista[[4]]
  colori<-lista[[5]]
  pwd<-lista[[6]]
  
  
  
  ### GET BEST TREE AND FOREST
  print("Get best tree and best random Forest")
  bestForest<-findBestForest(X, outDir, w,h,r, colori, 
                             pwd=pwd, e=e)
  
  
  ### CALCULATE CLASSIC VARIABLE IMPORTANCE
  #filePath<-file.choose()
  #bestForest<-readRDS(filePath)
  
  print("Let's calculate classic variable importance")
  newDir<-file.path(outDir,"topFeaturesClassic")
  nperm<-1000
  conditional<-FALSE
  OOB<-FALSE
  selectedFeatures<-variablesImportance(bestForest,w,h,r,colori, 
                                        nperm,conditional,OOB, outDir=newDir, 
                                        pwd)
  if (!is.null( selectedFeatures) ) {
    title<-paste("topFeaturesClassicBy",metodoDiClassificazione, sep="")
    description<-"> from classic variable importance"
    contenuto<-genesetCreaGMTfiles(name=title,title=title, 
                                 description=description, genes=selectedFeatures,
                                 outDir=newDir)
    genesetRegister(name=title,resDir,contenuto )
  }
  
  
  ### CALCULATE CONDITIONAL VARIABLE IMPORTANCE
  #filePath<-file.choose()
  #bestForest<-readRDS(filePath)
  
  print("Let's calculate conditional variable importance")
  newDir<-file.path(outDir,"topFeaturesConditional")
  nperm<-10
  conditional<-TRUE
  OOB<-FALSE
  selectedFeatures<-variablesImportance(bestForest,w,h,r,colori, 
                                        nperm,conditional,OOB, outDir=newDir, 
                                        pwd)
  if (!is.null( selectedFeatures) ) {
    ### CREATE .GMT FILE
    title<-paste("topFeaturesConditionalBy",metodoDiClassificazione,sep="")
    description<-"> from conditional variable importance"
    contenuto<-genesetCreaGMTfiles(name=title,title=title, 
                                 description=description, genes=selectedFeatures,
                                 outDir=newDir)
    genesetRegister(name=title,resDir,contenuto )
  }

 }
