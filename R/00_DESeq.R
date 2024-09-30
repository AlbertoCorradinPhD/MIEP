#' @import DESeq2
#' @importFrom SummarizedExperiment assay colData
#' @importFrom GenomicRanges mcols

myDESeq <- function(pipelineDir, dataDir) {
 
  
  lista<-readDataFiles(dataDir)
  originalCounts<-lista[[1]]
  originalColdata <-lista[[2]]
  controls <-lista[[3]]
  cellLines<-lista[[4]]
  conditions <-lista[[5]]
  rm(lista)
  
  #set flag for particular case or erroneous use
  flag_singleCondition<- length(conditions)<2
  if (flag_singleCondition) { 
    print("No conditions to compare. I exit then")
    exit()
  }
  
  #########################################################################
  ### SETTINGS FOR DESEQ RUN
  #########################################################################

  lista<-settingsForRun(pipelineDir)
  flag_MFtest<-lista[[1]]
  flag_shrinkage<-lista[[2]]
  padj_max<-lista[[3]]
  FC_min<-lista[[4]]
  bF_threshold<-lista[[5]]
  cF_threshold<-lista[[6]]
  flag_regionReport<-lista[[7]]
  flag_reportingTools<-lista[[8]]
  DESeqDir<-lista[[9]]
  resDir<-lista[[10]]
  rm(lista)
  
  
  consideredCases<-c("Filter", cellLines)
  for (z in 1:length(consideredCases)){
    
    linea<-consideredCases[z]
    checkConsistentAnalysis(flag_MFtest,flag_cellLine=linea )
    
    if (linea=="Filter") {
      print("Filtering phase has started")
    } else  { print(paste("Cell line under test:", flag_cellLine=linea))}
   
    
    #########################################################################
    ###  DATA IMPORT
    #########################################################################  
    
    print("select data of interest")
    lista<-setDataForTest(originalCounts,originalColdata, 
            flag_cellLine=linea,conditions)
    rawCounts<-lista[[1]]
    coldata<-lista[[2]]
    rm(lista)
    #preview
    print("RNA-seq counts file:")
    print("coldata file:")
    print(coldata)
    print("end of data import chapter")
    
    
    #########################################################################
    ### FILTERING OR MASKING
    #########################################################################
    
    ### DeSeq OBJECT DI PARTENZA 
    dds<-creaOggettoDESeq(rawCounts, coldata)
    print("Initial DESeq object:")
    printDeSeqObj(dds)
    
    dds<-preprocessing(originalCounts, coldata, flag_cellLine=linea, DeSeqObj=dds, 
                       bF_threshold, cF_threshold, DESeqDir)
    print("DESeq object after preprocessing:")
    printDeSeqObj(dds)
    
    
    #########################################################################
    ### PREPARATION FOR STATISTICAL TEST
    ######################################################################### 
    
    lista<-prepareForTest(DeSeqObj=dds, DESeqDir,
                          controls, flag_cellLine=linea, flag_MFtest
                          )
    dds<-lista[[1]]
    testDir<-lista[[2]]
    rm(lista)
    printDeSeqObj(dds)
    print(paste("test directory:",testDir))
    
    ### BASIC DATA  VISUALIZATION 
    print("generation of basic images")
    basicImages (DeSeqObj=dds, testDir)
    
    if (linea=="Filter" && !flag_MFtest) {
      next
      #passo alla fase di test sulle singole linee
    } 
    
    #########################################################################
    ### STATISTICAL TESTS
    ######################################################################### 
    
    print("statistical tests chapter") 
    print(paste("chosen control samples:",controls))
    performStatisticalTests(DeSeqObj=dds,controls,flag_MFtest,testDir)
    print("statistical tests accomplished") 
    
    #########################################################################
    ### FEATURES SELECTION
    ######################################################################### 
    
    print("call to DE features selection")
    selectDEfeatures( DESeqDir, testDir, controls, 
                     flag_shrinkage, flag_cellLine=linea, flag_MFtest,
                     padj_max,FC_min,
                     flag_regionReport,flag_reportingTools
                     )
  if (flag_MFtest){
    break    #si fa un solo giro
    }
  }#chiude ciclo linee
  print("end of DESeq2 pipeline")
  lista<-list(resDir,DESeqDir)
  return(lista)
}

