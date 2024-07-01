settingsForRun<-function(pipelineDir){
  
  ### READ DEFAULT IPERPARAMETERS 
  lista<-iperparametersReadDefault("./sysdata.rda")
  flag_MFtest<-lista[[1]]
  flag_shrinkage<-lista[[2]]
  padj_max<-lista[[3]]
  FC_min  <-lista[[4]]
  bF_threshold<-lista[[5]]
  cF_threshold<-lista[[6]]
  flag_regionReport<-lista[[7]]
  flag_reportingTools<-lista[[8]]
  rm(lista)
  
  ### CHANGE IPERPARAMETERS 
  lista<-iperparametersChange( )
  if (!is.null(lista)){
    flag_shrinkage<-lista[[1]]
    padj_max<-lista[[2]]
    FC_min<-lista[[3]]
    bF_threshold<-lista[[4]]
    cF_threshold<-lista[[5]]
  }
  ### CHECK IPERPARAMETERS
  iperparametersCheck(flag_MFtest, flag_shrinkage, padj_max, FC_min,  
                      bF_threshold, cF_threshold, flag_regionReport, 
                      flag_reportingTools)
                      
  
  
  ### CREATE DIRECTORIES FOR THE RUN
  lista<-dirsForRun(pipelineDir)
  resDir<-lista[[1]]
  DESeqDir<-lista[[2]]
  rm(lista)
  
  filePath<-file.path(DESeqDir,"DESeqSettings.tsv")
  printSettings(filePath, flag_MFtest, flag_shrinkage, padj_max, FC_min, 
                bF_threshold, cF_threshold, flag_regionReport, flag_reportingTools)
  
  
  lista<-list(flag_MFtest, flag_shrinkage, padj_max, FC_min, 
             bF_threshold, cF_threshold, flag_regionReport,flag_reportingTools,
             DESeqDir, resDir)
  return(lista)

}



