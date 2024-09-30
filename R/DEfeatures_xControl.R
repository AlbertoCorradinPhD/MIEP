
DEfeatures_xControl<-function(  DESeqDir,testDir, DeSeqObj, 
                               flag_shrinkage, flag_cellLine, flag_MFtest,
                               padj_max,FC_min,  
                               flag_regionReport,flag_reportingTools
                              ){
  start<-ifelse(flag_MFtest,3,2)
  for (i in start:length(resultsNames(DeSeqObj))){ #i<-1 corresponds to intercept
    nameOfContrast<- resultsNames(DeSeqObj) [i]
    print(paste("name of the contrast:  ", nameOfContrast))
    testName<- sub("condition_","",nameOfContrast)
    testName<- sub("cellLine_","",testName)
    print(paste("test:  ", testName))
    
    # elenco dei confronti
    filePath<-file.path(DESeqDir,"comparisonsPerformed.txt")
    write(x=testName, file = filePath, ncolumns = 1,
            append = TRUE, sep = "\n")

    
    ### IDENTIFY DE FEATURES
    lista<-DEfeatures_xContrast( testDir, DeSeqObj, 
                                flag_shrinkage, flag_cellLine,
                                padj_max,FC_min, 
                                nameOfContrast, testName
                                )
    dds_DEonly<-lista[[1]]
    res_DEonly<-lista[[2]]
    rm(lista)
    print(paste("features selection accomplished for test:", testName))
    
    #########################################################################
    ### ADDITIONAL REPORTS
    #########################################################################
    
    # REGION REPORT
    if (flag_regionReport){
    #  print("call to region report")
    #  reportRegion(testDir, DeSeqObj=dds_DEonly, resultsObj=res_DEonly,
    #               nameOfContrast,  testName)
    }
    # REPORTING TOOL  
    if (flag_reportingTools){
    #  print("call to reporting tool")
    #  reportingTools( testDir, DeSeqObj=dds_DEonly, resultsObj=res_DEonly, 
    #                 nameOfContrast, testName) 
    }
  }#chiude ciclo per contrasti
  
}
