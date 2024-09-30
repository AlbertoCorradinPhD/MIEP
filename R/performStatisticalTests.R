performStatisticalTests<-function(DeSeqObj,controls,
                                flag_MFtest,testDir){
  
  for (c in 1: length(controls)){
    
    ### STATISTICAL TESTS
    controllo<-controls[c]
    print(paste("call to statistical test, based on control samples: ", controllo,sep=""))
    
    #copia per mantenere dds originale inalterato. Lo indico con _dummy
    dds_dummy<-DeSeqObj 
    dds_dummy$condition<-relevel(dds_dummy$condition, controllo)
    tryCatch(
        dds_tested<-statisticalTest(DeSeqObj=dds_dummy, flag_MFtest),
      error = function(err){
        print("DESeq2 can't perform any statistical test.")
        print("Check the number of replicates, please. Are they enough?")
        print("I exit from the pipeline")
        exit()
    })#chiudo tryCatch
    print("performed comparisons:")
    print(resultsNames(dds_tested)[ 2:length(resultsNames(dds_tested))] )
    print("store DeSeq object in rds file")
    filename<-paste("ddsTested_control",controllo,".rds",sep="")
    filePath<-file.path(testDir,filename)
    saveRDS(dds_tested, file =filePath)
    
  }#chiude ciclo controlli
  
  
}