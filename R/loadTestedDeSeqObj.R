loadTestedDeSeqObj<-function(controllo, testDir){
  
  # recupero DeSeqObj
  filename<-paste("ddsTested_control",controllo,".rds",sep="")
  filePath<-file.path(testDir,filename)
  dds<-readRDS(file =filePath)
  #i<-2 
  print("performed comparisons:")
  print(resultsNames(dds)[ 2:length(resultsNames(dds))] )
  return(dds)
  }