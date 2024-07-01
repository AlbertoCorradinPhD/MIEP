
creaTestDir<-function(DESeqDir, flag_cellLine, 
                      flag_MFtest, prefix="test-"){
  
  if (flag_MFtest){
    testDir<-file.path(DESeqDir,prefix)
    testDir<-paste(testDir,"Multifactor",sep="")
  } else if (flag_cellLine=="Filter" ){
    testDir<-file.path(DESeqDir,"wholeBatch")
  } else {
    testDir<-file.path(DESeqDir,prefix)
    testDir<-paste(testDir,flag_cellLine,sep="")
  }
  try({
    dir.create(testDir, showWarnings = FALSE, recursive = TRUE)
    print(paste("creata directory:", testDir))
    })
  return(testDir)
}
