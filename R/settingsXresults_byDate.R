settingsXresults_byDate<-function(pipelineDir){
  
  resDir<-defineResDir_byDate(pipelineDir)
  if (!dir.exists(resDir)) {
    print("results directory is missing! I exit")
    exit()
    }
  DESeqDir<-file.path(resDir,"DESeq2runs")
  if (!dir.exists(DESeqDir)) {
    print("results directory is missing! I exit")
    exit()
  }
  lista<-list(resDir,DESeqDir)
  return(lista)
}