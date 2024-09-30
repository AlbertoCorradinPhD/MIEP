#' @importFrom lubridate year

dirsForRun<-function(pipelineDir){
  
  dataOdierna<-Sys.Date()
  mese<-months(dataOdierna)
  anno<-year(dataOdierna)
  print("create run directories")
  foldername<-paste("res_",mese,"-",anno,sep="")
  resDir<-file.path(pipelineDir, foldername)
  dir.create(resDir, showWarnings = FALSE, recursive = TRUE)
  
  DESeqDir<-file.path(resDir,"DESeq2runs")
  dir.create(DESeqDir, showWarnings = FALSE, recursive = TRUE)
  
  lista<-list(resDir,DESeqDir)
  return(lista)

}



