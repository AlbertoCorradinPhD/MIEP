

pipelineInfo_byDate<-function(pipelineDir,  dataDir, resDir=NULL, DESeqDir=NULL){
  
  
  ###############################################################################
  ###  SETTINGS FOR ELABORATED DATA AND RESULTS' DIRS
  ###############################################################################
  
  if ( is.null(resDir) || is.null(DESeqDir)){
    lista<-settingsXresults_byDate(pipelineDir)
    resDir<-lista[[1]]
    DESeqDir<-lista[[2]]
  }
  
  filePath<-file.path(dataDir,"coldata.csv")
  tryCatch({
    coldata <- read.csv(file=filePath,  check.names=TRUE, header=TRUE, row.names="code" )
    cellLines<-unique(coldata$cellLine)
    },
    error = function(err){
      print("no coldata file. Why is it missing?")
      exit()
    })#chiudo tryCatch
  
  filePath<-file.path(DESeqDir,"comparisonsPerformed.txt")
  tryCatch( {
    confronti<-unique(scan(file = filePath, what = character(),
                          sep = "\n",strip.white = TRUE))
    if (confronti[1]=="") {confronti<-NULL}
  },
  error = function(err){
    print("no comparisons accomplished")
    exit()
  })#chiudo tryCatch
  
  
  lista<-list( resDir,DESeqDir, cellLines, confronti, coldata )
  }