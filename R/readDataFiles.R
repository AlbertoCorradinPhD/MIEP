readDataFiles<-function(dataDir){
  
  counts_file<-NULL
  coldata_file<-NULL
  

  tryCatch( {
    counts_file<-file.path(dataDir,"raw_gene_counts.tsv")
    coldata_file<-file.path(dataDir,"coldata.csv")
    suppressWarnings( originalCounts<- read.csv(file = counts_file, sep = '\t', header = TRUE, row.names="GeneID") )
    suppressWarnings( coldata <- read.csv(coldata_file,  check.names=TRUE, header=TRUE, row.names="code" ))
    cellLines<-unique(coldata$cellLine)
    conditions<-unique(coldata$condition)
  },
  error = function(err){
    print("either file coldata.csv or file raw_gene_counts is missing in data folder")
    exit()
  })#chiudo tryCatch
  
  suppressWarnings( controls<-importControls(dataDir) )
  checkControls(conditions, controls )
  
  lista<-list(originalCounts,coldata,controls,cellLines,conditions)
  return(lista)
}
