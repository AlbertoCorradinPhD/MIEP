expressionSet_wrapper<-function(inDir, dataDir,  coldata) {
  #import data
  filePath<-file.path(inDir,"normalizedCounts_withSymbol.tsv")
  exprData<-read.table(filePath, header=TRUE, sep="\t",
                       as.is=TRUE)
  
  # default values
  infoExprSet<-data.frame(name="name",lab="lab",contact="contact",title="title")
  
  tryCatch( {
    filePath<-file.path(dataDir,"infoExprSet.tsv")
    infoExprSet<-read.table(filePath, header=TRUE, sep="\t",
                          as.is=TRUE)
    #convert to dataframe
    infoExprSet<-data.table(infoExprSet)
    name<-infoExprSet[infoExprSet$parameter=="name",]$value
    lab<-infoExprSet[infoExprSet$parameter=="lab",]$value
    contact<-infoExprSet[infoExprSet$parameter=="name",]$value
    title<-infoExprSet[infoExprSet$parameter=="lab",]$value
    infoExprSet<-data.frame(name=name,lab=lab,contact=contact,title=title)
  },
  error=function(err) {
    print("no info to build expression set. Default will be used")
  })
   
  ### BUILD EXPRESSION SET FROM NORMALIZED COUNTS
  exprSet<-expressionSet_build(exprData,coldata, infoExprSet)
  filePath<-file.path(inDir,"expressionSet.rds")
  saveRDS(exprSet, file = filePath)
  
  return(exprSet)
}
