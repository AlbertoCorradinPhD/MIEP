genesetScan<-function(geneset, resDir=NULL, e=globalenv()) {
  
  #check separator
  separator<-"\t"
  flagN<-grepl(pattern="n$", x=geneset, ignore.case = FALSE, perl = TRUE)
  flagUnion<-grepl(pattern="ion$", x=geneset, ignore.case = FALSE, perl = TRUE)
  if (flagN && !flagUnion ){separator<-"\n"}
  
  geneList<-NULL
  if (!is.null(resDir)){
    tryCatch({
      filename<-paste(geneset,".gmt",sep="")
      filePath<-file.path(resDir,"gmtFiles",filename)
      geneList<-scan(file = filePath,sep = separator,what = "character")
      #head(geneList)
      geneList<-geneList[3:length(geneList)]
      return(geneList)
      }, error = function(err){
    })
  }#chiude if
  
  #NB: gmtFolder dovrebbe essere una variabile globale e non necessitare di ulteriori specificazioni
  gmtFolder<-get0("gmtFolder", envir = e, ifnotfound = NULL)
  if (!is.null(gmtFolder)){
    tryCatch({
      filename<-paste(geneset,".gmt",sep="")
      filePath<-file.path(gmtFolder,filename)
      geneList<-scan(file = filePath,sep = separator,what = "character")
      #head(geneList)
      geneList<-geneList[3:length(geneList)]
      return(geneList)
    }, error = function(err){
      print("gene sets description not found anywhere")
    })#chiude tryCatch
  } #chiude if
  
  return(geneList)
}