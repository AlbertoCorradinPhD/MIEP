fixNAs<-function(resultsObj){
  
  ### insert 1 where DESeq2 independent filter inserts NAs ###
  
  #indexes<-which(is.na(resultsObj$pvalue))
  indexes<-which(is.na(resultsObj$padj))
  if (length(indexes)>0){
    print("independent filter inserted NAs")
    resultsObj$padj[indexes]<-1
  }
  return(resultsObj)
}