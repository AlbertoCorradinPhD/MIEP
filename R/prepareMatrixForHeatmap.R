prepareMatrixForHeatmap<-function(dataML,splitMethod){
  
  #prepare data
  names(dataML)
  nomiMetadata<-c("labels")
  nomiFeatures<-setdiff(names(dataML),nomiMetadata)
  X<-t(as.matrix(subset(dataML,select=nomiFeatures)))
  #print(head(X[,1:3]))
  #dim(X)
  colnames(X)<-splitMethod
  print(head(X[,1:5],3))
  
  ############################### COLLAPSING ###################################
  if (FALSE){
    X<-collapsing(X)
    colnames(X)
    rownames(X)
  }
  
  return(X)
}