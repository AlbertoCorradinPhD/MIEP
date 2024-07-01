expressionSet_build<- function(exprData,coldata, infoExprSet) {
  
  #prepare data
  #names(exprData)
  nomiIDs<-c("ID_REF","ensembleID")
  nomiDati<-setdiff(names(exprData),nomiIDs)
  X<-as.matrix(subset(exprData,select=nomiDati))
  #print(head(X[,1:3]))
  row.names(X)<-exprData$ID_REF
  #print(head(X[,1:3]))
  #print(dim(X))

  ### collapsing
  X<-collapsing(X)
  #colnames(X)
  #head(X[,1:5],3)
  #head(rownames(X))
  #dim(X)

  ### BUILD EXPRESSIONSET
  exprSet<-expressionSet_template(data=X,pData=coldata,infoExprSet)
  expressionSet_print(exprSet)
  return(exprSet) 
  
}
