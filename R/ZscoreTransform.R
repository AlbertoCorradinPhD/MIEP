ZscoreTransform<-function(dataML_counts,coldata) {
  
  #SCALING AND CENTERING
  names(dataML_counts)
  X<-as.data.frame(dataML_counts[,-1])
  
  #elimino gli NAs qualora ci siano
  boolean<-is.na(X)
  #print(paste("number of NAs:",sum(boolean)))
  X[boolean] <- 0
  #check
  #boolean<-complete.cases(X)
  #sum(!boolean)
  
  
  cellLines<-unique(coldata$cellLine)
  temp<-rep(0,dim(X)[2]) #dummy piece
  for (j in 1:length(cellLines) ){
    linea<-cellLines[j]
    boolean<-coldata$cellLine %in% linea
    Xs<-scale(x=as.matrix(X[boolean,]), center = TRUE, scale = TRUE)
    temp<-as.data.frame(rbind(temp,Xs))
  }
  dataML<-temp[-1,]
  NAs<-is.na(dataML)
  dataML[NAs]<-0
  
  #RIPRISTINO FORMATTAZIONE
  dataML<-dataML[row.names(coldata),]#per riportare le righe all'ordine originario
  dataML<-cbind(as.data.frame(dataML_counts$labels),dataML)
  names(dataML)[1]<-"labels"
  names(dataML)
  
  
  
  #for GSEA
  exprData_ZscoreTransformed<-ZscoreTransform_getDataset(dataML)

  lista<-list(dataML=dataML,exprData_ZscoreTransformed=exprData_ZscoreTransformed)
  return(lista)

}