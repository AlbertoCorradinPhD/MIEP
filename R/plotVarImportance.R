plotVarImportance<-function(varImportance,outDir,w,h,r){
  
  dtVarImportance<-data.table(varImportance)
  dtVarImportance$feature <- names(varImportance)
  #names(dtVarImportance)
  names(dtVarImportance)<-c("importance","feature")
  dtVarImportance<- dtVarImportance[order(dtVarImportance$importance, 
                                          decreasing = T)]
  filePath<-file.path(outDir,"varImportance.tsv")
  write.table(dtVarImportance, file=filePath, row.names=F, sep="\t",col.names=T)
  
  #### CHARTS FOR MOST IMPORTANT FEATURES
 
  lista<-selectTopFeatures(data=dtVarImportance)
  Ntop<-lista[[1]]
  p<-lista[[2]]
  filePath<-file.path(outDir,"varImportance.tiff")
  tiff(filePath, width=w, height=h,res=r)
  print(p)
  dev.off()
  
  selectedFeatures<-dtVarImportance$feature[1:Ntop]
  return(selectedFeatures)
}
