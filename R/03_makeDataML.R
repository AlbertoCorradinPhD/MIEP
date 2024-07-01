
makeDataML<-function( inDir, coldata, Labels, linea){
  
  outDir<-inDir
  filePath<-file.path(inDir,"exprData.tsv")
  exprData<-read.table(file=filePath, header =TRUE, sep = "\t") 
  
  
  ########### TRASPONE MATRICE DI DATI E CORREGGE BASALI ###################
  #crea dataML_counts
  dataML_counts<-creaDataML_counts(exprData, Labels )
  filePath<-file.path(outDir,"dataML_counts.rds")
  saveRDS(dataML_counts, file=filePath)
  
  #crea dataML con valori basali corretti
  lista<-ZscoreTransform(dataML_counts,coldata)
  dataML_ZscoreTransformed<-lista[[1]]
  exprData_ZscoreTransformed<-lista[[2]]
  rm(lista)
  
  #write the files
  filePath<-file.path(outDir,"dataML_ZscoreTransformed.rds")
  saveRDS(dataML_ZscoreTransformed, file=filePath)
  filename<-paste(linea,"_ZscoreTransformedData.txt")
  filePath<-file.path(outDir,filename)
  write.table(exprData_ZscoreTransformed, file=filePath, sep="\t", row.names=FALSE, quote=FALSE)

}
  
  
  
