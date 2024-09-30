upDowns<-function(data, counter,inDir,DESeqDir,confronto, linea){
  
  #differenzio up e downregolati
  boolean<-data[,counter]==1
  nomi<-row.names(data)[boolean]
  
  filename<-paste(confronto,"-",linea,".tsv",sep="")
  filePath<-file.path(inDir,filename)
  exprData<-read.table( filePath, header=TRUE, skip=0, sep="\t")
  indexes<-match(x=nomi, table=exprData$ID_REF, nomatch = NA_integer_, incomparables = NULL)
  boolean<-is.na(indexes)
  indexes<-indexes[!boolean]
  ensembleIDs<-exprData$ensembleID[indexes]
  
  foldername<-paste("test-",linea,sep="")
  testTablesDir<-file.path(DESeqDir,foldername,"testTables")
  testedSequences<-importTestTable(testTablesDir,confronto)
  indexes<-match(x=ensembleIDs, table=testedSequences$ensembleID, 
                 nomatch = NA_integer_, incomparables = NULL)
  logFC<-testedSequences$log2FoldChange[indexes]
  boolean<-data[,counter]==1
  data[boolean,counter]<-ifelse(logFC>0,1,-1)
  #test: "XNDC1N-ZNF705EP-ALG1L9P" %in% row.names(data)
  return(data)
}