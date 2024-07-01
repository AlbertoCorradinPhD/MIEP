
generaTestTablePerGeneset<-function(inDir, linea, confronto, myTable ) {
  
  newTable<-generaTestTable(inDir, cellLines=linea, 
                            confronti=confronto, newTable=myTable[,c(1:2)] )
  names(newTable)[3:4]<-c("log2FoldChange","padj")
  temp<-generaTestTable_DEonly(inDir, cellLines=linea, 
                               confronti=confronto, newTable=myTable[,c(1:2)] )
  names(temp)[3:4]<-paste(c("log2FoldChange","padj"), "if DE")
  newTable<-cbind(newTable, temp[,c(3,4)])
  return(newTable)
}