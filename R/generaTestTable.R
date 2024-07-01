generaTestTable<-function(inDir, cellLines, confronti, newTable ){
  
  
  ########## IMPORT TEST TABLES FROM RESULTS OF DESEQ  ##############
  for (i in 1:length(cellLines)){
    linea<-cellLines[i]
    foldername<-paste("test-",linea,sep="")
    testTablesDir<-file.path(inDir,foldername,"testTables")
    for(j in 1: length(confronti)){
      confronto<-confronti[j]
      table<-importTestTable(testTablesDir,confronto)
      indexes<-match(x=newTable$ensembleID, table=table$ensembleID)
      df_toAdd<-table[indexes,c("log2FoldChange","padj" )] 
      
      ###  UPDATE DATABASE
      names(df_toAdd)
      nome1<-paste(linea,confronto, names(df_toAdd)[1], sep=" ")
      nome2<-paste(linea,confronto, names(df_toAdd)[2], sep=" ")
      names(df_toAdd)<-c(nome1,nome2)
      newTable<-cbind(newTable,df_toAdd)
    }
  }
  return(newTable)
}