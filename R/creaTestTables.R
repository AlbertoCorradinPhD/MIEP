creaTestTables<-function(outDir,resultsObj,testName, padj_max, FC_min){
  
  logFC_min<-abs(log2(FC_min))
  # ordering 
  df<-as.data.frame(resultsObj)
  resOrdered <- resultsObj[order(resultsObj$log2FoldChange, decreasing = TRUE),]
  dfOrdered <- df[order(df$log2FoldChange, decreasing = TRUE),]
  df<-dfOrdered
  #colnames(df)
  #head(df)
  df<-cbind(as.data.frame(row.names(df)), df)
  names(df)[1]<-"ensembleID"
  filename<-paste(testName,".tsv", sep="")
  filePath<-file.path(outDir,filename)
  write.table(df, file=filePath, sep="\t", col.names=TRUE, row.names=FALSE)
  
  ### estraggo DEs
  res_DEonly <- resOrdered[which(x=(resOrdered$padj < padj_max & 
            abs(resOrdered$log2FoldChange) > logFC_min )),] 
  #print("DESeq2 results object with only differentially expressed features:")
  #print(as.data.frame(res_DEonly))
  df_DEonly<-subset(df, subset=(df$padj < padj_max & 
                         abs(df$log2FoldChange) > logFC_min ))
  df<-df_DEonly
  #colnames(df)
  #head(df)
  #df<-cbind(as.data.frame(row.names(df)), df)
  #names(df)[1]<-"ensembleID"
  filename<-paste(testName,"-DEonly.tsv",sep="")
  filePath<-file.path(outDir,filename)
  write.table(df, file=filePath, sep="\t", col.names=TRUE, row.names=FALSE)
  print(paste("number of DE features:  ", dim(df)[1]))
  lista<-list( df,res_DEonly)
  return(lista)
  
}
