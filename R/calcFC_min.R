calcFC_min<-function(resultsObj,filePath, percentile=95){
  
  df<-as.data.frame(resultsObj)
  names(df)
  step<-0.05
  sommario<-quantile(x=abs(df$log2FoldChange), probs = seq(step, 1, step), na.rm = TRUE,
           names = TRUE, type = 7, digits = 2)
  #print(sommario)
  percentileInChar<-paste(as.character(percentile),"%",sep="")
  logFC_min<-sommario[percentileInChar]
  FC_min<-2^logFC_min
  write(FC_min, file = filePath,
        ncolumns = 1, append = TRUE, sep="\n")
  frase<-paste("distribution of FC values:")
  write(frase, file = filePath, append = TRUE,sep="\n")
  write(names(sommario), file = filePath, append = TRUE,sep="\t", ncolumns = length(sommario))
  write(sommario, file = filePath, append = TRUE,sep="\t", ncolumns = length(sommario))
  
  return(FC_min)

}
