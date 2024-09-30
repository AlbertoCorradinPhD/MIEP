calcPadj_max<-function(resultsObj,filePath, percentile=5){
  ### set padj_max to 1st quantile of the distribution
  df<-as.data.frame(resultsObj)
  names(df)
  step<-0.05
  sommario<-quantile(x=abs(df$padj), probs = seq(step, 1, step), na.rm = TRUE,
                     names = TRUE, type = 7, digits = 2)
  #print(sommario)
  percentileInChar<-paste(as.character(percentile),"%",sep="")
  padj_max<-sommario[percentileInChar]

  write(padj_max, file = filePath,
        ncolumns = 1, append = TRUE, sep="\n")
  frase<-paste("distribution of padj values:")
  write(frase, file = filePath, append = TRUE,sep="\n")
  write(names(sommario), file = filePath, append = TRUE,sep="\t", ncolumns = length(sommario))
  write(sommario, file = filePath, append = TRUE,sep="\t", ncolumns = length(sommario))

  return(padj_max)
}

