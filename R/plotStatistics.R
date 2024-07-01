plotStatistics<-function(outDir,resultsObj,testName,w,h,r){

  
  statDir<-file.path(outDir,"statistics")
  dir.create(statDir, showWarnings = FALSE, recursive = TRUE)
  
  
  #RESULTS OBJECT ADS DATA FRAME
  df<-as.data.frame(resultsObj)
  names(df)
  
  # boxplot pvalue
  pvalue<-df$pvalue
  titolo<-"boxplot_pvalue.tiff"
  filePath<-file.path(statDir,titolo)
  tiff(filePath, width=w, height=h, res=r)
  suppressWarnings(boxplot(-log10(pvalue),na.action = rm,ylab="-log10(pvalue)"))
  dev.off()
  
  # histogram pvalue
  titolo<-"histogram_pvalue.tiff"
  filePath<-file.path(statDir,titolo)
  tiff(filePath, width=w, height=h, res=r)
  hist(pvalue,breaks=50)
  dev.off()
  
  # boxplot padj
  padj<-df$padj
  titolo<-"boxplot_padj.tiff"
  filePath<-file.path(statDir,titolo)
  tiff(filePath, width=w, height=h, res=r)
  suppressWarnings(boxplot(-log10(padj),na.action = rm,ylab="-log10(padj)"))
  dev.off()
  
  
  # histogram padj
  titolo<-"histogram_padj.tiff"
  filePath<-file.path(statDir,titolo)
  tiff(filePath, width=w, height=h, res=r)
  hist(padj,breaks=50)
  dev.off()
  
  # boxplot absFC
  absFC<-abs(df$log2FoldChange)
  titolo<-"boxplot_absLog2FC.tiff"
  filePath<-file.path(statDir,titolo)
  tiff(filePath, width=w, height=h, res=r)
  suppressWarnings(boxplot(absFC,na.action = rm,ylab="abs(log2FC)"))
  dev.off()
  
  
  # histogram absFC
  titolo<-"histogram_absLog2FC.tiff"
  filePath<-file.path(statDir,titolo)
  tiff(filePath, width=w, height=h, res=r)
  hist(absFC,breaks=50)
  dev.off()
  
}