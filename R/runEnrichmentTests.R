runEnrichmentTests<- function(sampleGOdata,outDir) {
  resultFisher <- runTest(sampleGOdata, algorithm = "classic", statistic = "fisher") 
  resultKS <- runTest(sampleGOdata, algorithm = "classic", statistic = "ks")
  resultKS.elim <- runTest(sampleGOdata, algorithm = "elim", statistic = "ks")
  resultsTogether<-c(resultFisher,resultKS,resultKS.elim)
  resultsNames<-c("Fisher","KS","KS.elim")
  
  #head(score(resultFisher),10)
  #head(score(resultKS,whichGO = names(score(resultFisher)) ),10)
  #head(score(resultKS.elim,whichGO = names(score(resultFisher)) ),10)
  
  pvalFis<-score(resultFisher)
  pvalKS<-score(resultKS )
  pvalKS.elim<-score(resultKS.elim )
  
  # DISTRIBUTIONS OF TESTS' P-VALUES
  w<-2000
  h<-1000
  r<-100
  
  #boxplot
  filePath<-file.path(outDir,"pValues_boxplots_pValues.jpeg")
  jpeg(filePath, width=w, height=h, res=r)
  par(mfcol = c(1, 3), cex = 1)
  boxplot(pvalFis, main="test: Fischer" )
  boxplot(pvalKS , main="test: KS (classic)")
  boxplot(pvalKS.elim, main="test: KS (elim)" )
  dev.off()
  
  #histograms
  histFisher<-hist(pvalFis, 50, xlab = "p-values", main="test: Fischer")
  histKS<-hist(pvalKS, 50, xlab = "p-values", main="test: KS (classic)")
  histKS.elim<-hist(pvalKS.elim, 50, xlab = "p-values", main="test: KS (elim)")
  ymax<-110/100*max(histFisher$counts,histKS$counts, histKS.elim$counts)
  filePath<-file.path(outDir,"pValues_histograms.jpeg")
  jpeg(filePath, width=w, height=h, res=r)
  par(mfcol = c(1, 3), cex = 1)
  hist(pvalFis, 50, xlab = "p-values", main="test: Fischer", ylim=c(0,ymax))
  hist(pvalKS, 50, xlab = "p-values", main="test: KS (classic)", ylim=c(0,ymax))
  hist(pvalKS.elim, 50, xlab = "p-values", main="test: KS (elim)", ylim=c(0,ymax))
  dev.off()
  
  filePath<-file.path(outDir,"pValues_densities.jpeg")
  jpeg(filePath, width=w, height=h, res=r)
  par(mfcol = c(1, 3), cex = 1)
  plot(density(pvalFis), xlab = "p-values", main="test: Fischer")
  plot(density(pvalKS),  xlab = "p-values", main="test: KS (classic)")
  plot(density(pvalKS.elim), xlab = "p-values", main="test: KS (elim)")
  dev.off()
  
  
  lista<-list(resultsTogether, resultsNames)
  return(lista)

}