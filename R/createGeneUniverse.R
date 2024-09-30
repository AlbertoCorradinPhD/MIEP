
createGeneUniverse<- function(scoreTable, perc, outDir,  e=globalenv()) {
  #NB: perc è definita prima mediante variabile globale
  ### CREO IL GENE UNIVERSE
  step<-0.01
  sommario<-quantile(x=scoreTable[,"importance"], probs = seq(step, 1, step), na.rm = FALSE,
                     names = TRUE, type = 7, digits = 2)
  #print(sommario)
  threshold<-0
  if (!is.null(perc)){
    percentileInChar<-paste(as.character(perc),"%",sep="")
    threshold<-sommario[percentileInChar]
  }
 
  #creo il gene universe, cioe' un vettore con nomi
  geneList<-scoreTable$importance
  names(geneList)<-scoreTable$feature
  #head(geneList)
  
  #### HISTOGRAM OF SCORES
  w<-2000
  h<-1000
  r<-100
  filePath<-file.path(outDir,"scoresHistogram.jpeg")
  jpeg(filePath, width=w, height=h,res=r)
  main = "Distribution of importance values in the gene universe"
  hist(scoreTable$importance,main=main, xlab = NULL)
  dev.off()
  
  lista<-list(threshold=threshold,geneList=geneList)
  return(lista)
}
