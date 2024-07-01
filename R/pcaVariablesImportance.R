pcaVariablesImportance<-function(res.pca, outDir, N, inDir,  
                                 resDir, linea, geneset=NULL, Kmax=50,
                                 e=globalenv()){
  
  res.var <- get_pca_var(res.pca)
  contributions<-res.var$contrib 
  features<-row.names(contributions)
  features<-str_replace_all(features, "\\.", "-")
  contributions<-as.data.frame(contributions)
  
  for (Dim in 1:N) {
    #print(Dim)
    dfGains<-data.frame(contributions[,Dim])
    names(dfGains)<-"importance"
    dfGains$feature <- features
    dtGains<-dfGains[order(dfGains[,1] , decreasing = T),]
    #print(head(dtGains))
    filename<-paste("PC",Dim,"_contributions.tsv",sep="")
    filePath<-file.path(outDir,filename)
    write.table(x=dtGains, file = filePath, append = FALSE, quote = FALSE, sep = "\t",
                eol = "\n", na = "NA", dec = ".", row.names = FALSE,
                col.names = TRUE)
    filename<-paste("PC",Dim,"_importanceTrend.png",sep="")
    filePath<-file.path(outDir,filename)
    png(filePath, width=1000, height=1000)
    plot(dtGains[,1], ylab="importance", xlab="features", main=paste("Principal component",Dim,sep=" "))
    dev.off()
  }
  
  ##### BARPLOTS
  #read data to color bars based on reads' counts
  filename<-"exprData.tsv"
  filePath<-file.path(inDir,filename)
  exprData <-read.table(file=filePath, header = TRUE, sep = "\t", quote = "\"'",
                        dec = ".", numerals = c("allow.loss", "warn.loss", "no.loss"),
                        na.strings = "NA", colClasses = NA, 
                        strip.white = TRUE, blank.lines.skip = TRUE)
  
  for (Dim in 1:N) {
    filename<-paste("PC",Dim,"_contributions.tsv",sep="")
    filePath<-file.path(outDir,filename)
    indexTable<-read.table(file=filePath, header = TRUE, sep = "\t", quote = "\"'",
                           dec = ".", numerals = c("allow.loss", "warn.loss", "no.loss"),
                           na.strings = "NA", colClasses = NA, 
                           strip.white = TRUE, blank.lines.skip = TRUE)
    names(indexTable)<-c("importance", "feature")
    
    #INTRODUCE EVENTUAL COMMENTS BASED ON GENELISTS
    filename<-paste("PC",Dim,"_",Kmax,"topContributors.tiff",sep="")
    filePath<-file.path(outDir,filename)
    h<-Kmax*200
    tiff(filePath, width=7000, height=h,res=1000)
    p<-pcaBarplotVsGenelist( indexTable,data=exprData,  
                             resDir, linea, geneset, Kmax,e=e)
    print(p)
    dev.off()
  }
}