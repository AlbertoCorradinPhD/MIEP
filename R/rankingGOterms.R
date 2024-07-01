rankingGOterms<- function(sampleGOdata,resultsOfTest,ntop, resName,
                          outDir, w,h,r) {
  
    #print(resName)
    
    goEnrichment <- GenTable(
      sampleGOdata,
      pValues = resultsOfTest, #prende i p-values come carattere
      orderBy = "pValues",
      topNodes = 100,
      numChar = 99)
    names(goEnrichment)
    goEnrichment$pValues <- as.numeric(goEnrichment$pValues)
    # filter terms for p<0.05 con correzione per test multipli
    goEnrichment$padj<-p.adjust(p=goEnrichment$pValues , method = "BH", n = length(goEnrichment$pValues))
    goEnrichment <- goEnrichment[goEnrichment$padj < 0.05,] 
    

    ############################################################
    ### TENGO SOLO I PRIMI ntop PER NUMERO DI SIGNIFICATIVI
    #############################################################

    goEnrichment<-goEnrichment[order(goEnrichment$Significant,decreasing=TRUE),]
    
    ### give weights
    temp1<-scale(x=goEnrichment$Significant, center = TRUE, scale = TRUE)
    temp2<- (-1)*scale(x=goEnrichment$pValues, center = TRUE, scale = TRUE)
    goEnrichment$weights<- temp1+temp2
    goEnrichment<-goEnrichment[order(goEnrichment$weights,decreasing=TRUE),]
    ### prepare for printing
    M<-dim(goEnrichment)[2]
    goEnrichment<-goEnrichment[,-M]
    max<-dim(goEnrichment)[1]
    goEnrichment<- goEnrichment[1:ifelse(ntop<max,ntop,max),]
    goEnrichment<-goEnrichment[order(goEnrichment$Significant,decreasing=TRUE),]
    boolean<- names(goEnrichment) %in% "Significant"
    names(goEnrichment)[boolean]<- 'Genes in the subset of interest'
    formatted<-sapply(X=goEnrichment[c("pValues","padj")], FUN=formatC, format = "e", digits = 2) 
    goEnrichment_formatted<-cbind(goEnrichment[,-c(6,7)],as.data.frame(formatted))
    
    filename<-paste("GOenrichmentTestTable.html",sep="")
    filePath<-file.path(outDir,filename)
    titolo<-"Top GO terms with high number of significant features"
    printTableHTML(data=goEnrichment_formatted,titolo=titolo,filePath)
    
    ### PREPARO DATAFRAME PER ggplot
    ggdata <-goEnrichment
    ggdata$Term <- factor(ggdata$Term, levels = rev(ggdata$Term)) # fixes order
    filename<-paste("rankingOfGOterms.tiff",sep="")
    filePath<-file.path(outDir,filename)
    tiff(filePath, width=w, height=h,res=r)
    p<-rankingGOterms_plot(data=ggdata,resName)
    print(p)
    dev.off()
    
    return(goEnrichment)
  }

