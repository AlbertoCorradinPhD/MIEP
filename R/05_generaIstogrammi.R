generaIstogrammi<-function(geneList, inDir, outDir,  coldata){
  
  #read counts
  filename<-file.path(inDir,"normalizedCounts_withSymbol.tsv")
  exprData<-read.table(file=filename, header = TRUE, sep = "\t")
  names(exprData)
  
  #create  outDir
  outDir<-file.path(outDir,"histograms")
  dir.create(outDir, showWarnings = FALSE, recursive = TRUE)
  
  #settings
  w<-1000
  h<-500
  r<-100
  
  for (g in 1:length(geneList)){
    # prepara dataframe per ggplot
    gene<-geneList[g]
    index<-match(x=gene, table=exprData$ID_REF, nomatch = NA)
    if (!is.na(index)){
      counts<-data.table(exprData)
      counts<-counts[,3:dim(counts)[2]]
      gene_counts<-counts[index,]
      gene_counts<-data.frame(counts=t(gene_counts),condition=coldata$condition,
                              line=coldata$cellLine, replicate=coldata$replicates)
      gene_counts<-cbind(gene_counts, sample=with(gene_counts,
                    paste(coldata$cellLine," rep. ",coldata$replicates, sep="")))
      
      suppressWarnings(
        plotHistograms(gene,gene_counts, outDir=file.path(outDir,gene), w,h,r )
      )
    }#chiude else
  }#chiude loop
}