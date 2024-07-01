generaPcaPlots<-function(outdir,w,h, res,metodi,nomi_metodi,DeSeqObj){
  
  for (i in 1:length(metodi)) {
    metodo<-metodi[[i]]
    filename<-paste("pcaPlot_", nomi_metodi[i],".tiff",sep="")
    filePath<-file.path(outdir,filename)
    tiff(filePath, width=w, height=h, res=res)
    p<-plotPCA(metodo, intgroup=c( "cellLine", "condition"),ntop=dim(DeSeqObj)[1])
    print(p)
    dev.off()
    
    #by ggplot
    pcaData <- plotPCA(metodo, intgroup=c("cellLine", "condition"), returnData=TRUE,ntop=dim(DeSeqObj)[1])
    percentVar <- round(100 * attr(pcaData, "percentVar"))
    filename<-paste("ggPCAplot_", nomi_metodi[i],".tiff",sep="")
    filePath<-file.path(outdir,filename)
    tiff(filePath, width=w, height=h, res=res)
    p<-ggplot(pcaData, aes(.data$PC1, .data$PC2, 
                           color=.data$cellLine, shape=.data$condition)) +
      geom_point(size=7) +
      xlab(paste0("PC1: ",percentVar[1],"% variance")) +
      ylab(paste0("PC2: ",percentVar[2],"% variance")) +
      coord_fixed()
    p<-p+ scale_color_brewer(palette="Set1")
    show(p)
    dev.off()
  }
  
}