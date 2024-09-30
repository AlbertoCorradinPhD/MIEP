volcanoPlot<-function(filename, main, data, FC_min, padj_max,w,h,r,
                        boundaries=FALSE, yLimit=NULL, xLimit=NULL,
                      gene_names=FALSE){
  
  x_values<-data$log2FoldChange
  y_values<- data$padj
  logFC_min<-abs(log2(FC_min))
  tiff(filename, width=w, height=h, res=r)
  par(mfrow=c(1,1))
  
  # Make a basic volcano plot
  boolean<-is.finite(-log10(y_values))
  indexes<-which(boolean %in% TRUE)
  ymax<- max(-log10(y_values[indexes]), na.rm=TRUE)
  if (!is.null(yLimit)) {ymax<-yLimit}
  if (ymax<1) {ymax<-1}
  xmin<-min(x_values, na.rm=TRUE)
  xmax<-max(x_values, na.rm=TRUE)
  if (!is.null(xLimit)) {
    xmax<-xLimit
    xmin<- -xLimit
    }
  plot(data$log2FoldChange, -log10(data$padj), pch=20, 
        main=main, xlim=c(xmin,xmax),ylim=c(0,ymax), cex=0.5,
       xlab="log2(FoldChange)", ylab="-log10(padj)")
  
  # Add colored points: blue if padj<0.05, red if 2FC>FC_min and padj<0.05
  newData<-subset(data, data$padj<padj_max )
  points(newData$log2FoldChange,-log10(newData$padj), pch=20, col="blue",cex=1)
  newData<-subset(data, data$padj<padj_max & abs(data$log2FoldChange)>logFC_min)
  points(newData$log2FoldChange, -log10(newData$padj), pch=20, col="red",cex=1.5)  
       
  
  # Add boundaries
  if (boundaries){ #apre if boundaries
    #boundaries FC_min
    abline(v = c(logFC_min,-logFC_min), lty="dashed",col="red")
    frase<-paste("FC>",round(FC_min,2))
    text (x=logFC_min*200/100, y = ymax*95/100, labels = frase,cex=0.75)
    
    #boundaries padj_max
    abline(h = -log10(padj_max), lty="dashed",col="red")
    # aggiusto formato scritta
    if (padj_max<1e-2){
      frase<-paste("padj<",format(padj_max, scientific = TRUE,digits = 3))
    } else { frase<-paste("padj<",round(padj_max,2)) }
    text (x=xmax*80/100, y = -log10(padj_max)*3, labels = frase,cex=0.75)
  } #chiude if boundaries
  
  # Add gene names
  if (gene_names){
    newData<-subset(data, data$padj<padj_max & abs(data$log2FoldChange)>logFC_min)
    if (dim(newData)[1]>0){
      text(newData$log2FoldChange,-log10(newData$padj),  labels=newData$ID_REF, 
           cex=1.25, pos=1,col="red",offset=0.25)}
  }
  dev.off()
  
  

}