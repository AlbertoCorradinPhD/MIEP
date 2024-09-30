rankedDotplots<-function(parsedName, cellLines, confronti, inDir){
  #settings
  w<-5000
  h<-5000
  r<-500
  for (c in 1:length(cellLines)){
    linea<-cellLines[c]
    
    for (j in 1:length(confronti)){
      confronto<-confronti[j]
      
      filename<-paste("FoldChanges&padjs_",linea,"-", confronto,".html", sep="")
      filePath<-file.path(inDir,filename)
      testTable<-readHTMLTable(doc=filePath, header = TRUE)[[1]]
      names(testTable)<-str_trim(names(testTable))
      testTable<-testTable[testTable$'log2FoldChange if DE'!="Not DE for this test",]
      if (dim(testTable)[1]<5) next
      testTable$log2FoldChange<-as.numeric(testTable$log2FoldChange)
      testTable$padj<-as.numeric(testTable$padj)
      testTable<-testTable[order(testTable$log2FoldChange),]
      testTable$ID_REF<-factor(testTable$ID_REF, levels=testTable$ID_REF)
      filename<-paste("FoldChanges&padjs_",linea,"-", confronto,".tiff", sep="")
      filePath<-file.path(inDir,filename)
      tiff(filePath, width=w, height=h,res=r)
      p <- ggplot(data=testTable,
                  aes(x = .data$ID_REF, y = .data$log2FoldChange, 
                      size = -log10(.data$padj), 
                      fill = .data$log2FoldChange)) +
        expand_limits(y = 1) +
        geom_point(shape = 21) +
        scale_size(range = c(5,15)) +
        scale_fill_continuous(low = 'royalblue', high = 'red4') +
        xlab('') + ylab('Fold change in logarithmic scale') +
        labs(
          title = parsedName,
          subtitle = 'Differentially expressed genes',
          #caption = 'Cut-off lines drawn at equivalents of padj=0.05, padj=0.01, padj=0.001'
          ) +
        geom_hline(yintercept = log2(1),
                   linetype = "longdash",
                   colour = "black",
                   linewidth = 1) +
        theme_bw(base_size = 10) +
        theme(
          legend.position = 'right',
          legend.background = element_rect(),
          plot.title = element_text(angle = 0, size = 18, face = 'bold', vjust = 1),
          plot.subtitle = element_text(angle = 0, size = 14, face = 'bold.italic', vjust = 1),
          plot.caption = element_text(angle = 0, size = 10, face = 'bold.italic', vjust = 1),
          axis.text.x = element_text(angle = 0, size = 12, face = 'bold', hjust = 1.10),
          axis.text.y = element_text(angle = 0, size = 12, face = 'bold', vjust = 0.5),
          axis.title = element_text(size = 12, face = 'bold'),
          axis.title.x = element_text(size = 12, face = 'bold'),
          axis.title.y = element_text(size = 12, face = 'bold'),
          axis.line = element_line(colour = 'black'),
          #Legend
          legend.key = element_blank(), # removes the border
          legend.key.size = unit(1, "cm"), # Sets overall area/size of the legend
          legend.text = element_text(size = 16, face = "bold"), # Text size
          title = element_text(size = 16, face = "bold")) +
        #ylim(-5,5)+
        coord_flip()
      print(p)
      dev.off()
    }
  }
}