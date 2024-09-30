 
plotHistograms<-function(gene,gene_counts, outDir,  w,h,r ){
  
  dir.create(outDir, showWarnings = FALSE, recursive = TRUE)
  #par(las=3) #labels verticali
  #in scala lineare
  filename<-paste(gene,".jpeg", sep="")
  filePath<-file.path(outDir,filename)
  jpeg(filePath, width=w, height=h,res=r)
  #print(file.path("disegna histogram: ", gene))
  p<-ggplot(data=gene_counts, aes( x=sample, y=counts, fill = .data$condition ) )+ 
    geom_bar(position="dodge",stat='identity',colour="black")+
    scale_fill_brewer(palette="Pastel1")+ ggtitle(label=gene) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  print(p)
  #print(file.path("disegna histogram: ", gene,"fatto"))
  dev.off()
  
  # WRITE DOWN DATA
  filename<-paste(gene,".html", sep="")
  filePath<-file.path(outDir,filename)
  printTableHTML(data=gene_counts,titolo=gene,filePath)
}  
