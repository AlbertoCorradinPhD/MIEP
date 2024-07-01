#' @importFrom gplots heatmap.2

generaScacchiera<-function( DESeqDir, cellLines, confronti, inDir, outDir, DEids){
  
  nomiColonne<-attr(DEids[[1]], "names")
  
  
  ### FEATURES NELLA SCACCHIERA
  #passo iniziale
  features<-DEids[[1]][[1]]
  #passo iterativo
  for(j in 1: length(confronti)){
    for(k in 1: length(cellLines)){ 
      features<-unique(union(features,DEids[[j]][[k]]))
    }
  }  
  #pulisci features (eventualmente)
  boolean<-features != ""
  features<-features[boolean]
  #print("features nelle scacchiera:")
  #print(length(features))
  data<-formattazioneDatiScacchiera(features, database=DEids, nomiColonne,
                                    inDir,DESeqDir,confs=confronti,myPattern="NA")
  
  filename<-file.path(outDir,"scacchiera.png")
  png(filename, width=1000, height=15000,res=300)
  col <- colorRampPalette(c("blue","grey","red"))(30) 
  heatmap.2(as.matrix(data),  Rowv = T, Colv = F, dendrogram = "none", margins = c(10, 5), 
            scale = NULL, col = col, density.info = "none", trace = "column",tracecol="black",
            cexCol=0.75,cexRow=0.25,lhei=c(0.75,10), #dimensione righe e colonne
            key = FALSE
  )
  dev.off()

}