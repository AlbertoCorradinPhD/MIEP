

generaDistMatrices<-function(outdir,w,h, res,metodi,nomi_metodi){
  titolo<-"Distance matrix: graphical representation"
  
  for (i in 1:length(metodi)) {
    metodo<-metodi[[i]]
    filename<-paste("sampleDistMatrix_", nomi_metodi[i],".tiff",sep="")
    filePath<-file.path(outdir,filename)
    sampleDists <- dist(t(assay(metodo)), method="manhattan")

    tiff(filePath, width=w, height=h, res=res)
    sampleDistMatrix <- as.matrix(sampleDists)
    rownames(sampleDistMatrix) <- paste(metodo$condition, metodo$cellLine, sep="-")
    colnames(sampleDistMatrix) <- rownames(sampleDistMatrix)
    colors <- colorRampPalette( rev(brewer.pal(9, "Blues")) )(255) #9 is max number of colors in the palette
    p<-pheatmap(sampleDistMatrix,
                clustering_distance_rows=sampleDists,
                clustering_distance_cols=sampleDists,
                color=colors, main=titolo)
    print(p)
    dev.off()
  }
  
}