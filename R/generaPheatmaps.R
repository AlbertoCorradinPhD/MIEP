generaPheatmaps<-function(DeSeqObj,outdir,w,h, res,metodi,nomi_metodi){
  matrice<-counts(DeSeqObj,normalized=FALSE)
  N<-min(dim(DeSeqObj)[1],50)
  select <- order(apply(X=matrice, MARGIN=1, FUN=sd), decreasing=TRUE)[1:N]
  df <- as.data.frame(colData(DeSeqObj)[,c("condition","cellLine")])
  
  titolo<-paste("Heatmap with the", N, "features with the highest variance")
  for (i in 1:length(metodi)) {
    metodo<-metodi[[i]]
    filename<-paste("pheatmap_", nomi_metodi[i],".tiff",sep="")
    filePath<-file.path(outdir,filename)
    tiff(filePath, width=w, height=h, res=res)
    p<-pheatmap(assay(metodo)[select,], cluster_rows=TRUE, show_rownames=TRUE,
                cluster_cols=TRUE, annotation_col=df,show_colnames=FALSE,
                main=titolo)
    print(p)
    dev.off()
  }#chiude ciclo
}