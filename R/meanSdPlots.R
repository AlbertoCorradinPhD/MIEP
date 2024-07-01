meanSdPlots<-function(DeSeqObj,outdir,w,h,res, metodi,nomi_metodi){
  
  for (i in 1:length(metodi)) {
    metodo<-metodi[[i]]
    filename<-paste("meanSdPlot_", nomi_metodi[i],".tiff",sep="")
    filePath<-file.path(outdir,filename)
    tiff(filePath, width=w, height=h, res=res)
    meanSdPlot(assay(metodo),bins  = 250, ranks = FALSE)
    dev.off()
  }
}