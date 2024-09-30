MAplot_shrComparison<-function(outDir,DeSeqObj,resultsObj,nameOfContrast,w,h,r,xlim_MA,ylim_MA){
 
  resLFC <- lfcShrink(DeSeqObj, coef=nameOfContrast, type="apeglm")
  resAsh <- lfcShrink(DeSeqObj, coef=nameOfContrast, type="ashr")
  
  tryCatch({
    resNorm <- lfcShrink(DeSeqObj, coef=nameOfContrast, type="normal")
    titolo<-"shrinkageMethodsComparison.tiff"
    filePath<-file.path(outDir,titolo)
    tiff(filePath, width=w, height=h, res=r)
    par(mfrow=c(2,2), mar=c(4,4,2,1))
    plotMA(resultsObj, xlim=xlim_MA, ylim=ylim_MA, main="none")
    plotMA(resLFC, xlim=xlim_MA, ylim=ylim_MA, main="apeglm")
    plotMA(resAsh, xlim=xlim_MA, ylim=ylim_MA, main="ashr")
    plotMA(resNorm, xlim=xlim_MA, ylim=ylim_MA, main="normal")
    dev.off()
  }, error = function(err){
    titolo<-"shrinkageMethodsComparison.tiff"
    filePath<-file.path(outDir,titolo)
    tiff(filePath, width=w, height=h, res=r)
    par(mfrow=c(2,2), mar=c(4,4,2,1))
    plotMA(resultsObj, xlim=xlim_MA, ylim=ylim_MA, main="none")
    plotMA(resLFC, xlim=xlim_MA, ylim=ylim_MA, main="apeglm")
    plotMA(resAsh, xlim=xlim_MA, ylim=ylim_MA, main="ashr")
    dev.off()
  })
  
}
