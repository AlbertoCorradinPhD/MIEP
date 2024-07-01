MAplot<-function(resultsObj,outDir, w,h,r,xlim_MA,ylim_MA,flag_shrinkage){
 
  titolo<-"MAplot.tiff"
  filePath<-file.path(outDir,titolo)
  tiff(filePath, width=w, height=h, res=r)
  plotMA(resultsObj, ylim=ylim_MA, xlim=xlim_MA, main=flag_shrinkage)
  dev.off()

}