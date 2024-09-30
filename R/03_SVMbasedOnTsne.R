SVMbasedOnTsne<-function(tSNE2D, outDir,  coloriAsFactor, pointsizeAsFactor, shapeAsFactor,
               w, h, res,legenda){
  
  ###########################################################################
  ####### SUPPORT VECTOR MACHINE
  ###############################################################################
  
  #filePath<-file.path(outDir,"tSNE2D.rds")
  #tSNE2D<-readRDS(file = filePath)
  tsne<-tSNE2D[[1]]
  
  #with tsNE-transformed data
  dataSVM<-as.data.frame(tsne$Y)
  dataSVM$labels<-as.factor(names(coloriAsFactor))
  names(dataSVM)<-c("tSNE_dimension1","tSNE_dimension2","labels" )
  
  #predisposizione per disegno
  p<-tSNE2D[[2]]
  xmin<-layer_scales(p)$x$range$range[1]
  xmax<-layer_scales(p)$x$range$range[2]
  ymin<-layer_scales(p)$y$range$range[1]
  ymax<-layer_scales(p)$y$range$range[2]
  
  kernels<-c("linear","polynomial","radial")
  for (k in 1:length(kernels)){
    kernel<-kernels[k]
    print(paste("SVM:",kernel,"kernel" ))
    model.tuned<-SVMmapping(dataSVM, kernel)
    
    #plot
    print(paste("classification plot"))
    filename<-paste("SVM_",kernel,".tiff",sep="")
    filePath<-file.path(outDir,filename)
    SVMclassificationPlot(filePath, model.tuned, dataSVM, w,h,res, 
                          xlim=c(xmin,xmax), ylim=c(ymin,ymax),
                          coloriAsFactor)
    
  }#chiude ciclo for kernel
}