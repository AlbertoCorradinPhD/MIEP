SVMclassificationPlot<-function(filename, model.tuned, dataSVM, w,h,res, xlim, ylim,coloriAsFactor){
  tiff(filename, width=w, height=h,res=res)
  plot(x=model.tuned, data=dataSVM,formula= (tSNE_dimension2~tSNE_dimension1), 
       fill = TRUE, grid = 50*50, slice = list(), #50*10 100*20
       col=levels(coloriAsFactor), xlim,ylim,
       svSymbol = "", dataSymbol = "", symbolPalette =  "black"
    )
  text(x=dataSVM[,1]*0.75, y=dataSVM[,2]*0.95, labels=dataSVM$labels, col="black",cex=1)
  dev.off()
  }