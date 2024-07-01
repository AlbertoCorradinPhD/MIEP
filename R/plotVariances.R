
#varianze per principal component
variancesPlot<-function(res.pca, outDir, w,h,res, M=10){
  filePath<-file.path(outDir,"varianceXpc.tiff")
  tiff(filePath, width=w, height=h,res=res)
  par(cex=1.5)
  p<-fviz_eig(res.pca,  barcolor = "grey",barfill="grey",
              linecolor = "black",
              ncp = M, addlabels = TRUE,main="",
              choice = "variance"
  )
  print(p)
  dev.off()
  return(p)
}

#eigenvalue per principal component
eigenvaluesPlot<-function(res.pca, outDir, w,h,res, M=10){
  filePath<-file.path(outDir,"eigenvaluesXpc.tiff")
  tiff(filePath, width=w, height=h,res=res)
  par(cex=1.5)
  p<-fviz_eig(res.pca,  barcolor = "grey",barfill="grey",
              linecolor = "black",
              ncp = M, addlabels = TRUE,main="",
              choice = "eigenvalue"
  )
  print(p)
  dev.off()
  return(p)
}
