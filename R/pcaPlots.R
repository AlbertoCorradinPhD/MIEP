# plots samples
samplesPlot<-function(res.pca,plotsDir, w,h,res,i,j, coloriAsFactor, 
                      shapeAsFactor,legenda, explainedVariances, coldata){
  
  outDir<-file.path(plotsDir,"samples")
  dir.create(outDir, showWarnings = FALSE, recursive = TRUE)
  titolo<-"Principal component analysis (PCA), based on SVD"
  #i<-1
  #j<-2
  #x11()
  filename<-paste("comps",i,"and",j, ".tiff",sep="")
  filePath<-file.path(outDir,filename)
  tiff(filePath, width=w, height=h,res=res)
  temp<-as.data.frame(res.pca$x)
  data<-cbind(temp[,c(i,j)],coldata$condition,coldata$cellLine)
  xLabel<-paste("PC",i," (",round(explainedVariances[i],1),"%)",sep="")
  yLabel<-paste("PC",j," (",round(explainedVariances[j],1),"%)",sep="")
  M<-dim(data)[2]
  names(data)[1:M]<-c(paste("V",1:M, sep=""))
  p<-ggplot(data,aes(x=.data$V1, y=.data$V2, 
                     colour=names(coloriAsFactor), 
                     shape=names(shapeAsFactor), 
                     
  )) + 
    geom_point(size=5)+ theme_bw()+ggtitle(titolo)+
    xlab(label=xLabel)+ ylab(label=yLabel)
  print(p)
  p<-p+ guides( colour = guide_legend(title = legenda[1]))
  print(p)
  p<-p+ guides( shape = guide_legend(title = legenda[2]))
  print(p)
  p<-p+geom_hline(yintercept=0)
  print(p)
  p<-p+geom_vline(xintercept=0)
  print(p)
  dev.off()
  return(p)
}

variablesPlot<-function(res.pca,plotsDir, w,h,res,i,j,  M=10){
  
  customPalette<-customPaletteBalanced()
  
  outDir<-file.path(plotsDir,"variables")
  dir.create(outDir, showWarnings = FALSE, recursive = TRUE) 
  legenda<-"Importance"
  titolo<-paste("Top",M,"variables for principal components",i,"and",j)
  #i<-1
  #j<-2
  #x11()
  filename<-paste("comps",i,"and",j, ".tiff",sep="")
  filePath<-file.path(outDir,filename)
  tiff(filePath, width=w, height=h,res=res)
  p<-fviz_pca_var(res.pca, axes = c(i, j),
                  col.var = "contrib", # Color by contributions to the PC
                  gradient.cols = customPalette,
                  repel = TRUE, # Avoid text overlapping
                  select.var=list(name = NULL, cos2 = NULL, contrib = M),
                  title=titolo, 
                  legend.title=legenda, font.legend=c(12)
                  
  )
  print(p)
  dev.off()
  return(p)
}

biPlot<-function(res.pca,plotsDir, w,h,res,i,j, coloriAsFactor, pointsizeAsFactor, legenda, M=10){
  
  outDir<-file.path(plotsDir,"biplots")
  dir.create(outDir, showWarnings = FALSE, recursive = TRUE)
  titolo<-paste("Biplot with top",M,"variables")
  #i<-1
  #j<-2
  #x11()
  filename<-paste("comps",i,"and",j, ".tiff",sep="")
  filePath<-file.path(outDir,filename)
  tiff(filePath, width=w, height=h,res=res)
  p<-fviz_pca_biplot(res.pca, axes = c(i, j), #
                     col.var = "#DE3163", #colore delle variabili
                     palette=levels(coloriAsFactor),
                     col.ind = names(coloriAsFactor), fill.ind=names(coloriAsFactor),   
                     repel = TRUE,     # Avoid text overlapping
                     select.var=list(name = NULL, cos2 = NULL, contrib = M),
                     geom="point", mean.point = FALSE,
                     pointsize = pointsizeAsFactor, 
                     font.x=c(12), font.y=c(12),
                     legend.title=legenda[1], font.legend=c(12),
                     title=titolo, font.main=c(12)
  )
  print(p)
  p<-p+ guides( size = guide_legend(title = legenda[2]))
  print(p)
  p<-p+scale_size_discrete(labels=unique(names(pointsizeAsFactor)))
  print(p)
  dev.off()
}