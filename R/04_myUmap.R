#' @import uwot

myUmap<-function(inDir, outDir,  coloriAsFactor, pointsizeAsFactor, shapeAsFactor,
               w, h, res,legenda) {
  
  filePath<-file.path(inDir,"dataML_ZscoreTransformed.rds")
  dataML<- readRDS(filePath) ## dataML
  names(dataML)
  X<-dataML[,-1]
  row.names(X)<-rownames(dataML)
 
  ###########################################################################
  ### UMAP
  #########################################################################
  ### NB: necessita di un update dei package R causa problema con le "dll" e di package Matrox ver 1.6-1
  
  metric<-"manhattan"
  n_neighbors<-stimaNeighbors(data=dataML)
  
  ### PLOT 2D #################################################################
  dimensioni<-2
  res.umap<-umapMapping(X, dimensioni, metric,n_neighbors,
                        #y=as.factor(dataML$labels) #supervised case
                        )
  
  filename<-paste("umap_",dimensioni,"d.tiff",sep="")
  filePath<-paste(outDir,filename)
  suppressWarnings(
    p<-ggplotStandard(filePath, dimensioni, w,h,res, coloriAsFactor,
                    shapeAsFactor, pointsizeAsFactor, legenda, 
                    data=as.data.frame(res.umap)
                    )
  )
  
  ### PLOT 3D #################################################################
  dimensioni<-3
  res.umap<-umapMapping(X, dimensioni, metric,n_neighbors
                        #y=as.factor(dataML$labels) #supervised case
                        )
  
  filename<-paste("umap_",dimensioni,"d.tiff",sep="")
  filePath<-file.path(outDir,filename)
  suppressWarnings(
    p<-ggplotStandard(filePath, dimensioni, w,h,res, coloriAsFactor,
                    shapeAsFactor, pointsizeAsFactor, legenda, 
                    data=as.data.frame(res.umap)
                    )
  )
}

