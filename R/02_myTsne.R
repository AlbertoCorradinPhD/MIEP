#' @import Rtsne

myTsne<-function(inDir, outDir,  coloriAsFactor, pointsizeAsFactor, shapeAsFactor,
              w, h, res,legenda){
  

  filePath<-file.path(inDir,"dataML_ZscoreTransformed.rds")
  dataML<- readRDS(filePath) ## dataML
  names(dataML)
  X<-dataML[,-1]
  row.names(X)<-rownames(dataML)
  
  ###########################################################################
  ####### tSNE
  ###############################################################################
  perplexity<-2
  exaggeration_factor<-25
  
  
  ############# 3D tSNE ######################################################
  print(paste("tSNE 3D"))
  dimensioni<-3
  res.tsne<-tsneMapping(X, dimensioni, perplexity, exaggeration_factor)
  filename<-paste("tSNE_",dimensioni,"d.tiff",sep="")
  filePath<-file.path(outDir,filename)
  suppressWarnings(
    p<-ggplotStandard(filename=filePath, dimensioni, w,h,res, coloriAsFactor,
                    shapeAsFactor, pointsizeAsFactor, legenda, 
                    data=as.data.frame(res.tsne$Y)
                    )
  )
  
  
  ############# 2D tSNE ######################################################
  print(paste("tSNE 2D"))
  dimensioni<-2
  res.tsne<-tsneMapping(X, dimensioni, perplexity, exaggeration_factor)
  filename<-paste("tSNE_",dimensioni,"d.tiff",sep="")
  filePath<-file.path(outDir,filename)
  suppressWarnings(
    p<-ggplotStandard(filePath, dimensioni, w,h,res, coloriAsFactor,
                    shapeAsFactor, pointsizeAsFactor, legenda, 
                    data=as.data.frame(res.tsne$Y)
                    )
  )
  tSNE2D<-list(res.tsne, p)
  filePath<-file.path(outDir,"tSNE2D.rds")
  saveRDS(object=tSNE2D, file = filePath)
  return(tSNE2D)
}

