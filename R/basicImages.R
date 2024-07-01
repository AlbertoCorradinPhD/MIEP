#' @importFrom pheatmap pheatmap
#' @import apeglm
#' @import ashr
#' @importFrom vsn meanSdPlot

basicImages <- function(DeSeqObj,testDir) {
  
  outdir<-file.path(testDir,"basicImages")
  dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
  
  ### TRASFOMAZIONI
  # anzich? fare semplicemente log2, si tratta di fittare un modello che include l'intercetta
  vsd <- vst(DeSeqObj, blind=TRUE)
  rld <- rlog(DeSeqObj, blind=TRUE, fitType='local')
  ntd <- normTransform(DeSeqObj)  # this gives log2(n + 1)
  metodi<-list(vsd,  rld, ntd)
  nomi_metodi<-c("vsd",  "rld", "ntd")
  
  ### PLOT SETTINGS
  w<-5000
  h<-5000
  res<-500
  
  ######### meanSdPlots ##########
  #inguardabile
  meanSdPlots(DeSeqObj,outdir,w,h,res, metodi,nomi_metodi)
  
  ######### PHEATMAPS ##########
  generaPheatmaps(DeSeqObj,outdir,w,h, res,metodi,nomi_metodi)
  
  ####### HEATMAP OF THE SAMPLE-TO-SAMPLE DISTANCES WITH CLUSTERING ##########
  generaDistMatrices(outdir,w,h, res,metodi,nomi_metodi)
  
  ###### PRINCIPAL COMPONENT PLOT OF THE SAMPLES ############
  generaPcaPlots(outdir,w,h, res,metodi,nomi_metodi, DeSeqObj)
  
}
