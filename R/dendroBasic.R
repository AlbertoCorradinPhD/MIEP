#' @importFrom dplyr select group_by

dendroBasic<-function(X,metric,clusteringMethod,w,h,res, cex, outDir){
  X<-as.data.frame(X) #serve sia dataframe
  dim(X)
  
  #BASIC DENDROGRAM
  X %>% 
    select(names(X)) %>% #dplyr
    dist(method = metric) %>%  #stats
    hclust(method = clusteringMethod ) %>%  #stats
    as.dendrogram() -> dend #stats
  
  # Plot
  filePath<-file.path(outDir,"dendroBasic.tiff")
  tiff(filePath, width=w, height=h,res=res)
  par(mar=c(3,1,1,8),cex=cex)  # Increase bottom margin to have the complete label
  p<-plot(dend,horiz=TRUE, axes=TRUE)
  print(p)
  #abline(v = 27, lty = 2,col="red")
  dev.off()
  return(dend)
}
