dendroClusters<-function(dend,metric,clusteringMethod,w,h,res,f_row,
                         clustersColors,N_clusters,shadedCluster, outDir){
  
  # Plot
  filePath<-file.path(outDir,"dendroClusters.tiff")
  tiff(filePath, width=w, height=h,res=res)
  par(mar=c(3,1,1,6),cex=0.4)
  
  dend %>%
    dendextend::set("labels_cex", value =f_row, k = N_clusters) %>%
    dendextend::set("labels_colors", value =clustersColors , k = N_clusters) %>%
    dendextend::set("branches_lwd", value = 1) %>%
    dendextend::set("labels_col", value =clustersColors, k = N_clusters ) %>%
    dendextend::set("branches_k_color", value = clustersColors, k = N_clusters) %>%
    
    # ADD CATCHY GRAPHICS
    # Nodes
    dendextend::set("nodes_pch", 18)  %>% 
    dendextend::set("nodes_cex", 0.5) %>% 
    dendextend::set("nodes_col", "black") %>% 
    # Leaves
    dendextend::set("leaves_pch", 18)  %>% 
    dendextend::set("leaves_cex", 0.5) %>% 
    dendextend::set("leaves_col", "black") %>% 
    plot(horiz=TRUE, axes=FALSE)
  
  
  # add rectangles
  dendextend::rect.dendrogram( dend, k=N_clusters, lty = 5, lwd=0,  col=rgb(0.1, 0.2, 0.4, 0.2),
                   horiz=TRUE,which=shadedCluster) 
  
  dev.off()
}