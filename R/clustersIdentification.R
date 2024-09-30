clustersIdentification<-function(dend,inDir,outDir,cutoff, N_clusters ){
  
  clusters <- cutree(
    tree=dend,
    k = N_clusters
  )
  features<-attributes(clusters)$names
  df_clusters<-data.frame(clusters=clusters, gene_symbols= features)
  filePath<-file.path(outDir,"df_clusters.tsv")
  write.table(x=df_clusters, file =filePath, append = FALSE, quote = FALSE, 
              sep = "\t", row.names = FALSE, col.names = TRUE
  )
  
  ###### ANNOTATIONS PER CLUSTER
  names(df_clusters)
  filePath<-file.path(inDir,"annotations_cleaned.tsv")
  annotations<-read.table(file=filePath, header = TRUE, sep = "\t", quote="\"",
                          strip.white = FALSE, blank.lines.skip = FALSE,
                          stringsAsFactors = FALSE
  )
  names(annotations)
  
  
  #loop
  for (i in 1:N_clusters){
    df_temp<-subset(x=df_clusters, clusters==i)
    M<-dim(df_temp)[1]
    if (M>cutoff){
      #subset
      boolean<-as.vector(annotations$hgnc_symbol) %in% df_temp$gene_symbols 
      #print(sum(boolean))
      #test<-setdiff(df_temp$gene_symbols ,as.vector(annotations$hgnc_symbol))
      
      annotations_sub<-annotations[boolean,]
      
      #write file
      filename<-paste("annotations_cluster",i,".tsv",sep="")
      filePath<-file.path(outDir,filename)
      write.table(x=annotations_sub, file = filePath, append = FALSE, quote = FALSE, 
                  sep = "\t", row.names = FALSE, col.names = TRUE
      )
    }#chiude if
    
  }#chiude for
  
}