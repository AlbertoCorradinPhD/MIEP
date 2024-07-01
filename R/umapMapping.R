umapMapping<-function(X, dimensioni, metric,n_neighbors,y=NULL) {
  
  seed<-150
  set.seed(seed)
  res.umap  <- umap(X, n_neighbors = n_neighbors, learning_rate = 0.05, init = "spectral",
                    metric =metric, n_components = dimensioni, 
                    scale=FALSE, pca_center = FALSE, y=y
                  )
  
  return(res.umap)
}
