tsneMapping<-function(X, dimensioni, perplexity, exaggeration_factor) {
  seed<-150
  set.seed(seed)
  res.tsne <- Rtsne(X, dims = dimensioni, perplexity=perplexity, verbose=FALSE, 
                    exaggeration_factor=exaggeration_factor, max_iter=1e6, 
                    pca=FALSE, eta=50, theta = 0, #exact t-SNE
                  )# niente PCA per avere una rappresentazione diversa
  
  #exeTimeTsne<- system.time(Rtsne(data[,-1], dims = 2, perplexity=30, 
  #verbose=TRUE, max_iter = 500))
  
  return(res.tsne)
}
