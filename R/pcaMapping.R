pcaMapping<-function(res.pca, outDir){
  
 
  # EIGENVALUES
  eigenvalues <- get_eigenvalue(res.pca)
  filePath<-file.path(outDir,"eigenvalues.tsv")
  write.table(x=eigenvalues , file = filePath, append = FALSE, quote = FALSE, sep = "\t",
              eol = "\n", na = "NA", dec = ".", row.names = TRUE,
              col.names = TRUE)
  
  
  # EIGENVECTORS
  #rotation: the matrix of variable loadings (i.e., a matrix whose columns contain 
  # the eigenvectors).
  eigenvectors<-res.pca$rotation
  dim(eigenvectors)
  filePath<-file.path(outDir,"eigenvectors.tsv")
  write.table(x=eigenvectors, file = filePath, append = FALSE, quote = FALSE, sep = "\t",
              eol = "\n", na = "NA", dec = ".", row.names = TRUE,
              col.names = TRUE)
  
  # RESULTS FOR INDIVIDUALS
  res.ind <- get_pca_ind(res.pca)
  #head(res.ind$coord)          # Coordinates
  #head(res.ind$contrib)        # Contributions to the PCs
  #head(res.ind$cos2)           # Quality of representation 
  
  filePath<-file.path(outDir,"individualsCoordinates.tsv")
  write.table(x=res.ind$coord, file = filePath, append = FALSE, quote = FALSE, sep = "\t",
              eol = "\n", na = "NA", dec = ".", row.names = TRUE,
              col.names = TRUE)
  
  
  # RESULTS FOR VARIABLES 
  res.var <- get_pca_var(res.pca)
  #head(res.var$coord)          # Coordinates
  #head(res.var$contrib)        # Contributions to the PCs
  #head(res.var$cos2)           # Quality of representation 
  
  coordinates<-as.data.frame(res.var$coord)
  filePath<-file.path(outDir,"variablesCoordinates.tsv")
  write.table(x=coordinates , file = filePath, append = FALSE, quote = FALSE, sep = "\t",
              eol = "\n", na = "NA", dec = ".", row.names = TRUE,
              col.names = TRUE)
  
}