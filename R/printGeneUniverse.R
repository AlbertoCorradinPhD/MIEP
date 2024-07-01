printGeneUniverse<-function(geneUniverse) {
  #The topDiffGenes function included in this dataset will select the differentially expressed genes, 
  # at 0.01 significance level, from geneUniverse. Boolean
  
  print("Let's have a look to Gene Universe:")
  print("number of features in the Gene Universe:")
  print(length(geneUniverse))
  bestGenes<-topGenes(geneUniverse) #thresholdSig variabile globale
  #print("Significant features:")
  #print(bestGenes[bestGenes==TRUE])
  print("Number of significant features:")
  N_sig<-sum(bestGenes)
  print(N_sig)
  return(N_sig)
}