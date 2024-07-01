generaScoreTable<-function(object,exprSet,selectedGenes, importance=NULL) {
  
  ### CREATE GENE UNIVERSE WITH SCORES
  
  #print(selectedGenes)
  seqs_withSymbol<-row.names(exprs(exprSet))
  scoreTable<-data.frame(importance=rep(0,length(seqs_withSymbol)), feature=seqs_withSymbol)
  
  if (object=="PCA" && !is.null(importance)) { #open if
    for (g in 1:length(selectedGenes)){
      gene<-selectedGenes[g]
      index<-match(x=gene, table=scoreTable$feature, nomatch = NA, incomparables = NULL)
      #check
      if (is.na(index)){print(paste("problems with the following feature:",gene))}
      scoreTable$importance[index]<-importance[g]
    }#chiude for
  } else  {
    boolean<-seqs_withSymbol %in% selectedGenes
    scoreTable$importance[boolean]<-1
    ### check  contenuto of scoreTable
    checkScoreTable(scoreTable,selectedGenes)
  }
  
  return(scoreTable)
}
