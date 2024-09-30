checkScoreTable<-function(scoreTable,selectedGenes){
  print("number of significant features:")
  print(sum(scoreTable$importance))
  print("do they correspond to the ones indicated by the user?")
  boolean<-sum(scoreTable$importance)==length(selectedGenes)
  print(boolean)
  if (!boolean){
    print("number of missing features:")
    print(length(selectedGenes)-sum(scoreTable$importance!=0))
    print("verify eventual collapsing in the expression set construction")
  }

}