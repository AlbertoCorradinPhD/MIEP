expressionSet_print<-function(exprSet){
  print("newly created expression set:")
  print(class(exprSet))
  print(str(exprSet))
  print("dimension of the expression set")
  print(dim(exprSet))
  print("example features:")
  print(head(featureNames(exprSet),10))
  print("names of samples:")
  print(sampleNames(exprSet))
  print("annotations:")
  print(annotation(exprSet))
}