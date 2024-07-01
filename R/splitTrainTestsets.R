#' @importFrom splitTools partition

splitTrainTestsets<-function(X,train = 0.7, valid = 0.1, test = 0.2, seed=150) {
  
  print("split dataset in train and test sets")
  set.seed(seed)
  inds <- partition( X$classi, p = c(train, valid, test ))
  trainset<-X[inds[[1]],]
  testset<-X[inds[[2]],]
  testset<-rbind(testset,X[inds[[3]],])
  #print(paste("dimensioni trainset: ",dim(trainset)[1]))
  #print(paste("dimensioni testset: ",dim(testset)[1]))
  
  lista<-list(trainset, testset)
}