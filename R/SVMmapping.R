SVMmapping<-function(dataSVM,kernel){
  
  seed<-150
  set.seed(seed)
  ##by tuning SVM
  print("tuning phase")
  tuning <- tune.svm(labels~., data = dataSVM, gamma = 10^(-6:-1), 
                   cost = 10^(1:2),scale=FALSE, kernel=kernel)
  summary( tuning )
  print("develop best model")
  model.tuned <- svm(labels~., data = dataSVM, 
                    gamma = tuning$best.parameters$gamma, cost = tuning$best.parameters$cost, 
                    scale=FALSE, kernel=kernel)
  summary(model.tuned )
  
 return(model.tuned)
 
 
}