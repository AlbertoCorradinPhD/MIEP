randomForest<-function(fcontrols,data, newdata,seed=NULL){
 
  #seed
  if (!is.null(seed)) {
    print("Gotta seed for random forest construction")
    set.seed(seed)
  } 
  
  rForest<- cforest(formula=classi ~ ., data =data, 
                          controls =fcontrols )
  summary(rForest)
  
  #### MODEL EVALUATION
  rForest.predict  <- predict(rForest, newdata = newdata)
  # a volte richiede di aggiungere OOB=TRUE
  if (is.null(newdata)) {
    CM<-confusionMatrix(rForest.predict , data$classi)
  } else  {
    CM<-confusionMatrix(rForest.predict , newdata$classi)
  }
  lista<-list(CM, rForest)
  return(lista)
}