singleConditionalTree<-function(controls, data,newdata, seed=NULL) {
  
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  singleCtree <- ctree(classi ~ ., data=data, controls=controls)
  #print(singleCtree)
  
 
  #### MODEL EVALUATION
  ctree.predict <- predict(singleCtree, newdata)
  if (is.null(newdata)) {
      CM<-confusionMatrix(ctree.predict , data$classi)
  } else  {
      CM<-confusionMatrix(ctree.predict , newdata$classi)
    }
  lista<-list(CM, singleCtree)
  return(lista)
}
