stimaNeighbors<-function(data){
  
  by_labels <- data %>% group_by(labels)
  ragruppamenti<-attributes(by_labels)$groups[,2][[1]]
  media<-mean(sapply(X=ragruppamenti, FUN=length, simplify = TRUE, USE.NAMES = TRUE))
  stima<-round(x=media, digits = 0)
  return(stima)
}