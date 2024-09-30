parseName<-function(parola, isDir=FALSE) {
  
  parola<- sub(pattern="n \\?", replacement=" ?", x=parola,ignore.case = FALSE)
  flag<-grepl(pattern="ion$", x=parola, ignore.case = FALSE)
  if (!flag){
    parola<-sub(pattern="n$", replacement="", x=parola,ignore.case = FALSE)
  }
  if (!isDir){
    while (grepl(pattern="_", x=parola)){
      parola<- sub(pattern="_", replacement=" ", x=parola)
    }
  }
  return(parola)
}