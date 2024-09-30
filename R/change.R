changeRowS<-function(rowS, linea){
  
    myLabel<-paste("Look heatmap for",linea ,". Is row split ok? No is default")
    input<-NULL
    while (is.null(input)){
      input<-shinyBooleanInput(myLabel)
      }
    ans<-input
    
    if (!ans){ 
      myLabel<-("insert number of clusters to be identified")
      input<-NULL
      while (is.null(input)){
        input<-shinyNumInput(myLabel, value=2)
      }
      rowS<-input
      #caso particolare
      if (rowS==1) { rowS<-NULL }
    } else { ans<-TRUE }
  

  lista<-list(ans, rowS)
  return(lista)
}

changeShadedCluster<-function(shadedCluster, linea){
  
  myLabel<-paste("Look dendrogram for",linea ,". Did I shade right cluster? No is default")
  input<-NULL
  while (is.null(input)){
    input<-shinyBooleanInput(myLabel)
  }
  ans<-input
  
  if (!ans){ 
    myLabel<-("insert cluster to shade")
    input<-NULL
    while (is.null(input)){
      input<-shinyNumInput(myLabel)
    }
    shadedCluster<-input
  } else { ans<-TRUE }
  
  lista<-list(ans, shadedCluster)
  return(lista)
}