settingsGeneUniverse<-function(object){
  
  myLabel<-("Do you want to change default settings for gene universe? (not recommended)")
  input<-NULL
  while (is.null(input)){
    input<-shinyBooleanInput(myLabel)
  }
  ans<-input
  
  if (ans){
    #percentile
    myLabel<-paste("insert percentile (e.g. 90,95,99) to set new threshold.",
          "This will change the set of significant genes ")
    input<-NULL
    while (is.null(input)){ input<-shinyNumInput(myLabel)  }
    perc<-input
    
    #ntop number
    myLabel<-("insert desired number of top GO terms. Default is 10 GO terms. ")
    input<-NULL
    while (is.null(input)){ input<-shinyNumInput(myLabel) }
    ntop<-input
  } else {
    #default
    if (object=="PCA") {perc<-99} else {perc<-90}   
    ntop<-10 
  }
  
  lista<-list(perc,ntop)
  return(lista)
}
