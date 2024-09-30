settingsXvolcanoPlot<-function(){
  
  lista<-list(yLimit=NULL, xLimit=NULL)
  
  myLabel<-("Apply a custom upper limit for y-axis in volcano plots. No is default")
  input<-NULL
  while (is.null(input)){
    input<-shinyBooleanInput(myLabel)
  }
  flag<-input
  
  if (flag) {
    myLabel<-("Insert y-axis limit then")
    input<-NULL
    while (is.null(input)){
      input<-shinyNumInput(myLabel)
    }
    yLimit<-input
    lista$yLimit=yLimit
  }
  
  myLabel<-("Apply a custom upper limit for x-axis in volcano plots. No is default")
  input<-NULL
  while (is.null(input)){
    input<-shinyBooleanInput(myLabel)
  }
  flag<-input
  
  if (flag) {
    myLabel<-("Insert x-axis limit then")
    input<-NULL
    while (is.null(input)){
      input<-shinyNumInput(myLabel)
    }
    xLimit<-input
    lista$xLimit=xLimit
  }
  
  
  return(lista)
}