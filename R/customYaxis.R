customYaxis<-function(){
  
  filename<-paste("shinyInputs.R",sep="")
  # asse y volcano plots
  myLabel<-("Asse y prestabilito? (0 for FALSE)")
  input<-NULL
  while (is.null(input)){
    input<-shinyNumInput(myLabel)
  }
  flagYaxis<-input
  return(flagYaxis)
  
}