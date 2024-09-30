classificationMethod<-function(coldata, input=NULL){
  
  repeat {
    myLabel<-paste("insert set of classes to classify samples with random forests.", 
		"Choose among the following options: Cell line, condition, both")
    while (is.null(input)){
      input<-shinyTextInput(myLabel)
    }
    ans<-input
    
    if (ans=="CellLine" || ans=="cellLine" || ans=="Cell Line" || ans=="cell line") {
      classi<-factor( coldata$cellLine)
      ans<-"CellLine"
      break
    } else if (ans=="Condition" || ans=="condition" || ans=="conditions" || ans=="Conditions") {
      classi<-factor( coldata$condition)
      ans<-"Condition"
      break
    } else  if (ans=="Both" || ans=="both" ) {
      classi<-paste(coldata$condition, coldata$cellLine, sep="_")
      classi<-factor( classi)
      ans<-"Both"
      break
    }
  
  }
  lista<-list(classi,ans)
  return(lista)
}
