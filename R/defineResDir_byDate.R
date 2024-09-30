defineResDir_byDate<-function(pipelineDir){
  
  myLabel<-("insert month (es: november)")
  input<-NULL
  while (is.null(input)){
    input<-shinyTextInput(myLabel)
  }
  mese<-input
  
  #filtro massivo
  myLabel<-("insert the year")
  input<-NULL
  while (is.null(input)){
    input<-shinyNumInput(myLabel)
  }
  anno<-input
  
  folder<-paste("res_",mese,"-",anno,sep="")
  resDir<-file.path(pipelineDir,folder)
  return(resDir)
  
}