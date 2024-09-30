askSudoPwd<-function(){
  
  
  myLabel<-("insert superuser pwd")
  input<-NULL
  while (is.null(input)){
    input<-shinyPwdInput(myLabel)
  }
  pwd<-input
  if (pwd=="") {pwd<-NULL}
  
  return(pwd)
  
}
