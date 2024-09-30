checkControls<-function(conditions, controls){
  
  if (is.null(controls)) {
    print("no control at all")
    print("Please, take care of providing controls in appropriate control files")
    exit()
  } 
  else if  (!all(controls %in% conditions)) {
       print("wrong control")
      print("Please, take care of providing RIGHT controls in appropriate control files")
      exit()
  } else print("provided controls are acceptable")

}