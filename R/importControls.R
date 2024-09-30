importControls<-function(dataDir){
  
  controls<-NULL
  
  ### READ CONTROLS
  filePath<-file.path(dataDir,"controls.txt")
  tryCatch( {
    controls<-unique(scan(file = filePath, what = character(),
                       sep = "\n",strip.white = TRUE))
    if (controls[1]=="") {controls<-NULL}
    },
    error = function(err){
      print("no control to compare conditions")
      exit()
    })#chiudo tryCatch
  
  return(controls)
}
