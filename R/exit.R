exit <- function() {
  
 # .Internal(.invokeRestart(list(NULL, NULL), NULL))
  print("Forced exit from the pipeline")
  q()
}

