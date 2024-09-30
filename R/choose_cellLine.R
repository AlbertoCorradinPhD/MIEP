choose_cellLine <- function(coldata,  flag_cellLine) {
  
  if ( flag_cellLine=="Filter") { 
    return(coldata)
  } else  {
    boolean<-  flag_cellLine %in% coldata$cellLine
    if (!boolean){
      print("erroneous run")
      exit() 
    } else {
    #estrae la singola linea cellulare
    coldata <- coldata[coldata$cellLine== flag_cellLine,]  
    return(coldata)
    }
  }
}