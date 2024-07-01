choose_conditions <- function(coldata, conditions) {
  if (!is.null(conditions)){
  coldata <- coldata[coldata$condition %in% conditions,]  
   } else { 
    print("erroneous run")
    exit() 
  } 
  
  return(coldata)
}

