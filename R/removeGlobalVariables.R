removeGlobalVariables<-function(e=globalenv()){
  
  .pkgglobalenv <- get0(".pkgglobalenv", envir =e, ifnotfound = NULL)
  rm(.pkgglobalenv)
  print("Package environment removed")
  
}	
