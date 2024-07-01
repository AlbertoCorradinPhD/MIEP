#' @importFrom plyr llply

calcoloParallelo<-function(fun,x, way,  pwd){
  
  
  OS<-Sys.info()['sysname']
  if (OS=="Linux"){
    mypwd<-swappinessIncrease( pwd=pwd) 
  }
  
  
  #####################################################################
  ### SELECT THE WAY
  ###################################################################
  print(paste("computation performed by':", way))
  
  if (way=="serial_PkgDoParallel") {
    suppressWarnings(
      results <- llply(.data=x[1],.fun=fun,parameters=x, .progress="text")
    )
  } 
  if (way=="parallel_PkgDoParallel") {
    suppressWarnings(
      results <-callLlply(x,fun)
    )
  } 
  if (way=="serial_PkgParallel") { 
    suppressWarnings(
     results <-lapply(X=x[1], FUN=fun,parameters=x )
    )
  } 
  if (way=="parLapply_PkgParallel") { 
    suppressWarnings(
      results <-callParLapply(x,fun)
    )
  }
  if (way=="pbapply_PkgParallel") { 
    suppressWarnings(
      results <-callPbapply(x,fun)
      #chiama parLapply ma aggiunge progress bar
    )
  }
  if (way=="mclapply_PkgParallel") { 
    suppressWarnings(
      results<-callMclapply(x,fun)
    )
  }
  
  if (OS=="Linux"){
    swappinessDecrease( pwd=mypwd) 
  }
  return(results)
}