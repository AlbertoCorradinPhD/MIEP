callLlply<-function(x,fun){
  cores <- detectCores()
  print("clusters opened")
  
  OS<-Sys.info()['sysname']
  if (OS=="Linux") { cl<-makeForkCluster(cores) } else { cl<-makeCluster(cores) }
  registerDoParallel(cl=cl, cores=cores)
  results<-llply(.data=x[1],.fun=fun,parameters=x, .parallel = TRUE )
  print("going to close clusters")
  suppressWarnings(stopCluster(cl))
  print("clusters closed")
  return(results)
}