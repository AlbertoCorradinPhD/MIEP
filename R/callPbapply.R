callPbapply<-function(x,fun){
  cores <- detectCores()
  OS<-Sys.info()['sysname']
  if (OS=="Linux") { cl<-makeForkCluster(cores) } else { cl<-makeCluster(cores) }
  print("clusters opened")
  # set progress bar
  pboptions(use_lb=TRUE, type="timer")
  splitpb(nx=1, ncl=cores, nout = cores)
  results<-pblapply(cl=cl , X=x[1], FUN=fun, parameters=x )  #chiama parLapply ma aggiunge progress bar
  print("going to close clusters")
  suppressWarnings(stopCluster(cl))
  print("clusters closed")
  return(results)
}