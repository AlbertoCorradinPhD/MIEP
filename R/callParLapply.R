callParLapply<-function(x,fun){
  cores <- detectCores()
  OS<-Sys.info()['sysname']
  if (OS=="Linux") { cl<-makeForkCluster(cores) } else { cl<-makeCluster(cores) }
  print("clusters opened")
  results<-parLapply(cl=cl , X=x[1], fun=fun, parameters=x )  #chiama parLapply ma aggiunge progress bar
  print("going to close clusters")
  suppressWarnings(stopCluster(cl))
  print("clusters closed")
  return(results)
}