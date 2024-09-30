callMclapply<-function(x,fun){
  cores <- detectCores()
  results<-mclapply(X=x[1], FUN=fun, parameters=x,
             mc.preschedule = TRUE, mc.set.seed = TRUE,
             mc.silent = FALSE, mc.cores = getOption("mc.cores", cores),
             mc.cleanup = TRUE, mc.allow.recursive = TRUE
           )
  return(results)
}