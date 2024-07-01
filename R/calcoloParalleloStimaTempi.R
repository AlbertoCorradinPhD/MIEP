#' @importFrom microbenchmark microbenchmark
#' @importFrom plyr llply

stimaViaPkgPlyr<-function(fun, x){
  
  #calcolo parallelo via doParallel package
  print("test methods from package 'doParallel'")
 
  
  suppressWarnings(
    stima<-microbenchmark(
      serial = {
        print("===================================================")
        print("by 'serial approach from Pkg doParallel'")
        llply(.data=x[1],.fun=fun,parameters=x, .parallel = FALSE, .progress="text")
        },
      parallel = {
        print("===================================================")
        print("by 'parallel approach from Pkg doParallel'")
        suppressWarnings(
          callLlply(x,fun)
        )
      },
      times = 1
    )
  )
  #str(stima)
  tempi<-stima$time/1e6
  attr(tempi,'names')<-c("serial","parallel")
  tempi<-tempi/1000/60
  print(paste("execution time for serial approach [min]:",tempi[1]))
  print(paste("execution time for parallel approach [min]:",tempi[2]))
  return(tempi)
 
}

stimaViaPkgParallel<-function(fun, x){
  
  #calcolo parallelo via parallel package
  print("test methods from package 'parallel'")
  
  suppressWarnings(
    stima<-microbenchmark(
      serial = {
        print("===================================================")
        print("by 'serial approach from Pkg parallel'")
        lapply(X=x[1], FUN=fun,parameters=x )
        },
      parL = { 
        print("===================================================")
        print("by 'parLapply from Pkg parallel'")
        suppressWarnings(
          callParLapply(x,fun)
        )
        },
      pb = { 
        print("===================================================")
        print("by 'pbapply from Pkg parallel'")
        suppressWarnings(
          callPbapply(x,fun)
          #chiama parLapply ma aggiunge progress bar
        )
      },
      mcL= {
        print("===================================================")
        print("by 'mclapply from Pkg parallel'")
        suppressWarnings(
          callMclapply(x,fun)
        )
        },
      times = 1
    )
  )
 
  tempi<-stima$time/1e6
  attr(tempi,'names')<-c("serial","parLapply","pbapply","mclapply")
  tempi<-tempi/1000/60
  print(paste("execution time for serial approach [min]:",tempi[1]))
  print(paste("execution time for parLapply [min]:",tempi[2]))
  print(paste("execution time for pbapply [min]:",tempi[3]))
  print(paste("execution time for mclapply [min]:",tempi[4]))
  return(tempi)
}