calcoloParalleloFindBest<-function( funzione, x,  
                                   pwd){
  
  OS<-Sys.info()['sysname']
  if (OS=="Linux"){
    mypwd<-swappinessIncrease( pwd=pwd) 
  }
  

  tempi1<-stimaViaPkgPlyr(fun=funzione, x=x)
  attr(tempi1,'names')<-c(paste(attr(tempi1,'names'),"PkgDoParallel", sep="_"))
  tempi2<-stimaViaPkgParallel(fun=funzione, x=x)
  attr(tempi2,'names')<-c(paste(attr(tempi2,'names'),"PkgParallel", sep="_"))
 
  #select best way based on time
  bestTime<-min(tempi1,tempi2)
  index<-match(x=bestTime, table=c(tempi1,tempi2))
  ways<-c(attributes(tempi1)$names,attributes(tempi2)$names)
  bestWay<-ways[index]
  
  if (OS=="Linux"){
    swappinessDecrease(  pwd=mypwd) 
  }
  return(bestWay)
  
}