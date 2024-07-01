swappinessIncrease<-function( pwd){
  ### INCREASE SWAPPINESS
 
  if (!is.null(pwd)) { #riverifico dopo richiesta della password
    try({
      command<-paste("echo", pwd," | sudo -S sysctl vm.swappiness=90")
      system(command=command, intern = FALSE,
           ignore.stdout = TRUE, ignore.stderr = FALSE,
           wait = TRUE, input = NULL, timeout = 60)
      print("current swappiness")
      command<-"cat /proc/sys/vm/swappiness"
      system(command=command, intern = FALSE,
           ignore.stdout = FALSE, ignore.stderr = FALSE,
           wait = TRUE, input = NULL, timeout = 60)
      return (pwd)
  })
  }
  return(NULL) #se "try" va male
}#chiude funzione

swappinessDecrease<-function( pwd){
  ### DECREASE SWAPPINESS
  if (is.null(pwd)){
    return()
  } else { try({
    command<-paste("echo", pwd," | sudo -S sysctl vm.swappiness=60")
    system(command=command, intern = FALSE,
           ignore.stdout = TRUE, ignore.stderr = FALSE,
           wait = TRUE, input = NULL, timeout = 60)
    print("current swappiness")
    command<-"cat /proc/sys/vm/swappiness"
    system(command=command, intern = FALSE,
           ignore.stdout = FALSE, ignore.stderr = FALSE,
           wait = TRUE, input = NULL, timeout = 60)
  })#chiude try
  }#chiude else
}