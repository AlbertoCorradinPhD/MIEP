makeGlobalPerc<-function(myVar, e=globalenv()){
    assign("perc", myVar, envir = e)
}

makeGlobalThresholdSig<-function(myVar, e=globalenv()){
    assign("thresholdSig", myVar, envir = e)
}
