stateOfProcess_create<-function(e=globalenv()){

    seed <- get0(".Random.seed", envir = globalenv(), ifnotfound = NULL) 
    #seed<<-seed[-1]
    temp<-data.frame(seed)
    assign("randomSeeds", temp, envir = e)
    assign("counter", 0, envir = e)
    assign("confMatrices", list(), envir = e)
    assign("seed", seed, envir = e)

}