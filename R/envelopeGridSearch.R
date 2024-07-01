#' @importFrom NMOF gridSearch

envelopeGridSearch<-function(levels,parameters){
  
  sol <- gridSearch(fun = testSettings, levels = levels, 
                    data=parameters$trainset, newdata=parameters$testset, 
                    e=parameters$e,
                    asList = TRUE, keepNames = TRUE)
  return(sol)
}