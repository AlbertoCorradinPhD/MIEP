envelopeVarImportance <- function(nperm, parameters) {
  
  threshold <- (1-parameters$pval)

  
  if (!is.null(parameters$rForest)){
    varImportance<-varimp(object=parameters$rForest, 
                          mincriterion=parameters$mincriterion, 
                          conditional=parameters$conditional, 
                          threshold=threshold,
                          nperm=nperm, OOB=parameters$OOB)
    return(varImportance)
  }
}