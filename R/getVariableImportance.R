getVariableImportance<-function(nperm,conditional, OOB,rForest,
                                pval,mincriterion,outDir, w,h,r, pwd){
  
  print("=====================================")
  #applico alcuni dei parametri di default per velocizzare
  bestWay<-bestCPForVarImportance(rForest, pval,mincriterion, pwd=pwd
                                  #conditional, OOB=OOB, nperm=nperm,
                                  ) 
  print("=============================================")
  print(paste("Fastest for calculation is:", bestWay))
  
  ### LANCIA CALCOLO PARALLELO
  varImportance<-NULL
  selectedFeatures<-NULL
  
  tryCatch({
    varImportance<-calcVarImportance(nperm,conditional, OOB,rForest,
                              pval,mincriterion, bestWay, pwd)
    #genera barplot
    selectedFeatures<-plotVarImportance(varImportance,outDir,w,h,r)
    }, 
  error = function(err){ 
    kind<-"classic"
    if (conditional) {kind<-"conditional"}
    print(paste("Not possible to evaluate variables importance for",kind,"random forests"))
    }
  )
    
  return(selectedFeatures) 
}