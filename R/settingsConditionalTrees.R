settingsConditionalTrees<-function( pval, testtype, teststat, ntrees){
  
  mincriterion <- ifelse(testtype != "Teststatistic", (1-pval),qnorm(1-pval))
  #print(paste("mincriterion is:" , mincriterion))
  maxsurrogate <- 3
  minsplit <-0 
  minbucket<- 0
  mtry<-5
  nresample <- 100 #for MonteCarlo
  controls<-ctree_control(teststat = teststat, 
                          testtype = testtype, 
                          mincriterion = mincriterion, 
                          stump = FALSE, nresample = nresample, maxsurrogate = maxsurrogate, 
                          savesplitstats = TRUE, maxdepth = 0, remove_weights = FALSE,
                          mtry=mtry,minsplit=minsplit, minbucket=minbucket
  )
  
  
  #print(cforest_unbiased()) #parametri suggeriti
 
  fcontrols<-cforest_control(teststat = teststat,
                             testtype = testtype,
                             mincriterion = mincriterion,
                             savesplitstats = TRUE, mtry=mtry,
                             ntree = ntrees, replace = TRUE,
                             trace = TRUE,  #fraction = 0.75,
                             maxdepth = 0, remove_weights = FALSE, 
                             maxsurrogate = maxsurrogate,
                             minsplit=minsplit, minbucket=minbucket,
                             nresample = nresample)
  lista<-list(controls,fcontrols, pval, mincriterion)
  return(lista)
}