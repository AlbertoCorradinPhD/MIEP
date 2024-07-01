
releaseResults<-function(GOobject,resultsTogether,outDir, ntop, 
                      titleForGmtFile,resDir){
  

  #settings
  w<-5000
  h<-5000
  r<-500
  
  for (t in 1:length(resultsTogether)){
    try( {
    resultsOfTest<-resultsTogether[[t]]
    resName<-attributes(resultsTogether)$names[t]
    foldername<-paste(resName,"_test",sep="")
    testDir<-file.path(outDir, foldername)
    dir.create(testDir, showWarnings = FALSE, recursive = TRUE)
    
    GOenrichmentTestTable<-rankingGOterms(sampleGOdata= GOobject,resultsOfTest, ntop, 
                                          resName,outDir=testDir, w,h,r)
    
    ### PLOT SUBGRAPHS OF MOST SIGNIFICANT NODES
    wantedNodes<-GOenrichmentTestTable$GO.ID
    wantedTerms<-GOenrichmentTestTable$Term
    
    exploringGraph(wantedNodes,sampleGOdata=GOobject,resDir,
                   titleForGmtFile, wantedTerms,outDir=testDir,resName)
    
		p<-nodesGraph(sampleGOdata=GOobject,result=resultsOfTest,ntop,
                  wantedNodes, outDir=testDir, w,h,r)
    
    })#chiude try
  } #chiude ciclo tests


}#chiude funzione
