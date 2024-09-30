printTestResults<-function(results,sampleGOdata,GO2gene, outDir) {
  
  resultFisher <-results[[1]]
  resultKS<-results[[2]]
  resultKS.elim<-results[[3]]
  
  pvalFis<-score(resultFisher)
  pvalKS<-score(resultKS )
  pvalKS.elim<-score(resultKS.elim )
  
  #### STATISTICHE DI PRESENZA GO TERMS 
  df<-termStat(sampleGOdata)
  df<-cbind(row.names(df),df)
  names(df)[1]<-"GO term"
  
  filePath<-file.path(outDir,"termStats.html")
  printTableHTML(data=df,titolo="Statistics",filePath)
 
  ### INFO SU UTILIZZO DEI TERMINI DI GENE ONTOLOGY
  N<-length(usedGO(sampleGOdata))
  print(paste("GO terms used to compose the topGO object:",N))
  L<-length(names(GO2gene))
  print(paste("GO terms in the GO2genes file:",L))
  print(paste("number of unused GO terms:",L-N))
  
  ########################################################################
  ###  RACCOGLIERE RISULTATI DEI TEST 
  #######################################################################
  
  #raccoglie i risultati in un unico oggetto
  testsResults <- GenTable(sampleGOdata, Fisher = resultFisher, 
                           KS = resultKS, KS.elim = resultKS.elim,
                           orderBy = "Fisher", ranksOf = "Fisher",
                           topNodes = 100
  )
  names(testsResults)
  testsResults<-data.table(testsResults)
  testsResults<- testsResults[order( testsResults$Significant,
                                     decreasing = TRUE),] 
  
  filePath<-file.path(outDir,"testsResults.html")
  printTableHTML(data=testsResults,titolo="Results of Tests",filePath)
  
}
