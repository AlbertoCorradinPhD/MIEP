printBestSettings<-function(bestAccuracy,bestPval, bestTesttype,
                            bestTeststat, bestNtrees,bestConfMatrix,
                            outDir){
  
  
  #print best
  filePath<-file.path(outDir,"bestSettings.txt")
  
  frase<-paste("best accuracy:",bestAccuracy)
  print(frase)
  write(x=frase, file=filePath, append = FALSE, sep = "\t")
  frase<-paste("best pval:",bestPval)
  print(frase)
  write(x=frase, file=filePath,append = TRUE, sep = "\t")
  frase<-paste("best testtype:",bestTesttype)
  print(frase)
  write(x=frase, file=filePath, append = TRUE, sep = "\t")
  frase<-paste("best teststat:",bestTeststat)
  print(frase)
  write(x=frase, file=filePath, append = TRUE, sep = "\t")
  frase<-paste("best number of trees in the forest:",bestNtrees)
  print(frase)
  write(x=frase, file=filePath, append = TRUE, sep = "\t")
  
  if (!is.null(bestConfMatrix)){
    print("=====================================")
    print("confusion matrix of best random forest:")
    print(bestConfMatrix)
    table<-as.table(bestConfMatrix)
    titolo<-"confusion matrix for best forest"
    filePath<-file.path(outDir,"confMatrix_bestForest.html")
    printTableHTML(data=table,titolo=titolo,filePath=filePath, flag=TRUE)
    cm<-bestConfMatrix
    tocsv <- data.frame(t(cm$overall))
    filePath<-file.path(outDir,"overallPerformance_bestForest.csv")
    write.csv(tocsv,file=filePath,row.names = FALSE)
    tocsv <- data.frame(t(cm$byClass))
    filePath<-file.path(outDir,"byClassPerformance_bestForest.csv")
    write.csv(tocsv,file=filePath,row.names = TRUE)
    rm(cm)
  }
}