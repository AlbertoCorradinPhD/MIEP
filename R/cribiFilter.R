cribiFilter<-function(originalCounts, coldata, DeSeqObj,cF_threshold, DESeqDir){
  
  #str(DeSeqObj)
  df<-data.frame(dummy=rep(0,dim(DeSeqObj)[1])) 
  k<-2 #per posizionare nomi colonne
  filePath<-file.path(DESeqDir,"detailsOf_filterOnPhenotype.txt")
  write(x="number of kept sequences for paired 'cell line-condition':", 
        file = filePath, ncolumns = 1, append = FALSE, sep = "\n") 
  
  ### FILTRAGGIO PER OGNI FENOTIPO
  for (i in 1:length(levels(DeSeqObj$cellLine))){ #links a DeSeqObj@colData@listData$condition
    cellLine<-levels(DeSeqObj$cellLine)[i]
    for  (j in 1:length(levels(DeSeqObj$condition))){
      condition<-levels(DeSeqObj$condition)[j]
      keep<-filterOnPhenotype(originalCounts, coldata, cellLine, condition,
                              threshold=cF_threshold)
      print(paste(cellLine,condition,sep="-"))
      print("number of kept sequences:")
      print(sum(keep))
      frase<-paste(cellLine,"_",condition,": ",sum(keep),sep="")
      write(x= frase, file = filePath, ncolumns = 1, append = TRUE, sep = "\n")
      df<-cbind(df,as.data.frame(keep))
      #str(df)
      names(df)[k]<-paste(cellLine,condition,sep="-")
      k<-k+1
    }#chiude first loop
  }#chiude second loop
  df<-df[,-1] #elimino colonna dummy
  
  ### FILTRAGGIO COMPLESSIVO CRIBI
  #pretendo che la sequenza sia presente per tutti i fenotipi
  keep<-apply(df,MARGIN=1,FUN=all)
  #str(keep)
  
  ### SAVE RESULTS ON THE DESEQ OBJECT
  nomeFiltro<-"filterOnPhenotype"
  DeSeqObj<-saveResultsOfFilter(DESeqDir,DeSeqObj,nomeFiltro,keep, df)
  return(DeSeqObj)
}