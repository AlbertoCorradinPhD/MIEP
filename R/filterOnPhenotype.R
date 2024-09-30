
filterOnPhenotype<-function(originalCounts, coldata, cellLine, condition, threshold){
  
  lista<-setDataForTest(originalCounts, coldata, flag_cellLine=cellLine, 
                              conditions=condition)
  rawCounts<-lista[[1]]
  coldata<-lista[[2]]    
  sub_dds <- DESeqDataSetFromMatrix(countData = rawCounts,
                                     colData = coldata,
                                     design = ~ 1) #usa 1 perché ho solo un fenotipo
  #selection
  df<-counts(sub_dds)
  M<-dim(df)[2]
  boolean <- df>threshold #confronto per ogni casella
  v_sum<-apply(boolean, MARGIN=1,FUN=sum) #somma per righe
  if (M>=2) {
    keep<- v_sum >= max(2,ceiling(M/2)) # maggiore della metà delle colonne, ma minimo due per precauzione
  } else { #caso particolare
    keep<- v_sum >= 1 
  }
  return(keep)
} 