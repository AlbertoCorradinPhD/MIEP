selectDataOfInterest<- function(originalCounts, coldata, flag_cellLine, conditions) {
  
  
  #NB: attento che la colonna da cui row.names abbia un nome accettabile 
  roundedCounts<-round_df(originalCounts)
  roundedCounts <- as.matrix(roundedCounts)
  
  #set coldata
  coldata <- coldata[,c("condition","cellLine")]
  coldata$condition <- factor(coldata$condition)
  coldata$cellLine <- factor(coldata$cellLine)
  
  #subset dei dati + check
  #subset dei dati da sottoporre a test statistico
  coldata<-choose_cellLine(coldata, flag_cellLine)
  coldata<-choose_conditions(coldata, conditions)
  
  lista<-list(roundedCounts,coldata)
  return(lista)
}
