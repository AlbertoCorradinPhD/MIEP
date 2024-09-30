setDataForTest<-function(originalCounts,coldata, 
                        flag_cellLine,conditions){
  
  #importa i file e seleziona i dati d'interesse
  lista<-selectDataOfInterest(originalCounts, coldata, flag_cellLine, conditions)
  roundedCounts<-lista[[1]]
  coldata<-lista[[2]]
  rm(lista)
  
  #regular expression nel caso di difformità nei nomi tra coldata e raw_counts
  lista<-coordinateNamesInData(rawCounts=roundedCounts, coldata)
  rawCounts<-lista[[1]]
  coldata<-lista[[2]]
  rm(lista)

  lista<-list(rawCounts,coldata)
  return(lista)
}