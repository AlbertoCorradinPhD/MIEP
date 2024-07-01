coordinateNamesInData<-function(rawCounts, coldata){
  
  #clean names and rearrange
  #colnames(rawCounts) <- sub("_S\\d+", "", colnames(rawCounts)) #specifica di un dataset
  check<- all(rownames(coldata) %in% colnames(rawCounts))
  if (!check){
    print("erroneous inputs")
    exit()
  } 

  #verica della corrispondenza dei nomi tra coldata e raw_counts
  rawCounts <- rawCounts[, rownames(coldata)] # reorder
  check<-all(rownames(coldata) == colnames(rawCounts))
  if (!check){
    print("erroneous inputs")
    exit()
  } else print("All right! Sample names in coldata file correspond to column names in counts file")
  
  lista<-list(rawCounts,coldata)
  return (lista)
}  