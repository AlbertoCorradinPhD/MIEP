GOterms_retrieval<-function(exprSet){
  
  
  flag<-FALSE
  repeat {
    tryCatch({
      mart<-creaConnessioneEnsemble()
      #generatore d'errore se oggetto mart e' nullo
      temp <- getBM(attributes= c(annotation(exprSet), "go_id",#"goslim_goa_accession",
                   "goslim_goa_description","namespace_1003","name_1006"),
                   filters = annotation(exprSet), 
                   values = featureNames(exprSet)[1:2], mart=mart)
      flag<-TRUE
      if (flag){ break }
    },
    error = function(err){
      print("problems in downloading from ensemble database")
    })# chiude tryCatch
  }#chiude repeat
  

  #SUDDIVIDI IN PICCOLI BATCH (di 100 termini ciascuno, vedi step)
  # inserisco pausa tra le query per evitare che siano scambiate per attacco hacker 
  
  N<-length(featureNames(exprSet))
  step<-100

  if (N>step){
    repeat {
      try({
        sequenza<-c(seq(1,N, by=step),N)
        N_slices<-length(sequenza)-1
        
        #initial step
        print(paste(1,"out of",N_slices,sep=" "))
        GO_tbl  <- getBM( mart=mart, filters = annotation(exprSet),
          attributes= c(annotation(exprSet), "go_id",#"goslim_goa_accession",
                        "goslim_goa_description","namespace_1003","name_1006"),
          values = featureNames(exprSet)[1:sequenza[2]], uniqueRows=TRUE
          )
        #step iterativo
        for (i in 2:N_slices){
          s<-2
          print(paste("I will sleep for",s,"seconds. Then I will query again"))
          Sys.sleep(s)
          print(paste(i,"out of",N_slices,sep=" "))
          temp  <- getBM( mart=mart, filters = annotation(exprSet),
            attributes= c(annotation(exprSet), "go_id",#"goslim_goa_accession",
                    "goslim_goa_description","namespace_1003","name_1006"),
            values = featureNames(exprSet)[(sequenza[i]+1):sequenza[i+1]], 
            uniqueRows=TRUE
          )
          GO_tbl<-rbind(GO_tbl,temp)
        }#chiude for 
        print("data collection complete at time:")
        print(Sys.time())
      })# chiude try
      
      #esco dal repeat
      if ((i+1)>N_slices) break
    }#chiude repeat loop
  } else { 
    GO_tbl  <- getBM( mart=mart, filters = annotation(exprSet),
               attributes= c(annotation(exprSet), "go_id",#"goslim_goa_accession",
                "goslim_goa_description","namespace_1003","name_1006"), 
                values=c(featureNames(exprSet)),uniqueRows=TRUE
                )
  } #chiude else
  
  return(GO_tbl)

}
