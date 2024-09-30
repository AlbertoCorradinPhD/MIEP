checkDuplicates<-function(nomiFeatures, trasposta){
  
  ### VERIFICO PRESENZA DI DUPLICATI
  colonneDescrittive<-c("labels")
  gene_symbols<-nomiFeatures
  set<-unique(gene_symbols)
  #check
  #print("vi sono delle ripezioni?")
  boolean<-length(set)<dim(trasposta)[2]-length(colonneDescrittive) 
  #print(boolean)
  
  if (boolean){
    #print("number of levels:")
    L<-length(set)
    #print(L)
    for (i in 1:L) {
      gene<-set[i]
      indexes<-which(gene_symbols==gene, arr.ind=TRUE)
      M<-length(indexes)
      if (M>1){
        for (j in 2:M){
          indice<-indexes[j]
          nome<-paste(gene,"_seq",j,sep="")
          names(trasposta)[indice]<-nome
          nomiFeatures[indice]<-nome
        }#chiude for
        #check
        #print(names(trasposta)[indexes])
      }#chude if
      
    }#chiude for
  }#chiude if boolean

}
