collapsing<-function(X){
  #individua doppioni nel dataset e calcola mediana
  gene_symbols<-rownames(X)
  pattern<-"(_seq[1-9])"
  gene_symbols<-str_replace_all(gene_symbols,pattern , "")
  set<-unique(gene_symbols)
  #check
  #print("vi sono delle ripezioni?")
  boolean<-length(set)<dim(X)[1]
  #print(boolean)
  
  if (boolean){
    #print("nel dataset alcune features sono ripetute. Procedo al 'collapsing' e a sostituirle con un valore mediano")
    #print("number of levels:")
    L<-length(set)
    #print(L)
    dataShrinked<-matrix(data = NA, ncol = dim(X)[2])
    #NB: viene creata una prima riga fatta di soli NA
    colnames(dataShrinked)<-colnames(X)
    for (i in 1:L) {
      gene<-set[i]
      indexes<-which(gene_symbols==gene, arr.ind=TRUE)
      Li<-length(indexes)
      if (Li>1) {
        #print("feature non unica:")
        #print(gene)
        temp<-apply(X=X[indexes,], MARGIN=2, FUN=median) #mediana 
        #print("Median of normalized counts:")
        #print(temp)
      } else {temp<-X[indexes,]}
      dataShrinked<-rbind(dataShrinked,temp)
      row.names(dataShrinked)[dim(dataShrinked)[1]]<-gene
    }
    #check
    #print("la dimensione torna?")
    #tolgo prima riga fatta di soli NA
    dataShrinked<-dataShrinked[2:dim(dataShrinked)[1],]
    #print((dim(dataShrinked)[1])==L)
    X<-dataShrinked
    
  }
  return(X)
  
}
