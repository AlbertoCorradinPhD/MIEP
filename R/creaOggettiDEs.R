creaOggettiDEs<-function(inDir,cellLines, confronti){
  ##############################################################################
  #### CREO OGGETTI CONTENENTE TUTTI I DEs SEPARATI PER CONFRONTO
  ############################################################################## 
  DEids<-list()
  DEseqs<-list()
  for(j in 1: length(confronti)){
    confronto<-confronti[j]
    #print(paste("confronto:",confronto))
    DEseqs[[j]]<-list()
    DEids[[j]]<-list()
    #check
    #print(attributes( DEseqs[[j]]) ) #NULL
    
    for(k in 1: length(cellLines)){ 
      linea<-cellLines[k]
      file<-paste(confronto,"-",linea,".tsv",sep="")
      filename<-file.path(inDir,file)
      table<-read.table( filename, header=TRUE, skip=0, sep="\t")  
      input<-table$ID_REF
      #check
      #L<-length(input)
      #print(paste("number of items: ",L))
      DEids[[j]][[k]]<-input
      input<-table$ensembleID
      DEseqs[[j]][[k]]<-input
    }#chiudi ciclo for
    attr(DEids[[j]], "names")<- cellLines
    #print(attributes( DEids[[j]]) )
    attr(DEseqs[[j]], "names")<- cellLines
    #print(attributes( DEseqs[[j]]) )
    
  }  #chiude for per confronti
  attr(DEids, "names")<- confronti
  #print(attributes( DEids) )
  attr(DEseqs, "names")<- confronti
  #print(attributes( DEseqs) )
  lista<-list(DEids,DEseqs)
  return(lista)
}