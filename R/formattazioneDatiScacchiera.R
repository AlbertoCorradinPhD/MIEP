formattazioneDatiScacchiera<-function(features,database, nomiColonne,inDir,DESeqDir,confs, myPattern){
  
  #passo iniziale
  jj<-1
  confronto<-confs[jj]
  c<-1
  kk<-1
  temp<-ifelse(features %in% database[[confronto]][[nomiColonne[kk]]],1,0)
  data<-data.frame(temp)
  row.names(data)<-features
  names(data)[c]<-paste(confronto,"_",nomiColonne[kk],sep="")
  testo<-sub(pattern=paste(confronto,"_",sep=""), replacement="", x=names(data)[c])
  start<-regexpr(pattern=myPattern, text=testo)[[1]]
  if (start>2) {linea<-substr(testo, 1, start-1)
  } else  {linea<-testo}
  #print(paste("linea cellulare:",linea))
  data<-upDowns(data, counter=c,inDir,DESeqDir,confronto, linea)
  c<-c+1
  #passo iterativo
  for(jj in 1: length(confs)){
    confronto<-confs[jj]
    #print(confronto)
    for(kk in 1: length(nomiColonne)){
      temp<-ifelse(features %in% database[[confronto]][[nomiColonne[kk]]],1,0)
      data<-cbind(data,data.frame(temp))
      names(data)[c]<-paste(confronto,"_",nomiColonne[kk],sep="")
      testo<-sub(pattern=paste(confronto,"_",sep=""), replacement="", x=names(data)[c])
      #print(testo)
      start<-regexpr(pattern=myPattern, text=testo)[[1]]
      if (start>2) {linea<-substr(testo, 1, start-1)
      } else  {linea<-testo}
      #print(paste("linea cellulare:",linea))
      data<-upDowns(data, counter=c,inDir,DESeqDir,confronto, linea)
      c<-c+1
    }
  }
  data<-data[,-1]
  #test: data["XNDC1N-ZNF705EP-ALG1L9P",]
  return(data)
}