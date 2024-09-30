

thresholdsForDEs<-function(resultsObj,filename1, filename2,
                           flag_PADJ,flag_FC,padj_max,FC_min){

  
  #calc padj_max
  if (padj_max=="calculated" ) {
    flag_PADJ<-TRUE
    ### set padj_max to 1st quantile of the distribution
    frase<-paste("padj_max:")
    write(frase, file = filename1,
          ncolumns = 1, append = FALSE,sep="\n")
    padj_max<-calcPadj_max(resultsObj, filePath=filename1)
  }
  
  #calc FC_min
  if (FC_min=="calculated" ) {
    flag_FC<-TRUE
    frase<-paste("minimal (linear) fold change:")
    write(frase, file = filename2,
          ncolumns = 1, append = FALSE,sep="\n")
    FC_min<-calcFC_min(resultsObj,filePath=filename2)
    }
  lista<-list(padj_max,FC_min,flag_PADJ,flag_FC)
  return(lista)
}