readSettings<-function(filePath) {
  
  iperparameters<-read.table(file=filePath, header = TRUE, sep = "\t")
  iperparameters<-data.table(iperparameters)
  padj_max<-iperparameters[iperparameters$parameter=="padj_max",]$value
  if (padj_max!="calculated"){
    padj_max<-as.numeric(padj_max)
  } 
  FC_min<-iperparameters[iperparameters$parameter=="FC_min",]$value
  if (FC_min!="calculated" ){
    FC_min<-as.numeric(FC_min)
  } 
  flag_shrinkage<-iperparameters[iperparameters$parameter=="shrinkage",]$value
  lista<-list(padj_max,FC_min,flag_shrinkage)
  return(lista)
}
