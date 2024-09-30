
iperparametersReadDefault<-function(filePath){  
  
  ### READ IPERPARAMETERS FROM FILE
  #da pacchetto installato
  tryCatch( {
     iperparameters<-as.data.table(iperparameters)
      print("Default DESeq2's settings:")
      print(iperparameters)
      },
     error = function(err){
        print("not able to load default iperparameters")
   }) #chiude tryCatch
        
  #proseguo
  names(iperparameters)
  columnsOfInterest<-c("parameter","value")
  iperparameters<-subset(iperparameters,select=columnsOfInterest)
 
  ### CASTING
  #NB: sono tutti letti come caratteri
  flag_MFtest<-as.logical(
    iperparameters[iperparameters$parameter=="flag_MFTest",]$value)
  flag_shrinkage<-iperparameters[iperparameters$parameter=="flag_shrinkage",]$value
  padj_max<-iperparameters[iperparameters$parameter=="padj_max",]$value
  if (padj_max!="calculated"){
    padj_max<-as.numeric(padj_max)
  } 
  FC_min<-iperparameters[iperparameters$parameter=="FC_min",]$value
  if (FC_min!="calculated" ){
    FC_min<-as.numeric(FC_min)
  }
  
 
  bF_threshold<-as.numeric(
    iperparameters[iperparameters$parameter=="bF_threshold",]$value)
  cF_threshold<-as.numeric(
    iperparameters[iperparameters$parameter=="cF_threshold",]$value)
  flag_regionReport<-as.logical(
    iperparameters[iperparameters$parameter=="flag_regionReport",]$value)
  flag_reportingTools<-as.logical(
    iperparameters[iperparameters$parameter=="flag_reportingTools",]$value)
  flag_cellLine<-iperparameters[iperparameters$parameter=="flag_cellLine",]$value
  
  lista<-list(flag_MFtest, flag_shrinkage, padj_max, FC_min,  
              bF_threshold, cF_threshold, flag_regionReport,flag_reportingTools, 
              flag_cellLine)
  return(lista)
  
  }
