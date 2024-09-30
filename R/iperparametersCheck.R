iperparametersCheck<-function(flag_MFtest, flag_shrinkage, padj_max, FC_min,
                              bF_threshold, cF_threshold, flag_regionReport,
                               flag_reportingTools){
                               
  
  
  #check input iperparameters
  boolean<-any(length(flag_MFtest)==0, length(flag_shrinkage)==0, 
               length(padj_max)==0, length(FC_min)==0,  
                length(bF_threshold)==0, length(cF_threshold)==0,
                length(flag_regionReport)==0,length(flag_reportingTools)==0
               ) 
  if (boolean){
    print("erroneous parameters")
    exit()
  } else print("parameters are not empty: then proceed")
}