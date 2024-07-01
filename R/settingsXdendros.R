settingsXdendros<-function( numberOfFeatures) {
  #############  SETTINGS: 100 GENES ######################################
  w<-7000
  h<-5000
  res<-500
  cex<-0.75
  f_row<-1.5
  
  if (numberOfFeatures>100){
    
    #############  SETTINGS: 150 GENES ######################################
    w<-7000
    h<-7000 #5000
    res<-500
    cex<-0.5
    f_row<-1.25
  }
  
  if (numberOfFeatures>200){
    
    #############  SETTINGS: 250 GENES ######################################
    w<-7000
    h<-10000 #5000
    res<-500
    cex<-0.35
    f_row<-1.25
  }
  if (numberOfFeatures>350){
    
    #############  SETTINGS: 350 GENES ######################################
    w<-7000
    h<-10000 #5000
    res<-500
    cex<-0.3
    f_row<-1.15
  }
  if (numberOfFeatures>500){
    
    #############  SETTINGS: 500 GENES ######################################
    w<-7000
    h<-15000 #5000
    res<-500
    cex<-0.25
    f_row<-1
  }
  if (numberOfFeatures>750){
    
    #############  SETTINGS: 750 GENES ######################################
    w<-10000
    h<-20000 #5000
    res<-500
    cex<-0.15
    f_row<-1 
  }
  if (numberOfFeatures>1000){
    
    #############  SETTINGS: 1000 GENES ######################################
    w<-10000
    h<-25000 #5000
    res<-500
    cex<-0.15
    f_row<-1
    }
  if (numberOfFeatures>1250){
    
    #############  SETTINGS: 1500 GENES ######################################
    w<-10000
    h<-25000 #5000
    res<-500
    cex<-0.075
    f_row<-0.35
  }
  
  #metrics<-c("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski", "pearson", "spearman", "kendall")
  metric<-"manhattan"
  clusteringMethod<-"complete"
  lista<-list(w,h,res,cex,f_row,metric,clusteringMethod)

}
