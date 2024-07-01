settingsXheatmaps<-function( numberOfFeatures) {
  
  
  w<-7000
  h<-5000 #5000
  res<-1000
  f_row<-6#5.5
  f_col<-7
  ft_col<-10
  ft_row<-10
  fl<- 6
  
  if (numberOfFeatures>50){
    ############# SETTINGS: 50 GENES ######################################
    w<-7000
    h<-7000 #5000
    res<-1000
    f_row<-5.5
    f_col<-7
    ft_col<-10
    ft_row<-10
    fl<- 6
    
  }
  
  if (numberOfFeatures>100){
    ############# SETTINGS: 100 GENES ######################################
    w<-7000
    h<-10000 #5000
    res<-1000
    f_row<-3.5
    f_col<-7
    ft_col<-10
    ft_row<-10
    fl<- 6
  }
  if (numberOfFeatures>200){

    w<-7000
    h<-10000 #5000
    res<-2000
    f_row<-1 
    f_col<-3
    ft_col<-7
    ft_row<-7
    fl<- 6
  }
  if (numberOfFeatures>400){
    
    w<-7000
    h<-15000 #5000
    res<-2000
    f_row<-1 
    f_col<-4
    ft_col<-7
    ft_row<-7
    fl<- 5
  }
  if (numberOfFeatures>750){
    
    w<-10000
    h<-20000 #5000
    res<-2000
    f_row<-1 
    f_col<-5
    ft_col<-6
    ft_row<-6
    fl<- 5
  }
  if (numberOfFeatures>1000){
    w<-10000
    h<-25000 #5000
    res<-2000
    f_row<-1 
    f_col<-5
    ft_col<-5
    ft_row<-5
    fl<- 4
  }
  if (numberOfFeatures>1250){
    
    w<-10000
    h<-25000 #5000
    res<-2000
    f_row<-0.5
    f_col<-5
    ft_col<-5
    ft_row<-5
    fl<- 1
  }
  
  #metrics<-c("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski", "pearson", "spearman", "kendall")
  metric<-"manhattan"
  clusteringMethod<-"complete"
  lista<-list(w,h,res, f_row,f_col,ft_col,ft_row,fl,metric,clusteringMethod)
  return(lista)
}