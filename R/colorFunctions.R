#' @importFrom circlize colorRamp2

customColors_counts<-function(){
  
  customPalette<-customPaletteBalanced()
  
  #per conte normalizzate
  xx<-log2(c(50,100,250,500,1000,2000,5000,10000,15000))
 
  #print("soglie di ripartizione:")
  #print(xx)
  col_fun <- colorRamp2(xx, customPalette)
  return(col_fun)

}


customColors_ZscoreTransformed<-function(){
  
  customPalette<-customPaletteBalanced()
  
  xx<-c(-2.5,-1.5,-1,-0.5,0,0.5,1,1.5,2.5)
  #print("soglie di ripartizione:")
  #print(xx)
  col_fun <- colorRamp2(xx,customPalette)
  
}


deciliColors<-function(M){
  customPalette<-customPaletteBalanced()
  
  xx<-quantile(M, probs = seq(.1, .9, by = .1))
  #print("decili:")
  #print(xx)
  col_fun <- colorRamp2(xx,customPalette)
  return(col_fun)
  
}



customColors_FC<-function(){
  
  customPalette<-customPaletteUnbalanced()
  
  xx<-c(0.45,0.6,0.75,1,1.25,1.5,2,2.75,3.5)
  #print("soglie di ripartizione:")
  #print(xx)
  col_fun <- colorRamp2(xx,customPalette)
  
}
