#' @importFrom viridis scale_fill_viridis

selectTopFeatures<-function(data,outDir, Nmax=50) {
  
  #### BOXPLOT
  #to identify positive outliers
  bp<-boxplot(x=data$importance)
  #summary(bp$stats)
  boolean<-bp$out >0 #solo outlier con valori positivi
  #caso particolare: se non ho outliers, prendi i primi 10
  Ntop<-ifelse(length(bp$out[boolean])>10,length(bp$out[boolean]),10) 
  if (Ntop>Nmax)  {Ntop<-Nmax}
  
  #### BARPLOT
  top_importance<-data[1:Ntop,]
  #str(top_importance)
  top_importance$feature<-factor(top_importance$feature,levels=rev(top_importance$feature))
  
  p<-ggplot(top_importance, aes(.data$feature, .data$importance)) + 
    geom_bar( aes(fill=.data$feature), stat="identity", width=.8, 
              position = position_dodge(width = .25))+ 
    coord_flip(clip = "off")+viridis::scale_fill_viridis(guide=NULL,discrete=TRUE)+
    theme(axis.text=element_text(size=10))
  
  lista<-list(Ntop,p)
  return(lista)
  
}