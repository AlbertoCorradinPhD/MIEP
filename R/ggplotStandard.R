#' @import gg3D

ggplotStandard<-function(filename, dimensioni, w,h,res, coloriAsFactor,
                         shapeAsFactor, pointsizeAsFactor, legenda, data) {
  N<-dim(data)[2]
  names(data)[1:N]<-c(paste("V",1:N, sep=""))
  
  if (dimensioni==2) {
    tiff(filename, width=w, height=h,res=res)
    p<-ggplot(data,aes(x=.data$V1, y=.data$V2, 
                  colour=names(coloriAsFactor), 
                  shape=names(shapeAsFactor), 
                  size=names(pointsizeAsFactor))) + 
      theme_void()+geom_point()
    print(p)
    p<-p+ guides( colour = guide_legend(title = legenda[1]))
    print(p)
    p<-p+ guides( shape = guide_legend(title = legenda[1]))
    print(p)
    p<-p+ guides( size = guide_legend(title = legenda[2]))
    print(p)
    dev.off()
  }
  if (dimensioni==3) {
    theta<- 45
    phi<-15
    tiff(filename, width=w, height=h,res=res)
    p<-ggplot(data, 
              aes(x=.data$V1, y=.data$V2, z=.data$V3, 
                  colour=names(coloriAsFactor), 
                  shape=names(shapeAsFactor), 
                  size=names(pointsizeAsFactor))) + 
      theme_void() + 
      axes_3D(theta=theta, phi=phi)+ stat_3D(theta=theta, phi=phi)
    print(p)
    p<-p+ guides( colour = guide_legend(title = legenda[1]))
    print(p)
    p<-p+ guides( shape = guide_legend(title = legenda[1]))
    print(p)
    p<-p+ guides( size = guide_legend(title = legenda[2]))
    print(p)
    dev.off()
  } 
  return(p)
}
