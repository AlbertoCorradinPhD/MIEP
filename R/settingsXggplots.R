settingsXggplots<-function(coldata,cellLines) {
  
  Labels<-unique(coldata$condition)
  colors<-rainbow(length(Labels))
  pointsizes<-c(3,7,5,9,1) #lista custom
  shapes<-c(19,17,15,3,7,8) #lista corripondente alla sequenza usata da ggplot
  w<-6000
  h<-4000
  res<-500
  legenda<-c("Condition", "Cell Line")
  
  #COLORS FOR CONDITION
  #passo iniziale
  colori<-rep(colors[1],length(coldata$condition))
  names(colori)<-coldata$condition
  #passo iterativo
  for (j in 2:length(Labels)) {#ciclo colori
    boolean<-names(colori)==Labels[j]
    colori[boolean]<-colors[j]
  }#chiuse ciclo
  coloriAsFactor<-factor(colori, colors[1:length(Labels)])
  
  #SHAPE FOR CELL LINE (for the customized sample plots)
  #passo iniziale
  shape<-rep( shapes[1],length(coldata$cellLine))
  names(shape)<-coldata$cellLine
  #passo iterativo
  for (j in 2:length(cellLines)) {#ciclo colori
    boolean<-names(shape)==cellLines[j]
    shape[boolean]<-shapes[j]
  }#chiuse ciclo
  shapeAsFactor<-factor(shape, shapes[1:length(cellLines)])
  
  #POINT SIZE FOR CELL LINE (for factoextra biplots)
  #passo iniziale
  pointsize<-rep(pointsizes[1],length(coldata$cellLine))
  names(pointsize)<-coldata$cellLine
  #passo iterativo
  for (j in 2:length(cellLines)) {#ciclo colori
    boolean<-names(pointsize) %in% cellLines[j]
    pointsize[boolean]<-pointsizes[j]
  }#chiude ciclo 
  pointsizeAsFactor<-factor(pointsize, levels=pointsizes[1:length(cellLines)])
  
  lista<-list(coloriAsFactor, pointsizeAsFactor, shapeAsFactor,w,h,res, legenda)
  return(lista)
  
}
