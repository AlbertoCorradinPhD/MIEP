dataPreparation<-function(dataML, outDir, 
                          pwd, metodoDiClassificazione, classi){
  
 
  ## inserisco nel database le classi per la ripartizione dell'albero
  classi<-as.factor(classi)
  X<-cbind(as.data.frame(classi), dataML[,-1])
  
  foldername<-paste("by",metodoDiClassificazione,sep="")
  outDir<-file.path(outDir,foldername)
  dir.create(outDir, showWarnings = FALSE, recursive = TRUE)
  #change plot settings specifically
  w<-min(5000*length(unique(classi)), 30000)
  
  
  Ncolori<-length(unique(classi))
  colori<-colorPalette()[1:Ncolori]
  
  if (is.null(pwd)){
    pwd<-askSudoPwd()
  } 
  
  lista<-list(X,metodoDiClassificazione,outDir,w, colori, pwd)
  return(lista)
}