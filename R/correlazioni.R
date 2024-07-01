correlazioni<-function(results,sampleGOdata,outDir) {
  
  resultFisher <-results[[1]]
  resultKS <-results[[2]]
  resultKS.elim <-results[[3]]
  
  pvalFis<-score(resultFisher)
  pvalKS<-score(resultKS )
  pvalKS.elim<-score(resultKS.elim )
  
  #settings fro plots
  w<-2000
  h<-1000
  r<-100
  
  ### CALCOLO CORRELAZIONE TRA ALGORITMI DIVERSI
  pvalKS.elim_ordered<- score(resultKS.elim, whichGO = names(pvalKS )) #li vuole nello stesso ordine ddei precedenti
  corr_algs<-cor(pvalKS , pvalKS.elim_ordered) 
  print(paste("KS-KS.elim correlation:",corr_algs))
  
  #PLOTS
  gstat <- termStat(sampleGOdata, names(pvalKS)) #accede all'oggetto sampleGOdata restituendo un dataframe
  gSize <- gstat$Annotated / max(gstat$Annotated) * 4 #cexdel pch
  gCol <- colMap(gstat$Significant) #colore del pch
  filename<-file.path(outDir,"KSclassic_vs_KSelim.jpeg")
  jpeg(filename, width=w, height=h, res=r)
  par(mfcol = c(1, 2), cex = 1)
  plot(pvalKS, pvalKS.elim_ordered, xlab = "p-value KS classic", ylab = "p-value KS elim",
       pch = 19, cex = gSize, col = gCol)
  
  plot(-log10(pvalKS), -log10(pvalKS.elim_ordered), xlab = "-log10(p-value) KS classic", ylab = "-log10(p-value) KS elim",
       pch = 19, cex = gSize, col = gCol)
  dev.off()
  
  
  
  ### CALCOLO CORRELAZIONE TRA TEST DIVERSI 
  pvalKS_ordered<- score(resultKS, whichGO = names(pvalFis )) #li vuole nello stesso ordine dei precedenti
  corr_tests<-cor(pvalFis,pvalKS_ordered) 
  print(paste("Fisher-KS correlation:",corr_tests))
  
  #NB: non c'è correlazione tra i risultati di algoritmi diversi, ma non tra test diversi
  
  # PLOT
  gstat <- termStat(sampleGOdata, names(pvalFis )) #accede all'oggetto sampleGOdata restituendo un dataframe
  gSize <- gstat$Annotated / max(gstat$Annotated) * 4 #cexdel pch
  gCol <- colMap(gstat$Significant) #colore del pch
  filename<-file.path(outDir,"Fischer_vs_KS.jpeg")
  jpeg(filename, width=w, height=h, res=r)
  par(mfcol = c(1, 2), cex = 1)
  plot(pvalFis, pvalKS_ordered, xlab = "p-value Fisher", ylab = "p-value KS",
       pch = 19, cex = gSize, col = gCol)
  
  plot(-log10(pvalFis), -log10(pvalKS_ordered), xlab = "-log10(p-value) Fisher", ylab = "-log10(p-value) KS",
       pch = 19, cex = gSize, col = gCol)
  dev.off()
  
  lista<-list(corr_algs,corr_tests)
  return(lista)
  
}
