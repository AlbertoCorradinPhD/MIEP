generaVolcanoPlots<-function( geneList, yLimit, xLimit, cellLines, confronti,
                               inDir,outDir,DESeqDir){


  #read counts
  filePath<-file.path(inDir,"normalizedCounts_withSymbol.tsv")
  exprData<-read.table(file=filePath, header = TRUE, sep = "\t")
  #names(exprData)
  
  #settings
  w<-5000
  h<-5000
  r<-500
  filePath<-file.path(DESeqDir,"DESeqSettings.tsv")
  lista<-readSettings(filePath)
  padj_max_fromSettings<-lista[[1]]
  FC_min_fromSettings<-lista[[2]]
  shrinkage_fromSettings<-lista[[3]]
  rm(lista)
  
  outDir<-file.path(outDir,"volcanoPlots")
  dir.create(outDir, showWarnings = FALSE, recursive = TRUE)
  

  
  ###  CREATE TEST TABLE SEQUENCES OF GENELIST
  newTable<-exprData[,c(1,2)]
  griglia<- exprData$ID_REF %in% geneList  #booleano che funge da griglia della genelist
  print(paste("number of considered genes:", sum(griglia)))
  newTable<-newTable[griglia,]
  tableFConly<-data.frame(ID_REF=newTable$ID_REF)
  nomiColonne<-"ID_REF"
  tableDEonly<-data.frame(ID_REF=newTable$ID_REF)
  
 
  for (l in 1:length(cellLines)){# apri loop cellLines
    linea<-cellLines[l]
    for (c in 1:length(confronti)){ # apri loop confronti
      confronto<-confronti[c]

      #raccogli i dati
      newdata<- generaTestTablePerGeneset(inDir=DESeqDir, linea, confronto, 
                                          myTable=newTable )
      #genera tabella, formato pdf
      suppressWarnings(genesetFormattedTable(linea,confronto,outDir,newdata))
      
      #aggiorna tableFConly
      titolo<-paste(linea,"-",confronto, sep="")
      nomiColonne<-c(nomiColonne, titolo)
      tableFConly<-cbind(tableFConly,newdata$log2FoldChange)
      #aggiorna tableDEonly
      indexes<-which(is.na(newdata$`log2FoldChange if DE`), arr.ind = TRUE, useNames = FALSE)
      v<-rep(1,dim(tableDEonly)[1])
      v[indexes]<-0
      tableDEonly<-cbind(tableDEonly,v)

      #retrieve DESeq setting
      lista<-readThresholdsForDEs(padj_max=padj_max_fromSettings,
                                  FC_min=FC_min_fromSettings, 
                                  shrinkage=shrinkage_fromSettings, 
                                  DESeqDir, linea=linea, confronto=confronto)
      padj_max<-lista[[1]]
      FC_min<-lista[[2]]
      rm(lista)
      
      filename<-paste(linea,"-",confronto,".tiff",sep="")
      filePath<-file.path(outDir,filename)
      main<- parseName(paste(confronto,"in", linea))
      volcanoPlot(filePath, main, data=newdata, FC_min, padj_max,w,h,r,
                  boundaries=FALSE, yLimit, xLimit, gene_names=TRUE
                  )
    }# chiude for confronti
  }# chiude for cellLines  
  
  #save tables
  names(tableFConly)<-nomiColonne
  names(tableDEonly)<-nomiColonne
  lista<-list(tableFConly=tableFConly, tableDEonly=tableDEonly,nomiColonne=nomiColonne )
  filePath<-file.path(outDir,"tables.rds")
  saveRDS(lista, file=filePath)
  
  return(outDir)
}