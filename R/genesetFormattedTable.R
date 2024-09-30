genesetFormattedTable<-function(linea,confronto,outDir,newdata){
  
  
  filename<-paste("FoldChanges&padjs_",linea,"-",confronto,".html",sep="")
  filePath<-file.path(outDir,filename)
  formatted<-sapply(X=newdata[,-c(1,2)], FUN=formatC, format = "e", digits = 2) 
  newdata<-cbind(data.frame(ID_REF=newdata$ID_REF,ensembleID=newdata$ensembleID),
                 formatted)
  
  # sostituisco NAs con frase
  indexes<-which(newdata==" NA", arr.ind = TRUE, useNames = FALSE)
  newdata<-replace(x=newdata, list=indexes, 
                   values="Not DE for this test")
  tryCatch(
    kbl(x=newdata, format="html", row.names = FALSE,
        #col.names = TRUE, 
        align='c', caption =paste(linea,"-",confronto),
        label = NULL, escape = TRUE, booktabs = TRUE,
        longtable = TRUE, position = "t",
        centering = TRUE
    ) %>% kable_styling(font_size = 12) %>% save_kable(filePath),
    error = function(err){
      filename<-paste("FoldChanges&padjs_",linea,"-",confronto,".tsv",sep="")
      filePath<-file.path(outDir,filename)
      write.table(newdata, file=filePath, sep="\t", row.names=FALSE, quote=FALSE)
    })#chiudo tryCatch
  
}
