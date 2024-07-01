printTableHTML<-function(data,titolo,filePath, flag=FALSE){
  
  tryCatch({
    kbl(x=data, format="html", row.names =flag,
      #col.names = TRUE, 
      align='c', caption =titolo,
      label = NULL, escape = TRUE, booktabs = TRUE,
      longtable = TRUE, position = "t",
      centering = TRUE
  ) %>% kable_styling(font_size = 12) %>% save_kable(filePath)},
    error = function(err){
       write.table(data, file=filePath, sep="\t", row.names=FALSE, quote=FALSE)
    })#chiudo tryCatch
  
}