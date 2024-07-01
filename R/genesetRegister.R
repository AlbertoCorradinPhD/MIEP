genesetRegister<-function(name,resDir,contenuto){
  #scrittura su file per genelists overview
  write(x=name, file = file.path(resDir,"genesetsList.txt"),
        ncolumns = 1, append = TRUE, sep = "\n")
  #crea in gmtFolder
  filename<-paste(name,".gmt",sep="")
  foldername<-"gmtFiles"
  outDir<-file.path(resDir,foldername)
  dir.create(outDir, showWarnings = FALSE, recursive = TRUE)
  filePath<-file.path(outDir,filename)
  write(x=contenuto, file = filePath, 
        append = FALSE,  sep = "\t", ncolumns = length(contenuto))

}