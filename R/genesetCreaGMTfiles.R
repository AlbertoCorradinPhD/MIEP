genesetCreaGMTfiles<-function(name,title, description, 
                         genes,outDir){
  
  filename<-paste(name,".gmt",sep="")
  filePath<-file.path(outDir,filename)
  contenuto<-c(title,description,genes)
  write(x=contenuto, file = filePath, 
        append = FALSE,  sep = "\t", ncolumns = length(contenuto))
  return(contenuto)

}

# name<- "ResponseToEndoplasmaticReticulumStress"
# title<- "Response To Endoplasmatic Reticulum Stress"
# description<- ">inhouse built by Micol Silic-Benussi"
# genes<-scan(file = file.choose(), what = character(), sep = "\n") 
# outDir<-"/home/odysseus/Documents/gruppoCiminale/databases_x_GSEA/gmtFiles"
# contenuto<-genesetCreaGMTfiles(name,title, description, genes,outDir)