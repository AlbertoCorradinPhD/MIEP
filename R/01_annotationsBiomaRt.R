

annotationsBiomaRt<-function(inDir,outDir){
  # remove.packages("biomaRt")
  #  install.packages("remotes")
  #BiocManager::install('grimbough/biomaRt')
  
  #RETRIEVE ANNOTATIONS CONNECTING TO DATABASE

  #aggiungere nome colonna GeneID
  #DA RAW COUNTS
  filePath<-file.path(inDir,"raw_gene_counts.tsv")
  data<-read.table(file=filePath, header = TRUE, sep = "\t",
                   strip.white = TRUE, blank.lines.skip = TRUE) #se prendo UN tsv
  
  names(data)
  ens<-data$GeneID
  data$GeneID<-sub(pattern="\\.[0-9]+", replacement="", x=ens, ignore.case = FALSE, perl = FALSE,
                   fixed = FALSE, useBytes = FALSE)
  
  
  #### BIOMART CONNECTION ################
  
 
  #CREO CONNESSIONE
  flag<-FALSE
  repeat {
    tryCatch({
      mart<-creaConnessioneEnsemble()
      
      annotations <- getBM(attributes=c("ensembl_gene_id","hgnc_symbol","description"), 
                           filters="ensembl_gene_id", values =data$GeneID, mart=mart)
      flag<-TRUE
      if (flag){ break }
    },
    error = function(err){
      print("problems in downloading from ensemble database. Let's try again")
   })# chiude tryCatch
  }#chiude repeat
  
  filePath<-file.path(outDir,"annotations.tsv")
  write.table(x=annotations, file =filePath,  col.names = TRUE, quote = FALSE, sep="\t", row.names = FALSE)
  
  #############################################################################################
  ### CLEANING
  ################################################################################################
  
  lista<-cleaning(annotations)
  annotations_cleaned<-lista[[1]]
  annotations_discarded<-lista[[2]]
  
  ######################### WRITE FILES ####################
  
  filePath<-file.path(outDir,"annotations_cleaned.tsv")
  write.table(x=annotations_cleaned, file = filePath, append = FALSE, quote = FALSE, 
              sep = "\t", eol = "\n", na = "NA", dec = ".", row.names = FALSE,
              col.names = TRUE)
  
  
  filePath<-file.path(outDir,"annotations_discarded.tsv")
  write.table(x=annotations_discarded, file = filePath, append = FALSE, quote = FALSE, 
              sep = "\t", eol = "\n", na = "NA", dec = ".", row.names = FALSE,
              col.names = TRUE)

}
