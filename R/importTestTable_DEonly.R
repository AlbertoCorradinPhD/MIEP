importTestTable_DEonly<-function(testTablesDir,confronto){
  file<-paste(confronto,"-DEonly.tsv",sep="")
  filename<-file.path(testTablesDir,file)
  table<- read.table( filename, header=TRUE, skip=0, sep="\t") 
  #names(table)
  #str(table)
  ens<-table$ensembleID
  #head(ens)
  table$ensembleID<-sub(pattern="\\.[0-9]+", replacement="", x=ens, ignore.case = FALSE, perl = FALSE,
                                  fixed = FALSE, useBytes = FALSE)
  #head(table$ensembleID)
  return(table)
}