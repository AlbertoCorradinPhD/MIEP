selectBP_GOterms<- function(GO_table,inDir) {
  ### SELECTION OF SUBSETS OF GO TERMS 
  
  print("GO terms phase")
  GOBP_table<-with(GO_table,subset(x=GO_table,namespace_1003=="biological_process"  ))
  names(GOBP_table)
  #elimina doppioni in go_ids
  GOBP_unici<-GOBP_table[,c( "hgnc_symbol","go_id" )]
  GOBP_unici<-GOBP_unici[!duplicated(GOBP_unici),]
  
  
  ##############################################
  ### CREA CORRISPONDENZE GENE->GO
  ##############################################
  names(GO_table)
  geneUniverse<-unique(GO_table$hgnc_symbol)
  ### CORRESPONDENCE TABLES 
  print("gene2GO phase")
  filePath<-file.path(inDir,"gene2GO.tsv")
  gene2GO<-list()
  print(paste("number of genes in Gene Universe:",length(geneUniverse)))
  write(x="", file = filePath, ncolumns = 1, append = FALSE, sep = "\t")
  for (i in 1:length(geneUniverse)){
    gene<-geneUniverse[i]
    write(x=gene, file = filePath, ncolumns = length(gene), append = TRUE, sep = "\t")
    indexes_boolean<-gene == GOBP_unici[,"hgnc_symbol"]
    go_ids<-GOBP_unici[indexes_boolean,"go_id"]
    gene2GO[[gene]] <- go_ids
    write(x=c("",go_ids), file = filePath, ncolumns = (length(go_ids)+1), append = TRUE, sep = "\t")
  }
 
  print("GO2gene phase")
  filePath<-file.path(inDir,"GO2gene.tsv")
  GO2gene<-list()
  go_ids<-unique(GOBP_table[,"go_id"])
  print(paste("number of GO terms considered:",length(go_ids)))
  write(x="", file = filePath, ncolumns =1, append = FALSE, sep = "\t")
  for (i in 1:length(go_ids)){
    term<-go_ids[i]
    write(x=term, file = filePath, ncolumns = length(term), append = TRUE, sep = "\t")
    indexes_boolean<-term == GOBP_table[,"go_id"]
    genes<-unique(GOBP_table[indexes_boolean,"hgnc_symbol"])
    GO2gene[[term]] <- genes
    write(x=c("",genes), file = filePath, ncolumns = (length(genes)+1), append = TRUE, sep = "\t")
  }

  lista<-list(gene2GO=gene2GO,GO2gene=GO2gene,GOBP_table=GOBP_table)
  return(lista)


}
