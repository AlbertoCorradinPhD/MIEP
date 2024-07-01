
selectSequencesWithName<-function(inDir,outDir, DESeqDir){
  #IMPORTA ANNOTAZIONI
  
  filePath<-file.path(inDir,"annotations_cleaned.tsv")
  annotations<-read.table(file=filePath, header = TRUE, sep = "\t", quote="\"",
                          strip.white = FALSE, blank.lines.skip = FALSE)
  
  
  
  ### IMPORT NORMALIZED COUNTS
  #conte normalizzate nell'insieme di tutte le linee
  filePath<-file.path(DESeqDir,"normalizedCounts_afterFiltering.tsv")
  data<-read.table(file=filePath, header = TRUE, sep = "\t",
                   strip.white = TRUE, blank.lines.skip = TRUE) #se prendo UN tsv
  
  names(data)
  ens<-data$ensembleID
  data$ensembleID<-sub(pattern="\\.[0-9]+", replacement="", x=ens, ignore.case = FALSE, perl = FALSE,
                       fixed = FALSE, useBytes = FALSE)
  
  ###############################################################################
  ###  ANNOTATED NORMALIZED COUNTS
  ###############################################################################
  
  
  #tolgo le sequenze prive di un nome in nomenclatura
  annotations<-data.table(annotations)
  ann_withSymbolOnly<-subset(x=annotations, annotations$hgnc_symbol!="" ) #tolgo le sequenze cui non corrisponde alcun gene symbol
  ann_withSymbolOnly<-data.table(ann_withSymbolOnly)
  filePath<-file.path(outDir,"annotations_withSymbolOnly.tsv")
  write.table(x=ann_withSymbolOnly, file =filePath,  col.names = TRUE, quote = FALSE, sep="\t", row.names = FALSE)
  
  ######## PUT ASIDE SEQUENCES WITH MULTIPLE ANNOTATIONS #########################
  
  #verifico se una sequenza corrisponde a più geni. Quindi a più nomi
  multipleAnn<-ann_withSymbolOnly[duplicated(ann_withSymbolOnly$ensembl_gene_id),]
  
  #check
  names(ann_withSymbolOnly)
  id_test<-multipleAnn$ensembl_gene_id[1]
  boolean<-with(ann_withSymbolOnly,ensembl_gene_id==id_test)
  #print("test:")
  #print(ann_withSymbolOnly[boolean,])
  
  #estraggo i dati corrispondenti ai duplicati: li recupero dopo
  data<-data.table(data)
  #check
  #print("number of seqeunces with multiple gene names")
  LM<-sum(data$ensembleID %in% multipleAnn$ensembl_gene_id)
  #print(LM)
  if (LM>0){
    additionalRows<-data[data$ensembleID %in% multipleAnn$ensembl_gene_id]
  }
  
  ############### IDENTIFY UNKNOWN SEQUENCES ###################################
  # tolgo sequenze senza nome
  #NB: le annotazioni sono fatte sulle conte grezze mentre i dati derivano dal filtraggio di DeSeq 
  intersezione<-intersect(data$ensembleID,ann_withSymbolOnly$ensembl_gene_id)
  sequenzeSenzaNome<-setdiff(ann_withSymbolOnly$ensembl_gene_id,intersezione)
  #check
  #print("CHECK: probes senza nome nel file dati:")
  #print(length(sequenzeSenzaNome)) #NB: sono tantissime
  
  ############### FILTER AWAY UNKNOWN SEQUENCES ###################################
  #filteredData: considero solo le sequenze con nome
  filteredData<-data[data$ensembleID %in% intersezione] #prendo solo le sequenze cui corrisponde un gene symbol
  filteredData<-as.data.table(filteredData)
  boolean<-ann_withSymbolOnly$ensembl_gene_id %in% intersezione
  filteredAnn<-ann_withSymbolOnly[boolean]
  filteredAnn<-as.data.table(filteredAnn)
  
  ############## SEPARATE DUPLICATES ###########################################
  
  #tolgo i duplicati per recuperarli dopo
  uniqueAnn<-filteredAnn[!duplicated(filteredAnn$ensembl_gene_id),]
  check<-all(filteredData$ensembleID==uniqueAnn$ensembl_gene_id)
  #print("check: ho tolto i duplicati?")
  #print(check)
  #riordino. NB: va bene match poiché il confronto è fatto con database uniqueAnn
  indici_simboli<-match(x=filteredData$ensembleID, table=uniqueAnn$ensembl_gene_id)
  filteredData$hgnc_symbol<-uniqueAnn$hgnc_symbol[indici_simboli]
  
  #recupero le sequenze con annotazioni multiple
  if (LM>0){ #DA VERIFICARE IL CASO PIU' COMPLESSO
    sequenzeMultiple<-unique(additionalRows$ensembleID)
    for (s in 1:LM) {
      seq<-additionalRows$ensembleID[s]
      boolean<-multipleAnn$ensembl_gene_id == seq
      indici_simboli<-which(x=boolean, arr.ind = TRUE)
      for (j in 1: length(indici_simboli)){ #DA PERFEZIONARE PER I CASI PIÙ COMPLESSI
        additionalRows$hgnc_symbol[j]<-multipleAnn$hgnc_symbol[indici_simboli]
      }
    }
    #indici_simboli<-match(x=additionalRows$ensembleID, table=multipleAnn$hgnc_symbol)
    #additionalRows$hgnc_symbol<-multipleAnn$hgnc_symbol[indici_simboli]
    dataWithAddition<-rbind(filteredData,additionalRows)
  } else {  dataWithAddition<-filteredData }
  
  names(dataWithAddition) 
  #cambio nome alla colonna
  names(dataWithAddition)[names(dataWithAddition) == "hgnc_symbol"] <- "ID_REF" #coerente con GSEA input
  names(dataWithAddition) 
  #tolgo la dicitura Ensemble delle sequenze, mantengo invece il Gene Symbol
  dataWithAddition<-subset(dataWithAddition, select=names(dataWithAddition)!="GeneID")
  dim(dataWithAddition)
  #tolgo formattazione data table per riordinare le colonne 
  dataWithAddition<-as.data.frame(dataWithAddition)
  columns<-c(dim(dataWithAddition)[2],1:(dim(dataWithAddition)[2]-1))
  dataWithAddition<- dataWithAddition[,columns]
  names(dataWithAddition)
  
  #check
  #print("CHECK: sequenze non uniche nel database:")
  #boolean<-duplicated(dataWithAddition$ensembleID)
  #dataWithAddition[boolean,1:2]
  
  #DA CONTE
  filePath<-file.path(outDir,"normalizedCounts_withSymbol.tsv")
  write.table(x=dataWithAddition, file =filePath,  col.names = TRUE, quote = FALSE, sep="\t", row.names = FALSE)

}
