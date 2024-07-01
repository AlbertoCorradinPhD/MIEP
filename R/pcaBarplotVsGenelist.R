pcaBarplotVsGenelist<-function( indexTable,data,  resDir, linea,
                                geneset=NULL, Kmax=50,e=globalenv()) {
  ###  IMPORT DATA FOR REPARTITION IN SUBSET

  names(data)
  M<-dim(data)[2]
  #print(M)
  conte_totali<-apply(data[,3:M],1, sum) #somma per righe
  names(conte_totali)<- data$ID_REF
  #head(conte_totali,10)
  sommario<-summary(conte_totali)
  #print(sommario)
  
  # DIVISIONE IN 3 SUBSETS
  #genes below 1st quantile
  threshold1<-sommario[2] 
  geneset1_boolean<-conte_totali<=threshold1
  geneset1<-names(conte_totali[geneset1_boolean==TRUE])
  color1<-"red"
  #genes above 3rd quantile
  threshold3<-sommario[5] 
  geneset3_boolean<-conte_totali>threshold3
  geneset3<-names(conte_totali[geneset3_boolean==TRUE])
  color3<-"green"
  #commoners
  geneset2<-names(conte_totali[geneset1_boolean==FALSE &  geneset3_boolean==FALSE  ])
  color2<-"orange"
  firelight<-c(color1,color2,color3)
  
  # CLASSIFICA PER EXPRESSION LEVEL
  gene_symbols<-indexTable$feature
  sets<-rep("In the middle",length(gene_symbols))
  BelowT1<-gene_symbols %in% geneset1
  AboveT3<-gene_symbols %in% geneset3
  sets[BelowT1]<-"Below 1st quantile"
  sets[AboveT3]<-"Above 3rd quantile"
  sets<-factor(sets, levels=c("Below 1st quantile", "In the middle","Above 3rd quantile"))
  indexTable$sets<-sets

  # CONSIDERA ANCHE GENELIST D'INTERESSE
  if (!is.null( geneset)) {
    lista<-genesetInsertInBarplot( resDir, geneset, e=e)
    legenda<-lista[[1]]
    geneList<-lista[[2]]
    rm(lista)
  } else {
    legenda<-NULL
    geneList<-NULL
  }
  
  
  
  #ordina indexTable
  names(indexTable)
  nameorder <-  indexTable$feature[order( indexTable$importance)]
  # Turn name into a factor, with levels in the order of nameorder
  indexTable$feature<- factor( indexTable$feature, levels=nameorder)
  indexTableTop<-indexTable
  if (dim(indexTable)[1]>Kmax){
    indexTableTop<-indexTable[1:Kmax,]
  }
  #head(indexTableTop)
  
  #AGGIUSTAMENTO COLORI CLASSIFICA PER EXPRESSION LEVEL
  boolean<- "Below 1st quantile" %in% indexTableTop$sets
  if (!boolean){ firelight<-c(color2,color3) }
  boolean<- "Above 3rd quantile" %in% indexTableTop$sets
  if (!boolean){ firelight<-c(color1,color2) }
  boolean<- ("Below 1st quantile" %in% indexTableTop$sets | "In the middle" %in% indexTableTop$sets)
  if (!boolean){ firelight<-c(color3) }
  boolean<- ("Above 3rd quantile" %in% indexTableTop$sets | "In the middle" %in% indexTableTop$sets)
  if (!boolean){ firelight<-c(color1) }
  
  
  ### BARPLOT A LINEE
  font_size<-15
  titolo<-paste("VARIABLE IMPORTANCE IN:",linea)
  
  gene_symbols<-indexTableTop$feature
  inGenelist<-gene_symbols %in% geneList
  #head(inGenelist,10)
  indexTableTop$inGenelist<-inGenelist
  #str(indexTable)
  if (sum(inGenelist)==0){ 
    linetypes<-"dotted" 
    legenda
    p<-ggplot(indexTableTop, aes(x=.data$importance, y=.data$feature)) +
      #theme(text = element_text(size=30))+
      geom_point(size=3, aes(colour=sets))+
      scale_color_manual(values=firelight)+
      labs(x = "importance", y= "Gene Symbol") +
      labs(colour = "Quantity of reads", font=font_size)+
      geom_segment(aes(yend=.data$feature, 
                       linetype="No relevant gene set was inserted"), 
                   xend=0, colour="grey50",size=1 ) +
      scale_linetype_manual(values=linetypes)+
      labs(linetype = legenda, font=font_size)+
      ggtitle(titolo)+
      #scale_colour_brewer(palette="Set1", limits=c("set 1","set 0"), guide=FALSE) +
      theme_bw(base_size = font_size, base_line_size = 1) +
      theme(panel.grid.major.y = element_blank(), # No horizontal grid lines
            legend.position=c(0.97, 0.15), # Put legend inside plot area
            legend.justification=c(1, 0.5),
            plot.title = element_text(angle = 0,  face = 'bold.italic', vjust = 1)
      ) 
  } else  { 
    linetypes<-c("dotted","solid") 
    p<-ggplot(indexTableTop, aes(x=.data$importance, y=.data$feature)) +
      #theme(text = element_text(size=30))+
      geom_point(size=3, aes(colour=sets))+
      scale_color_manual(values=firelight)+
      labs(x = "importance", y= "Gene Symbol") +
      labs(colour = "Quantity of reads", font=font_size)+
      geom_segment(aes(yend=.data$feature, linetype = .data$inGenelist), xend=0, 
                   colour="grey50",size=1 ) +
      scale_linetype_manual(values=linetypes)+
      labs(linetype = legenda, font=font_size)+
      ggtitle(titolo)+
      #scale_colour_brewer(palette="Set1", limits=c("set 1","set 0"), guide=FALSE) +
      theme_bw(base_size = font_size, base_line_size = 1) +
      theme(panel.grid.major.y = element_blank(), # No horizontal grid lines
            legend.position=c(0.97, 0.15), # Put legend inside plot area
            legend.justification=c(1, 0.5),
            plot.title = element_text(angle = 0,  face = 'bold.italic', vjust = 1)
      ) 
  
  
  }
  return(p)
  
}
