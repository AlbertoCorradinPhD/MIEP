expressionSet_template<-function(data,pData, infoExprSet){

  #EXPRESSION DATA
  exprs <- as.matrix(data)
  #class(exprs)
  #dim(exprs)
  #colnames(exprs)
  #head(exprs)
  
  # PHENOTYPE DATA
  ###############################################################################
  # essential feature of the relationship between the assay and phenotype data; 
  # ExpressionSet will complain if these names do not match.
  ###############################################################################
  
  #PDATA
  #dim(pData)
  #rownames(pData)
  #summary(pData)
  #check
  #print("check:")
  #print(all(rownames(pData)==colnames(exprs)))

  # METADATA
  metadata <- data.frame(labelDescription=
                           c("Sample name",
                             "Treatment",
                             "Type",
                             "Replicates details",
                             "Cell line"),
                         row.names=colnames(pData)
                        )
  
  # PHENODATA
  phenoData <- new("AnnotatedDataFrame", data=pData, varMetadata=metadata)
  #class(phenoData)
  
  # EXPERIMENT DATA
  experimentData <- new("MIAME",
                        name=infoExprSet$name,
                        lab=infoExprSet$lab,
                        contact=infoExprSet$contact,
                        title=infoExprSet$title,
                        abstract="Normalized counts",
                        url="www.does.not.exist",
                        preprocessing=list("DESeq2"),
                        other=list(origin="Normalized RNA-seq data", 
                                   notes="")
                      )
  
  eset  <- ExpressionSet(assayData=exprs, phenoData=phenoData, 
                        experimentData=experimentData, annotation="hgnc_symbol"
                        )  
  return(eset)

}
