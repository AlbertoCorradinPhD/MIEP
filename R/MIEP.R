

#' MIEP function launches the Make-it-easy-pipeline for gene set-focused
#' analysis of RNA-seq data
#'
#' @param filePath1 Path to the pipeline folder. Inside it, data folder should
#'   be located in order to allow subsequent uploading of data files (included
#'   raw_gene_counts.tsv, coldata.csv, controls.txt,infoExprSet.tsv, and
#'   genesetsLists.txt). If MIEP is called without passing this parameter's value
#'   directly, the same information on the path will be asked by MIEP
#'   interactively by waiting the selection of a file to be uploaded. First line
#'   of such a text file should report the path to pipeline folder, and will
#'   correspond to parameter filePath1.
#' @param filePath2 Path to the folder containing Gene Matrix Transposed files
#'   (*.gmt) from GSEA web site (www.gsea-msigdb.org/gsea/index.jsp), which
#'   define composition of gene sets of interest.
#' @return A folder with the results of the pipeline. MIEP performs statistical
#'   testing, annotates sequences with gene ID names based on HGNC nomenclature
#'   (https://www.genenames.org/), corrects potential biases due to cell lines
#'   specificity (or batch effect), summarizes the main results by means of
#'   tables in HTML format, heat maps, and dendrograms based on hierarchical
#'   clustering. The latter can be customized to better highlight differences by
#'   specifically designed color palettes. Results' folder will be automatically
#'   created by MIEP inside the pipeline folder. Different results can be
#'   obtained by inserting different parameters interactively or by selecting
#'   different filtering process. If gene sets are supplied MIEP will provide
#'   heat maps focused on gene sets, separately for each of them, volcano plots,
#'   and tables showing results of statistical tests in HTML format.
#' @examples
#' MIEP(filePath1=NULL,filePath2=NULL);
#' MIEP();
#' @export
#' @importFrom grDevices colorRampPalette dev.off heat.colors jpeg png rainbow
#'   rgb tiff
#' @importFrom graphics abline boxplot hist par pie text points
#' @importFrom stats as.dendrogram cor density dist formula hclust median
#'   p.adjust prcomp qnorm quantile relevel rnorm sd predict
#' @importFrom utils read.csv read.table write.csv write.table head str data 
#' @importFrom BiocManager install
#' @importFrom data.table data.table as.data.table
#' @importFrom rlang .data
#' @importFrom grid gpar unit
#' @importFrom magrittr %>%
#' @importFrom stringr str_replace_all
#' @importFrom kableExtra kbl kable_styling save_kable
#' @importFrom biomaRt useMart getBM listAttributes
#' @importFrom ComplexHeatmap Heatmap rowAnnotation anno_simple
#'   HeatmapAnnotation
#' @importFrom rlist list.append
#' @importFrom httr set_config config
#' @importFrom methods new
#' @import ggplot2

MIEP<-function(filePath1=NULL,filePath2=NULL){
  
  .pkgglobalenv<-new.env(parent=emptyenv())
  .globalEnvSetHack<-function(key, val, pos) {
    assign(key,val, envir=as.environment(pos))
  }
  .globalEnvSetHack(key=".pkgglobalenv",val=.pkgglobalenv, pos=1L)
  
  .pkgglobalenv <- get0(".pkgglobalenv", envir = globalenv(), ifnotfound = NULL)
  #check
  #print("environment of the package:")
  #print(.pkgglobalenv )
 
  
  ################# SELECT PATH DIRECTORIES  ###################################
  pipelineDir<-NULL
  gmtFolder<-NULL
  
  suppressWarnings({
	  if (any(is.null(filePath1), is.null(filePath2))){
	    print("select file with paths indicated")
	    tryCatch( {
	      paths<-  scan(file = file.choose(), what = character(), sep="\n")
	      pipelineDir<-paths[1]
	      dataDir<-file.path(pipelineDir,"data")
	      if (length(paths)>1) {
	        assign("gmtFolder", paths[2], envir = .pkgglobalenv)
	       } else {
	        assign("gmtFolder", NULL, envir = .pkgglobalenv)
	       }
	      rm(paths)
	    },
	    error = function(err){
	      print("no paths inserted appropriately. I don't even start")
	    })#chiudo tryCatch
	  } else {
	    pipelineDir<-filePath1
	    dataDir<-file.path(pipelineDir,"data")
	    assign("gmtFolder", filePath2, envir = .pkgglobalenv)	    
	  }#chiude else
  }) #chiude suppressWarnings
  
  if (is.null(pipelineDir)){ 
    print("no pipeline path inserted. I don't even start")
	  try(rm(.pkgglobalenv))
    return() 
  } 
    
  
  ###################################################################
  ### DESeq CHAPTER
  ##################################################################
  
  lista<-myDESeq(pipelineDir, dataDir)
  resDir<-lista[[1]]
  DESeqDir<-lista[[2]]
  rm(lista)
  
  
  ### PIPELINE INFO resDir<-NULL DESeqDir<-NULL 
  suppressWarnings(
    lista<-pipelineInfo_byDate(pipelineDir,  dataDir, resDir, DESeqDir)
  )
  resDir<-lista[[1]]
  DESeqDir<-lista[[2]] 
  cellLines<-lista[[3]]
  confronti<-lista[[4]]
  coldata<-lista[[5]]
  rm(lista)
  
  ###################################################################
  ### ANNOTATIONS CHAPTER
  ##################################################################
  
  lista<-myAnnotations(resDir,  dataDir, DESeqDir, cellLines, confronti)
  inDir1<-lista$inDir1
  inDir2<-lista$inDir2
  rm(lista)
  
  ###################################################################
  ### FEATURES SELECTION CHAPTER
  ##################################################################
  #inDir1<-file.path(resDir,"normalizedCounts")
  #inDir2<-file.path(resDir,"normalizedCounts/DifferentiallyExpressedSeqs/profiling")
  
  myFeaturesSelection(resDir,  inDir1, inDir2, DESeqDir, 
                      cellLines, confronti, coldata)
  
  ###################################################################
  ### DIMENSIONALITY REDUCTION CHAPTER
  ##################################################################
  
  myDimensionalityReduction(resDir,  cellLines, 
                            confronti, coldata, e = .pkgglobalenv)
 
  
  ###################################################################
  ### CREA EXPRESSION SET
  ##################################################################
 
  suppressWarnings(
    exprSet<-expressionSet_wrapper(inDir=inDir1, dataDir,  coldata)
  )
  
  ###################################################################
  ### GO TERMS ENRICHMENT
  ##################################################################
  
  exprSet<-NULL
  tryCatch( { 
    filePath<-file.path(inDir1,"expressionSet.rds")
    exprSet<-readRDS(file = filePath)
  },
  error=function(err) {
    print("no espression set object")
  })
  if (!is.null(exprSet))  {
     repeat {
      myTopGO(resDir,  dataDir, outDir=inDir1, exprSet, 
          cellLines, confronti, e = .pkgglobalenv)
      myLabel<-paste("Let's repeat GO terms' enrichment test with", 
        "different sets of significant genes? No is default")
      input<-NULL
      while (is.null(input)){
        input<-shinyBooleanInput(myLabel)
      }
      ans<-input
      if (!ans) break 
    }#chiude repeat
  }# chiude if
  
  
  ###################################################################
  ### CONDITIONAL RANDOM FORESTS
  ##################################################################
  
  
  ### SELECT CLASSIFICATION
  repeat {
    lista<-classificationMethod( coldata)
    classi<-lista[[1]]
    metodoDiClassificazione<-lista[[2]]
    suppressWarnings( 
      myRandomForests(resDir,  cellLines, confronti,
                      metodoDiClassificazione, classi, e = .pkgglobalenv)
    )
    myLabel<-paste(
    "Let's run classification by conditional random forests",
    "with different classification approach? No is default")
    input<-NULL
    while (is.null(input)){
      input<-shinyBooleanInput(myLabel)
    }
    ans<-input
    if (!ans) break 
  }#chiude repeat
  
  
  ###################################################################
  ### GENELIST OVERVIEW
  ##################################################################
  
  
  #READ 2 GENE LISTS
  #from dataDir
  filePath<-file.path(dataDir,"genesetsList.txt")
  genesetsList1<-c()
  suppressWarnings(
    tryCatch( genesetsList1<-scan(file = filePath,sep = "\n",what = "character"),
          error = function(err){
            print("no gene sets list in data directory")
          })
  )  
  #from resDir
  filePath<-file.path(resDir,"genesetsList.txt")
  genesetsList2<-c()
  suppressWarnings(
    tryCatch( genesetsList2<-scan(file = filePath,sep = "\n",what = "character"),
              error = function(err){
                print("no gene sets list in results directory")
              }) 
    )
  suppressWarnings(
    tryCatch(  {
      genesetsList<-unique(c(genesetsList1,genesetsList2))
      myLabel<-paste("Let's plot histograms of gene counts? No is default (be careful about storage space)")
      input<-NULL
      while (is.null(input)){
        input<-shinyBooleanInput(myLabel)
      }
      flag<-input
      myOverview(resDir, DESeqDir, genesetsList, cellLines,  
                 confronti, coldata, flag, e = .pkgglobalenv)
    },
      error = function(err){   
        print("no gene sets list found")
    })  #close tryCatch 
  )#close warnings
  
  
  ###################################################################
  ### REMOVE GLOBAL VARIABLES CREATED BY THE PACKAGE
  ##################################################################
  removeGlobalVariables()

}

