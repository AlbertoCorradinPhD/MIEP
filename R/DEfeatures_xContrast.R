DEfeatures_xContrast <- function( testDir, DeSeqObj, 
                                 flag_shrinkage, flag_cellLine,
                                 padj_max,FC_min, 
                                 nameOfContrast, testName
                                ) {
  
  #crea oggetti con i risultati
  res<-creaResultsObj(nameOfContrast,DeSeqObj)   #df7<-as.data.frame(res) 
  printResObj(resultsObj=res)
 
  ### FIX NA INDEPENDENT FILTER
  res<-fixNAs(resultsObj=res)
  printResObj(resultsObj=res)
  
  
  ###########################################################################
  ### THRESHOLDS TO DEFINE DE
  ###########################################################################
  flag_PADJ<-FALSE
  flag_FC<-FALSE
  filename<-paste("padjMax_", testName,"_beforeShrinkage.txt",sep="")
  filePath1<-file.path(testDir,filename)
  filename<-paste("FCmin_", testName,"_beforeShrinkage.txt",sep="")
  filePath2<-file.path(testDir,filename)
  lista<-thresholdsForDEs(resultsObj=res,filename1=filePath1, filename2=filePath2,
                          flag_PADJ,flag_FC,padj_max,FC_min)
  padj_max<-lista[[1]]
  FC_min<-lista[[2]]
  flag_PADJ<-lista[[3]]
  flag_FC<-lista[[4]]

  ### GRAPHIC INSPECTION BEFORE SHRINKAGE
  outDir<-file.path(testDir,"graphicInspection")
  dir.create(outDir, showWarnings = FALSE, recursive = TRUE)
  outDir<-file.path(outDir,testName)
  dir.create(outDir, showWarnings = FALSE, recursive = TRUE)
  outDir<-file.path(outDir,"01_beforeShrinkage")
  dir.create(outDir, showWarnings = FALSE, recursive = TRUE)
  print("call to graphic Inspection")
  tryCatch({
    #non passo flag_shrinkage in questo momento
    graphicInspection(outDir,DeSeqObj, 
                      resultsObj=res,testName, nameOfContrast, 
                      padj_max,FC_min) 
    },
    error = function(err){
      print("difficulties in producing plots")
    })#chiudo tryCatch
  print("end of Graphic Inspection before shrinkage")
  
  ###########################################################################
  ### EVENTUALE SHRINKAGE
  ###########################################################################
  
  if (flag_shrinkage!="none"){
    try({
      print("call to LFC shrinkage")
      lista<-applyShrinkage( testDir, DeSeqObj, 
                          nameOfContrast, testName,
                          flag_shrinkage, flag_PADJ, flag_FC,
                          padj_max,FC_min)
      resShrinked<-lista[[1]]
      padj_max<-lista[[2]]
      FC_min<-lista[[3]]
      rm(lista)
      printResObj(resultsObj=resShrinked)
      res<-resShrinked
      })#chiude try
    }
  
 
  ############################################################################
  ### SELECT DIFFERENTIALLY EXPRESSED FEATURES
  ###########################################################################
  
  print("test tables chapter")
  outDir<-file.path(testDir,"testTables")
  dir.create(outDir, showWarnings = FALSE, recursive = TRUE)
  lista<-creaTestTables(outDir,resultsObj=res,testName, padj_max, FC_min)
  df_DEonly<-lista[[1]]
  res_DEonly<-lista[[2]]
  rm(lista)
  print("end of test Tables chapter")
  
  seqsOfInterest<-df_DEonly$ensembleID
  dds_DEonly<-subsettingDeSeqObj(DeSeqObj,seqsOfInterest)
  lista<-list(dds_DEonly,res_DEonly)
  return(lista)

  }
