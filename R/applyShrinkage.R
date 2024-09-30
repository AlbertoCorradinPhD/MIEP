

applyShrinkage<-function( testDir, DeSeqObj, 
                         nameOfContrast, testName,
                         flag_shrinkage, flag_PADJ, flag_FC,
                         padj_max,FC_min){
  ### SHRINKAGE
  try({ resShrinked<-shrinkage(DeSeqObj, nameOfContrast, flag=flag_shrinkage)  })
  
  if (!exists("resShrinked")){ #if N.1
    print("no shrinked object")
    exit()
  } else { #chiude if N.1
    print("shrinkage performed") 
    resShrinked<-fixNAs(resultsObj=resShrinked)
    
    ### UPDATE THRESHOLDS IF NECESSARY
    if (flag_PADJ || flag_FC){ #if N.2
      if (flag_PADJ) { padj_max<-"calculated"}
      if (flag_FC) { FC_min<-"calculated"}
      filename<-paste("padjMax_", testName,"_afterShrinkage.txt",sep="")
      filePath1<-file.path(testDir,filename)
      filename<-paste("FCmin_", testName,"_afterShrinkage.txt",sep="")
      filePath2<-file.path(testDir,filename)
      lista<-thresholdsForDEs(resultsObj=resShrinked,filename1=filePath1, 
                              filename2=filePath2, flag_PADJ,flag_FC,
                              padj_max,FC_min)
      
      padj_max<-lista[[1]]
      FC_min<-lista[[2]]
      flag_PADJ<-lista[[3]]
      flag_FC<-lista[[4]]
      rm(lista)
    }#if N.2
    
    ### GRAPHIC INSPECTION AFTER SHRINKAGE
    outDir<-file.path(testDir,"graphicInspection")
    dir.create(outDir, showWarnings = FALSE, recursive = TRUE)
    outDir<-file.path(outDir,testName)
    dir.create(outDir, showWarnings = FALSE, recursive = TRUE)
    outDir<-file.path(outDir,"02_afterShrinkage")
    dir.create(outDir, showWarnings = FALSE, recursive = TRUE)
    print("call to graphic Inspection")
    tryCatch({
      #qui invece passo flag_shrinkage
         graphicInspection(outDir,DeSeqObj, 
                           resultsObj=resShrinked,testName, nameOfContrast, 
                           padj_max,FC_min,
                           flag_shrinkage=flag_shrinkage)
      
    },
    error = function(err){
      print("difficulties in producing plots")
    })#chiudo tryCatch
    print("end of graphic Inspection")
    
    lista<-list(resShrinked, padj_max,FC_min)
    return(lista)
  } # chiude else
  
}
