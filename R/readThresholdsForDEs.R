readThresholdsForDEs<-function(padj_max,FC_min, shrinkage, DESeqDir, linea, confronto) {
  
  if (padj_max=="calculated") { 
    if (shrinkage=="none") { 
      filename<-paste("padjMax_",confronto,"_beforeShrinkage.txt",sep="")
      foldername<-paste("test-",linea,sep="")
      filePath<-file.path(DESeqDir,foldername,filename)
    } else  {
      filename<-paste("padjMax_",confronto,"_afterShrinkage.txt",sep="")
      foldername<-paste("test-",linea,sep="")
      filePath<-file.path(DESeqDir,foldername,filename)
      }
    lettura<-scan(file = filePath,sep = "\n",what = "character")
    padj_max<-as.numeric(lettura[2])
  }
  if (FC_min=="calculated") { 
    if (shrinkage=="none") { 
      filename<-paste("FCmin_",confronto,"_beforeShrinkage.txt",sep="")
      foldername<-paste("test-",linea,sep="")
      filePath<-file.path(DESeqDir,foldername,filename)
    } else  {
      filename<-paste("FCmin_",confronto,"_afterShrinkage.txt",sep="")
      foldername<-paste("test-",linea,sep="")
      filePath<-file.path(DESeqDir,foldername,filename)
    }
    lettura<-scan(file = filePath,sep = "\n",what = "character")
    FC_min<-as.numeric(lettura[2])
  }
  
  lista<-list(padj_max,FC_min)
  return(lista)
}