selectDEfeatures<-function( DESeqDir, testDir, controls, 
                           flag_shrinkage, flag_cellLine, flag_MFtest,
                           padj_max,FC_min, 
                           flag_regionReport,flag_reportingTools
                          ){
  

  for (c in 1: length(controls)){
    controllo<-controls[c]
    print(paste("call to features selection, based on control samples: ", controllo,sep=""))
    dds<-loadTestedDeSeqObj(controllo, testDir)
    print("see loaded DeSeq object:")
    printDeSeqObj(dds)
    DEfeatures_xControl(  DESeqDir, testDir, DeSeqObj=dds, 
                        flag_shrinkage, flag_cellLine, flag_MFtest,
                        padj_max,FC_min, 
                        flag_regionReport,flag_reportingTools
                       )
      
    } # end FOR loop controlli
}