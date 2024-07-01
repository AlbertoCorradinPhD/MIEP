prepareForTest<-function(DeSeqObj, DESeqDir, controls,
                         flag_cellLine, flag_MFtest
                          ){
 
 
  ### SET TEST DESIGN
  dds<-setTestDesign(DeSeqObj, controls,
                       flag_cellLine,flag_MFtest
                        )
  #set test directory
  testDir<-creaTestDir(DESeqDir, flag_cellLine, flag_MFtest) #prefisso di default
  
  lista<-list(dds, testDir)
  return(lista)
}