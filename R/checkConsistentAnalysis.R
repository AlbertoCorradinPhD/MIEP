checkConsistentAnalysis<-function(flag_MFtest,flag_cellLine ){
  
  #check
  if (flag_MFtest & flag_cellLine!="Filter") {
    print("erroneous flags. Change them and launch another run")
    exit()
  }   else {print("analysis appears consistent")}
}