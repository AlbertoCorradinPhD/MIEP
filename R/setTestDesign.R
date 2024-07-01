setTestDesign<-function(DeSeqObj, controls,
                        flag_cellLine, flag_MFtest
                        ){
  
  if (flag_MFtest){
    design(DeSeqObj) <- formula(~ cellLine+condition)
  } else if (flag_cellLine=="Filter" ){
    design(DeSeqObj) <- formula(~ 1)
  } else { 
    design(DeSeqObj) <- formula(~ condition) 
  } 

  return(DeSeqObj)
}