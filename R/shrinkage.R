#implementati i test per trovare i differenzialmente espressi
shrinkage <- function(DeSeqObj, nameOfContrast, flag) {
  frase<-paste("shrinkage to apply: ", flag)
  print(frase)
  resShrinked <- lfcShrink(DeSeqObj, coef=nameOfContrast, type=flag)
  return(resShrinked)
}