

#seleziono colori
colMap <- function(x) {
  .col <- rep(rev(heat.colors(length(unique(x)))), time = table(x))
  return(.col[match(1:length(x), order(x))])
}


#identifico i geni differenzialmente espressi mediante un boolean
topGenes <- function(allScore) {
  boolean<-NULL
  
  .pkgglobalenv<-get0(x=".pkgglobalenv", envir= globalenv(), ifnotfound = NULL) 
  try({
    thresholdSig<-get0("thresholdSig", envir = .pkgglobalenv, ifnotfound = NULL)
    boolean<- allScore > thresholdSig
    })
  return(boolean)
}