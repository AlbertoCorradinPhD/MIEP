chooseAnalysis<-function(DESeqDir){
  #choose analysis
  myLabel<-("insert analysis approach of choice")
  input<-NULL
  while (is.null(input)){
    input<-shinyTextInput(myLabel)
  }
  analysis<-input
  if (analysis=="ABTvsDHEA") {
    cellLines<-c("U2932","OCI_Ly19")
    confronti<-c("ABT_vs_untreated","DHEA_vs_untreated","ABTandDHEA_vs_untreated","ABTandDHEA_vs_ABT") 
    
  } 
  if (analysis=="Everolimus") { 
    cellLines<-c("Jurkat","TALL1","CEM")
    confronti<-c("treated_vs_untreated") 
  } 
  if (analysis=="HeLa") {}
  if (analysis=="longitudinal") { 
    cellLines<-c("4h","12h")
    confronti<-c("EVE_vs_CNTR","Isrib_vs_CNTR","Isrib_Eve_vs_CNTR","Isrib_Eve_vs_EVE") 
  } 
  if (analysis=="snapshot") { 
    cellLines<-c("K562","KG1","ML2")
    confronti<-c("ven_vs_NT","ven_DHEA_vs_NT","DHEA_vs_NT") 
  } 
  if (analysis=="requisiti") { 
    cellLines<-c("PDX13at4h","PDX13at12h","PDX13at24h","PDX19at4h","PDX19at12h","PDX19at24h")
    confronti<-c("EVE_vs_CNTR") 
  } 
  if (analysis=="PDX") { 
    cellLines<-c("PDX13at4h","PDX13at12h","PDX13at24h","PDX19at4h","PDX19at12h","PDX19at24h")
    confronti<-c("EVE_vs_CNTR") 
  } 
  if (analysis=="twoWaves") { 
    cellLines<-c("CEM","TALL")
    confronti<-c()
    for (i in 1:length(cellLines)){
      linea<- cellLines[i]
      filename<-paste(DESeqDir,"test-",linea,"/confrontiEseguiti.txt",sep="")
      confronti<-c(confronti,scan(file = filename, what = "character", sep = "\n"))
      confronti<-unique(confronti)
    }
  } 
  lista<-list(analysis,cellLines,confronti)
  return(lista)
}
 