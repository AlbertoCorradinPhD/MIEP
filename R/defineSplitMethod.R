defineSplitMethod<-function(analysis, coldata){
  
  splitMethod<-paste(coldata$condition, coldata$cellLine, sep="_")
 
  if (analysis=="ABTvsDHEA") {
    rowS<-5
  }
  if (analysis=="longitudinal") {
    splitMethod<-factor(splitMethod, 
                        levels=c("CNTR-4h","CNTR-12h","Isrib-4h","Isrib-12h","Isrib_Eve-4h","Isrib_Eve-12h","EVE-4h","EVE-12h"))
    rowS<-6
    }
  if (analysis=="Everolimus") {
    rowS<-5
  }
  if (analysis=="snapshot") {
    splitMethod<-sub(pattern="NT$", replacement="without", x=splitMethod, ignore.case = FALSE, perl = FALSE)
    splitMethod<-sub(pattern="ven$", replacement="without", x=splitMethod, ignore.case = FALSE, perl = FALSE)
    splitMethod<-sub(pattern="ven_DHEA$", replacement="with", x=splitMethod, ignore.case = FALSE, perl = FALSE)
    splitMethod<-sub(pattern="DHEA$", replacement="with", x=splitMethod, ignore.case = FALSE, perl = FALSE)
    rowS<-4
  }
  if (analysis=="requisiti") {
    splitMethod<-factor(splitMethod, 
                        levels=c("CNTR-PDX13at4h","CNTR-PDX13at12h","CNTR-PDX13at24h",
                                 "EVE-PDX13at4h","EVE-PDX13at12h","EVE-PDX13at24h" ,
                                 "CNTR-PDX19at4h", "CNTR-PDX19at12h", "CNTR-PDX19at24h",
                                 "EVE-PDX19at4h",  "EVE-PDX19at12h","EVE-PDX19at24h"))
    rowS<-4
  }
  if (analysis=="PDX") {
     splitMethod<-factor(splitMethod, 
                        levels=c("CNTR-PDX13at4h","CNTR-PDX13at12h","CNTR-PDX13at24h",
                                 "EVE-PDX13at4h","EVE-PDX13at12h","EVE-PDX13at24h" ,
                                 "CNTR-PDX19at4h", "CNTR-PDX19at12h", "CNTR-PDX19at24h",
                                 "EVE-PDX19at4h",  "EVE-PDX19at12h","EVE-PDX19at24h"))
    rowS<-3
  }
  if (analysis=="twoWaves") {
    splitMethod<-factor(splitMethod, levels=c("T0_CEM" ,"T0_TALL","T1_CEM","T1_TALL",
                                              "T2_CEM","T2_TALL","T4_CEM" ,"T4_TALL",
                                              "T8_CEM", "T8_TALL", "T24_CEM","T24_TALL"))
    rowS<-3
  }
  lista<-list(splitMethod, rowS)
  return(lista)
}