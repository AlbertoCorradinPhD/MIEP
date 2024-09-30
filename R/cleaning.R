cleaning<-function(annotations) {
  
  
  #flag_mRNAs
  myLabel<-("Do you want to keep miRNAs? No is default")
  input<-NULL
  while (is.null(input)){
    input<-shinyBooleanInput(myLabel)
  }
  flag_miRNAs<-input
  
  goodRows<-c(1:dim(annotations)[1])
  badRows<-c()
  
    #LINC
  indexes<-grep(pattern="long intergenic", x=annotations$description, ignore.case = TRUE, perl = TRUE, value = FALSE,
                fixed = FALSE, useBytes = FALSE, invert = FALSE)
  #check
  goodRows<-setdiff(goodRows,indexes)
  badRows<-c(badRows,indexes)
  
  #ORF
  indexes<-grep(pattern="open reading frame", x=annotations$description, ignore.case = TRUE, perl = TRUE, value = FALSE,
                fixed = FALSE, useBytes = FALSE, invert = FALSE)
  #check
  goodRows<-setdiff(goodRows,indexes)
  badRows<-c(badRows,indexes)
  
  
  #MIR
  if (!flag_miRNAs) {
    indexes<-grep(pattern="microRNA", x=annotations$description, ignore.case = TRUE, perl = TRUE, value = FALSE,
                  fixed = FALSE, useBytes = FALSE, invert = FALSE)
    #check
    goodRows<-setdiff(goodRows,indexes)
    badRows<-c(badRows,indexes)
  } #chiudi if
  
  #AS (da tenere invece)
  indexes<-grep(pattern="antisense", x=annotations$description, ignore.case = TRUE, perl = TRUE, value = FALSE,
                fixed = FALSE, useBytes = FALSE, invert = FALSE)
  #check
  goodRows<-setdiff(goodRows,indexes)
  badRows<-c(badRows,indexes)
  
  #FAM
  indexes<-grep(pattern="sequence similarity", x=annotations$description, ignore.case = TRUE, perl = TRUE, value = FALSE,
                fixed = FALSE, useBytes = FALSE, invert = FALSE)
  #check
  goodRows<-setdiff(goodRows,indexes)
  badRows<-c(badRows,indexes)
  
  #SNO
  indexes<-grep(pattern="small nucleolar", x=annotations$description, ignore.case = TRUE, perl = TRUE, value = FALSE,
                fixed = FALSE, useBytes = FALSE, invert = FALSE)
  #check
  goodRows<-setdiff(goodRows,indexes)
  badRows<-c(badRows,indexes)
  
  #pseudogene
  indexes<-grep(pattern="pseudogene", x=annotations$description, ignore.case = TRUE, perl = TRUE, value = FALSE,
                fixed = FALSE, useBytes = FALSE, invert = FALSE)
  #check
  goodRows<-setdiff(goodRows,indexes)
  badRows<-c(badRows,indexes)
  
  #divergent transcript
  indexes<-grep(pattern="divergent transcript", x=annotations$description, ignore.case = TRUE, perl = TRUE, value = FALSE,
                fixed = FALSE, useBytes = FALSE, invert = FALSE)
  #check
  goodRows<-setdiff(goodRows,indexes)
  badRows<-c(badRows,indexes)
  
  #novel transcript
  indexes<-grep(pattern="novel transcript", x=annotations$description, ignore.case = TRUE, perl = TRUE, value = FALSE,
                fixed = FALSE, useBytes = FALSE, invert = FALSE)
  #check
  goodRows<-setdiff(goodRows,indexes)
  badRows<-c(badRows,indexes)
  
  #novel protein
  indexes<-grep(pattern="novel protein", x=annotations$description, ignore.case = TRUE, perl = TRUE, value = FALSE,
                fixed = FALSE, useBytes = FALSE, invert = FALSE)
  #check
  goodRows<-setdiff(goodRows,indexes)
  badRows<-c(badRows,indexes)
  
  #intronic transcript
  indexes<-grep(pattern="intronic transcript", x=annotations$description, ignore.case = TRUE, perl = TRUE, value = FALSE,
                fixed = FALSE, useBytes = FALSE, invert = FALSE)
  #check
  goodRows<-setdiff(goodRows,indexes)
  badRows<-c(badRows,indexes)
  
  #readthrough
  indexes<-grep(pattern="readthrough", x=annotations$description, ignore.case = TRUE, perl = TRUE, value = FALSE,
                fixed = FALSE, useBytes = FALSE, invert = FALSE)
  #check
  goodRows<-setdiff(goodRows,indexes)
  badRows<-c(badRows,indexes)
  
  #host gene
  indexes<-grep(pattern="host gene", x=annotations$description, ignore.case = TRUE, perl = TRUE, value = FALSE,
                fixed = FALSE, useBytes = FALSE, invert = FALSE)
  #check
  goodRows<-setdiff(goodRows,indexes)
  badRows<-c(badRows,indexes)
  
  #competing endogenous
  indexes<-grep(pattern="competing endogenous", x=annotations$description, ignore.case = TRUE, perl = TRUE, value = FALSE,
                fixed = FALSE, useBytes = FALSE, invert = FALSE)
  #check
  goodRows<-setdiff(goodRows,indexes)
  badRows<-c(badRows,indexes)
  
  # overlapping transcript
  indexes<-grep(pattern="overlapping transcript", x=annotations$description, ignore.case = TRUE, perl = TRUE, value = FALSE,
                fixed = FALSE, useBytes = FALSE, invert = FALSE)
  #check
  goodRows<-setdiff(goodRows,indexes)
  badRows<-c(badRows,indexes)
  
  # RNA first case
  indexes<-grep(pattern="RNA,", x=annotations$description, ignore.case = TRUE, perl = TRUE, value = FALSE,
                fixed = FALSE, useBytes = FALSE, invert = FALSE)
  #check
  goodRows<-setdiff(goodRows,indexes)
  badRows<-c(badRows,indexes)
  
  # RNA second case
  indexes<-grep(pattern=" RNA", x=annotations$description, ignore.case = TRUE, perl = TRUE, value = FALSE,
                fixed = FALSE, useBytes = FALSE, invert = FALSE)
  #check
  goodRows<-setdiff(goodRows,indexes)
  badRows<-c(badRows,indexes)
  
  # RNA third case
  indexes<-grep(pattern="RNA component", x=annotations$description, ignore.case = TRUE, perl = TRUE, value = FALSE,
                fixed = FALSE, useBytes = FALSE, invert = FALSE)
  #check
  goodRows<-setdiff(goodRows,indexes)
  badRows<-c(badRows,indexes)
  
  #mitochondrially encoded (da tenere invece)
  #indexes<-grep(pattern="mitochondrially encoded", x=annotations$description, ignore.case = TRUE, perl = TRUE, value = FALSE,
  #              fixed = FALSE, useBytes = FALSE, invert = FALSE)
  #check
  #head(annotations[indexes,1:3])
  #goodRows<-setdiff(goodRows,indexes)
  #badRows<-c(badRows,indexes)
  
  #ribosomal protein
  #indexes<-grep(pattern="ribosomal protein", x=annotations$description, ignore.case = TRUE, perl = TRUE, value = FALSE,
  #              fixed = FALSE, useBytes = FALSE, invert = FALSE)
  #check
  #head(annotations[indexes,1:3])
  #goodRows<-setdiff(goodRows,indexes)
  #badRows<-c(badRows,indexes)
  
  #AFFX IN affy_hg_u133_plus_2
  indexes<-grep(pattern="AFFX", x=annotations$affy_hg_u133_plus_2, ignore.case = TRUE, perl = TRUE, value = FALSE,
                fixed = FALSE, useBytes = FALSE, invert = FALSE)
  #check
  goodRows<-setdiff(goodRows,indexes)
  badRows<-c(badRows,indexes)
  
  annotations_cleaned<-annotations[goodRows,]
  annotations_discarded<-annotations[badRows,]
  lista<-list(annotations_cleaned=annotations_cleaned,annotations_discarded=annotations_discarded)
  return(lista)
  }