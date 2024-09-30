iperparametersChange<-function( ){
  
  
  myLabel<-("Change default settings? No is default.")
  input<-NULL
  while (is.null(input)){
    input<-shinyBooleanInput(myLabel)
  }
  ans<-input
  
  if (ans){ 
    
    # bF_threshold
    myLabel<-("Let's apply the 'batch filter'? If so, insert required 
              average number of reads per sample")
    input<-NULL
    while (is.null(input)){
      input<-shinyNumInput(myLabel)
    }
    bF_threshold<-input
    
    # cF_threshold
    myLabel<-("Let's apply the 'pheno filter'? If so, insert required 
              average number of reads for paired 'cell line-condition'")
    input<-NULL
    while (is.null(input)){
      input<-shinyNumInput(myLabel,value=10)
    }
    cF_threshold<-input
    
    # flag_shrinkage
    myLabel<-("Have you favourite shrinkage method to apply? If none, push the button")
    input<-NULL
    while (is.null(input)){
      input<-shinyTextInput(myLabel, value="none")
    }
    flag_shrinkage<-input
    if (!(flag_shrinkage %in% c("normal","ashr","apeglm") )){
      flag_shrinkage<-"none"
    }
    
    # padj_max
    myLabel<-("Padj lower than from <0.05? Insert different threshold then.
              Padj is p-value after Benjamini-Hochberg correction for multiple testing")
    input<-NULL
    while (is.null(input)){
      input<-shinyNumInput(myLabel,value=0.05)
    }
    padj_max<-input
    if (padj_max>0.05) { padj_max<-0.05 }
    
    #  FC_min
    myLabel<-("DE features selected among statistically significant features 
              by calculating top 5% of FC values? FC>2 is default")
    input<-NULL
    while (is.null(input)){
      input<-shinyBooleanInput(myLabel)
    }
    FC_min<-input
    if (FC_min) { FC_min<-"calculated" } else { FC_min<-2 }
    
    

    lista<-list(flag_shrinkage,padj_max,FC_min,bF_threshold,cF_threshold)
    return(lista)
    
  } else {return(NULL)}
  
}