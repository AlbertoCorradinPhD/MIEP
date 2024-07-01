seqsOfValues<-function(flag){
  
  #uguale per tutti
  ntrees_seq<-c(100,500)
  
  if (flag=="findBest"){#24 livelli
    #short case for test
    pval_seq<-seq(from = 0.05, to = 0.5, by =0.25)
    testtype_seq<-c("Bonferroni", #"MonteCarlo",
                    "Univariate", "Teststatistic"
    )
    teststat_seq<-c("quad", "max")
  }
  if (flag=="launchCP"){#228 livelli
    #true complete run
    pval_seq<-seq(from = 0.05, to = 0.5, by =0.025)
    testtype_seq<-c("Bonferroni", #"MonteCarlo",
                    "Univariate", "Teststatistic"
    )
    teststat_seq<-c("quad", "max")
  }
  if (flag=="testCP"){
    #for test
    pval_seq<-seq(from = 0.05, to = 0.5, by =0.25)
    testtype_seq<-c( "Teststatistic")
    teststat_seq<-c("quad")
  }
  levels <- list(pval = pval_seq, testtype = testtype_seq, 
                 teststat=teststat_seq, ntrees=ntrees_seq)
  return(levels)
}
