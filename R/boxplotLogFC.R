boxplotLogFC<-function(inDir, outDir,table1, table2, consideredCase, cellLines, confronti){
  
  #######################################################################
  ### ALL SELECTED SEQUENCES
  ######################################################################
  testTable<-table1
  #check the file in order to select right columns
  #colonne<-c(4,6,8,10,12,14,16,18)
  if (consideredCase=="union")  {
    endCol<-2+2*length(cellLines)*length(confronti)-1
    colonne<-seq(4,endCol,2)
  } else  {
    endCol<-2+2*1*length(confronti)
    colonne<-seq(3,endCol,2)
  }

  df<-as.data.frame(cbind(testTable[,colonne]))
  names(df)<-sub(".log2FoldChange","",x=names(df))
  df.m<-c()
  M<-dim(df)[2]
  for (i in 1:M){
    df.m<-c(df.m,df[,names(df)[i]])
  }
  
  Labels<-c()
  N<-dim(df)[1]
  for (i in 1:M){
    Labels<-c(Labels,rep(names(df)[i],N))
  }
  df.m<-cbind(as.data.frame(df.m),as.data.frame(Labels))
  names(df.m)
  names(df.m)[1]<-"log2FC"
  df_all<-df.m
  
  ########################################################################
  ##### "DEonly" SEQUENCES
  ########################################################################
  
  testTable<- table2
  df<-as.data.frame(cbind(testTable[,colonne]))
  names(df)<-sub(".log2FoldChange","",x=names(df))
  df.m<-c()
  M<-dim(df)[2]
  for (i in 1:M){
    df.m<-c(df.m,df[,names(df)[i]])
  }
  
  Labels<-c()
  N<-dim(df)[1]
  for (i in 1:M){
    Labels<-c(Labels,rep(names(df)[i],N))
  }
  df.m<-cbind(as.data.frame(df.m),as.data.frame(Labels))
  names(df.m)
  names(df.m)[1]<-"log2FC"
  df_sig<-df.m
  
  ########## PLOT BOXPLOTS ###################################################
  boolean<- is.infinite(df_all$log2FC)
  #check
  sum(boolean)
  ymax<-max(df_all$log2FC[!boolean],na.rm=TRUE)
  ymin<-min(df_all$log2FC[!boolean],na.rm=TRUE)
  plot1<-ggplot(df_all, aes(x=factor(Labels),y=.data$log2FC)) + 
    geom_boxplot( outlier.size=1.5, outlier.shape=21)+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,size=13)) +
    stat_summary(fun="mean", geom="point", shape=23, size=3, fill="white") +
    ggtitle("Boxplot of Fold Changes (FCs, log scale) in selected features")+ 
    theme(plot.title = element_text(size=18))+
    ylim(ymin,ymax)+xlab("")
  plot2<-ggplot(df_sig, aes(x=factor(Labels),y=.data$log2FC)) + 
    geom_boxplot( outlier.size=1.5, outlier.shape=21)+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,size=13)) +
    stat_summary(fun="mean", geom="point", shape=23, size=3, fill="white")+
    ggtitle("Boxplot of FCs limitedly to tests where H1 is chosen")+ 
    theme(plot.title = element_text(size=18))+
    ylim(ymin,ymax)+xlab("")
  filename<-file.path(outDir,"boxplots_log2FC.png")
  png(filename, width=3500, height=2000,res=200)
  grid.arrange(plot1, plot2,nrow=1,ncol=2)
  dev.off()

}