generaHeatmapsFC<-function(geneList, geneset, inDir, outDir, 
                            splitMethod, rowS){
  
  
  #cambia outDir
  outDir<-file.path(outDir,"heatmaps")
  dir.create(outDir, showWarnings = FALSE, recursive = TRUE)
  
  #read tables
  filePath<-file.path(inDir,"tables.rds")
  lista<-readRDS(file=filePath)
  tableFConly<-lista$tableFConly
  numberOfFeatures<-dim(tableFConly)[1]
  nomiColonne<-lista$nomiColonne[-1]
  
  nomiColonne<-sapply(X=nomiColonne, FUN=parseName,simplify = TRUE)
  names(nomiColonne)<-nomiColonne
  
  tableDEonly<-lista$tableDEonly
  annColonne<-sapply(X=tableDEonly[,-1], FUN=sum,simplify = TRUE)
  trasposta<-as.data.frame(t(tableDEonly[,-1])) #sapply opera per colonne
  colnames(trasposta)<-tableDEonly[,1]
  annRighe<-sapply(X=trasposta, FUN=sum,simplify = TRUE)
 
  #escape case
  if (numberOfFeatures<1){
    print("nessun gene del geneset tra i differenzialmente espressi")
    return()
  }
  
  ###############################################################################
  ##### SETTINGS
  ###############################################################################
  
  #graphical settings
  listaPars<-settingsXheatmaps(numberOfFeatures)
  w<-listaPars[[1]]
  h<-listaPars[[2]]
  res<-listaPars[[3]]
  f_row<-listaPars[[4]]
  f_col<-listaPars[[5]]
  ft_col<-listaPars[[6]]
  ft_row<-listaPars[[7]]
  fl<- listaPars[[8]]
  metric<-listaPars[[9]]
  clusteringMethod<-listaPars[[10]]
  rm(listaPars)
  
  #PREPARA LA MATRICE DI DATI
  X<-2^as.matrix(tableFConly[,-1])
  colnames(X)<-nomiColonne
  row.names(X)<-tableFConly$ID_REF
  
  #########################################################################
  ### HEATMAPS
  #########################################################################
  
  titolo<-paste("Heatmap of Fold Changes in geneset:", geneset)
  titolo<-parseName(parola=titolo)
  legenda<-"Fold Change"

  
  
  
  #annotazione colonne
  names(annColonne)<-nomiColonne
  pch <- rep("Y", length(annColonne))
  pch[annColonne==0]<-"N"
  colore<-colorRamp2(c(0, 10), c("white","green")) 
  tagliaC<-dim(X)[2]/4
  myUnit<-unit(10/(numberOfFeatures^(1/4)), "mm")
  anno_bar<-anno_simple(x=annColonne, which = , pch = pch, col=colore, 
                        pt_size=unit(1, "snpc")*0.8,
                        pt_gp=gpar(fontsize =f_col))
  column_ha = HeatmapAnnotation(
    DE=anno_bar, height=myUnit/tagliaC,
    annotation_name_gp = gpar(fontsize=ft_col),
    annotation_name_side = "right",show_legend = FALSE,
    show_annotation_name = TRUE
  )
  
  #annotazioni righe
  pch <- rep("Y", length(annRighe))
  pch[annRighe==0]<-"N"
  colore<-colorRamp2(c(0, 10), c("white","green")) 
  tagliaR<-0.01*numberOfFeatures+1
  anno_bar<-anno_simple(x=annRighe, which = "row", pch = pch, col=colore,
                        pt_size=unit(1, "snpc")/tagliaR,
                        pt_gp=gpar(fontsize =f_row))
  row_ha = rowAnnotation( DE=anno_bar, width=myUnit/tagliaC,
                          annotation_name_gp = gpar(fontsize=ft_col),
                          show_legend = FALSE, show_annotation_name = TRUE)
  
  #heatmap classic
  filePath<-file.path(outDir,"heatmapFC_classic.tiff")
  suppressMessages(
    generaHeatmap(filePath, M=X,w,h,res, f_row,f_col,ft_col,ft_row,fl,
          metric,clusteringMethod,splitMethod=nomiColonne,
          rowS, cluster_columns=FALSE, titolo,legenda,
          column_ha=column_ha, row_ha=row_ha
          )
  )
  
 
  #heatmap custom colors
  col_fun<-customColors_FC()
  filePath<-file.path(outDir,"heatmapFC_customColors.tiff")
  generaHeatmap(filePath, M=X,w,h,res, f_row,f_col,ft_col,ft_row,fl,metric,
          clusteringMethod, splitMethod=nomiColonne,
          rowS, cluster_columns=FALSE, titolo, legenda, 
          column_ha=column_ha, row_ha=row_ha,
          col_fun=col_fun)#evito split colonne


}