generaHeatmap<-function(filePath, M,w,h,res, f_row,f_col,ft_col,ft_row,fl,metric,
                  clusteringMethod, splitMethod, rowS, cluster_columns, 
                  titolo, legenda, col_fun=NULL, column_ha =NULL, 
                  row_ha=NULL,gap=1){
  
  tiff(filePath, width=w, height=h,res=res)
  print(
    Heatmap(M, col= col_fun,
                name = legenda, #title of legend
                column_title = titolo, row_title = "Features",
                column_split =  splitMethod, 
                column_gap = unit(gap, "mm"), use_raster=TRUE,
                clustering_distance_rows=metric,
                clustering_method_rows = clusteringMethod,
                row_names_gp = gpar(fontsize = f_row),
                column_names_gp = gpar(fontsize = f_col), # Text size for row names
                heatmap_legend_param = list(title_gp =gpar(fontsize = fl) ,
                                            labels_gp=gpar(fontsize = fl) ),
                column_title_gp = gpar(fontsize = ft_col),
                row_title_gp = gpar(fontsize = ft_row),
                row_split =rowS, cluster_columns = cluster_columns,
                top_annotation = column_ha, left_annotation = row_ha
   )
  )
  dev.off()
}