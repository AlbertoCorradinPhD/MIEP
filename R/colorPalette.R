#' @importFrom RColorBrewer brewer.pal brewer.pal.info

colorPalette<-function(){
  n <- 60
  qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
  col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
  OS<-Sys.info()['sysname']
  #if (OS=="Linux") {X11()} else {win.graph()}
  print(pie(rep(1,n), col=sample(col_vector, n), radius = 1))
  return(col_vector)

}
