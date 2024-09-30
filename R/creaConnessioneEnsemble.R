creaConnessioneEnsemble<-function() {
  
  set_config(httr::config(ssl_verifypeer = FALSE)) #httr
  set_config(httr::config(ssl_cipher_list = "DEFAULT@SECLEVEL=1")) #httr
  #CREO CONNESSIONE
  mart<-NULL
  tryCatch(
    mart <- useMart("ensembl", dataset="hsapiens_gene_ensembl", host="https://useast.ensembl.org"),
    error=function(err) {}
  )
  if (is.null(mart)) {
    tryCatch(
      mart <- useMart("ensembl", dataset="hsapiens_gene_ensembl"),
      error=function(err) {}
    )
  } 
  if (is.null(mart)) {
    tryCatch(
      mart <- useMart("ensembl", dataset="hsapiens_gene_ensembl", host="https://asia.ensembl.org/"),
      error=function(err) {}
    )
  } 
  return(mart)
}