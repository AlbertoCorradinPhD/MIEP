#' @importFrom DBI dbGetQuery 
#' @importFrom GO.db GO_dbconn

myGroupGOTerms <- function(where) {
  
  if(missing(where))
    where <- .GlobalEnv
  where <- as.environment(where)
  
  sql <- "SELECT go_id FROM go_term WHERE ontology IN"
  for(onto in c("BP", "MF", "CC")) {
    xx <- dbGetQuery(GO_dbconn(), paste(sql, "('", onto, "');", sep = ""))$go_id
    e <- new.env(hash = T, parent = emptyenv())
    multiassign(xx, value = rep(TRUE, length(xx)), envir = e)
    assign(paste("GO", onto, "Term", sep = ""), e, envir = where)
  }
  
}
