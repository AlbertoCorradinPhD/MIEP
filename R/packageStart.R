

.onAttach <- function(lib, pkg) {
  ## some preprocessing
  where <- match(paste("package:", pkg, sep=""), search())
  myGroupGOTerms(where)
}
