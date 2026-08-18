#' @export
dims_listed <- function(...) {
  
  varnames <- as.character(ensyms(...))
  vars <- list(...)
  listvec <- asplit(do.call(cbind, vars), 1)
  structure(listvec, varnames = varnames)

  }

#' @export
vars_unpack <- function(x) {
  pack_vars <- x
  df <- do.call(rbind, pack_vars)
  colnames(df) <- attr(pack_vars, "varnames")
  as.data.frame(df)
  
}
