#' @export
geom_tsne <- function(...){
  list(
    dims_expand(),
    geom_tsne0(...)
  )
}

#' @export
geom_tsne_label <- function(...){
  list(
    dims_expand(),
    geom_tsne_label0(...)
  )
}
