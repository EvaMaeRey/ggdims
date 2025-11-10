# compute_tsne allows individually listed variables that are all of the same type
#' @export
compute_tsne <- function(data, scales, perplexity = 20){
  
data_for_reduction <- data_vars_unpack(data)

dups <- data_for_reduction |>
   duplicated()

clean_data <- data_for_reduction |>
    bind_cols(data) |>
     _[!dups,] |> 
  remove_missing()

set.seed(1345)
clean_data |>
  _[names(data_for_reduction)] |>
  as.matrix() |>
  Rtsne::Rtsne(perplexity = perplexity) |>
  _$Y |>
  as_tibble() |>
 rename(x = V1, y = V2) |>
 bind_cols(clean_data)

}

#' @export
compute_tsne_group_label <- function(data, scales, perplexity = 20, fun = mean){
  
  compute_tsne(data, scales, perplexity) |> 
    summarise(x = fun(x),
              y = fun(y),
              .by = label)
  
}

#' @export
StatTsne <- ggproto("StatTsne", Stat, 
                     compute_panel = compute_tsne)

#' @export
StatTsneGroup <- ggproto("StatTsneGroup", Stat, 
                         compute_panel = compute_tsne_group_label)

GeomPointFill <- ggproto("GeomPointFill", 
                         GeomPoint,
                         default_aes = 
                           modifyList(GeomPoint$default_aes, 
                                      aes(shape = 21, 
                                          color = from_theme(paper),
                                          size = from_theme(pointsize * 1.5),
                                          alpha = .7,
                                          fill = from_theme(ink))))

#' @export
geom_tsne0 <- make_constructor(GeomPointFill, 
                               stat = StatTsne, 
                               perplexity = 30)

#' @export
geom_tsne_label0 <- make_constructor(GeomText, 
                                     stat = StatTsneGroup,
                                     perplexity = 30)
