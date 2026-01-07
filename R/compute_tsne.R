tsne_layout_2d <- function(data, perplexity){
  
  data |> 
    as.matrix() |>
    Rtsne::Rtsne(perplexity = perplexity) |>
    _$Y |>
    as_tibble() |>
    rename(x = V1, y = V2)
  
}


# compute_tsne allows individually listed variables that are all of the same type
#' @export
compute_tsne <- function(data, scales, perplexity = 20){
  
  features <- data_vars_unpack(data)
  non_feature_data <- data |> dplyr::select(-dims)

  # allowable for dimred
  ind_not_dup <- !duplicated(features)
  ind_no_missing <- complete.cases(features)
  
  ind_allowed <- ind_not_dup & ind_no_missing

  # clean_data <- 
 
  set.seed(1345)
  features |>
    _[ind_allowed, ] |>
    tsne_layout_2d(perplexity = perplexity)  |>
    bind_cols(non_feature_data |>
                bind_cols(features) |>
                _[ind_allowed, ])

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



#' @export
geom_tsne0 <- make_constructor(GeomPointFill, 
                               stat = StatTsne, 
                               perplexity = 30)

#' @export
geom_tsne_label0 <- make_constructor(GeomText, 
                                     stat = StatTsneGroup,
                                     perplexity = 30)
