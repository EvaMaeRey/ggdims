#' @export
compute_umap <- function(data, scales, n_components = 2, random_state = 15){

data_for_reduction <- data_vars_unpack(data)

clean_data <- data_for_reduction |>
  bind_cols(data) |>
  remove_missing()

set.seed(1345)
clean_data |>
  _[names(data_for_reduction)] |>
  umap::umap(n_components = n_components,
             random_state = random_state)  |>
  _$layout |>
  as_tibble() |>
 rename(x = V1, y = V2) |>
 bind_cols(clean_data)

}

#' @export
StatUmap <- ggproto("StatUmap",
                    Stat,
                    compute_panel = compute_umap)

#' @export
geom_umap0 <- make_constructor(GeomPointFill, stat = StatUmap, random_state = 15, n_components = 4)

#' @export
geom_umap <- function(...){

  list(dims_expand(),
       geom_umap0(...))

}
