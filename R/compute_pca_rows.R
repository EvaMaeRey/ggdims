#' @export
compute_pca_rows <- function(data, scales){
  
  data_for_reduction <- data_vars_unpack(data)

clean_data <- data_for_reduction |>
  bind_cols(data) |>
  remove_missing()

set.seed(1345)
reduced <- clean_data |>
  _[names(data_for_reduction)] |>
  ordr::ordinate(model = ~ prcomp(., scale. = TRUE)) |> 
  _[[5]] |> 
  as_tibble()

reduced |>
 bind_cols(clean_data)

}

#' @export
StatPcaRows <- ggproto("StatPcaRows", Stat,
                    compute_panel = compute_pca_rows,
                    default_aes = aes(x = after_stat(PC1), 
                                      y = after_stat(PC2))
                    )
#' @export
geom_pca0 <- make_constructor(GeomPointFill, stat = StatPcaRows)

#' @export
geom_pca <- function(...){
  
  list(
    dims_expand(),
    geom_pca0(...)
  )
  
}
