#' A function for plotting a fit output against the true distribution
#'
#' @param fit_output A data frame output from mix_dist()
#' @param dist A numeric vector defining the distribution
#' @param ps A numeric vector describing the granule sizes
#' @param title Logical. Should a title be added to the plot?
#'
#' @return a ggplot object
#' @export
#' @importFrom magrittr %>%
ggfit <- function(fit_output, dist, ps, title = NULL) {
  fit_dist <- make_dist(fit_output, ps)
  fit_dist <- fit_dist/(sum(fit_dist))
  dist <- dist/sum(dist)
  df <- data.frame(ps, fit_dist, dist)
  df$log_ps <- log(df$ps)
  df$step_breaks <- (df$log_ps + dplyr::lag(df$log_ps)) / 2
  
  plot_out <- ggplot2::ggplot(df) +
    ggplot2::geom_step(ggplot2::aes_string(x = "step_breaks", y = "dist"),
                       size = 1) +
    ggplot2::geom_line(ggplot2::aes_string(x = "log_ps", y = "fit_dist"),
                       colour = "red", size = 0.6) +
    ggplot2::xlab(
      expression(paste("Log granule size (", mu, "m)"))) +
    ggplot2::ylab("Proportion")
  
  if(!is.null(title)) {
    plot_out <- plot_out +
      ggplot2::ggtitle(title)
  }
  
  return(plot_out)
}

#' Make a mixture distribution
#'
#' @param fit_output A data frame output from mix_dist()
#' @param ps A numeric vector describing the granule sizes
#'
#' @return A numeric vector describing the mixture distribution
#'
make_dist <- function(fit_output, ps) {
  dist_out <- fit_output$mu %>% 
    purrr::map2(fit_output$sigma, ~ dnorm(log(ps), .x, .y)) %>% 
    purrr::map2(fit_output$pi, ~ .x * .y) %>% 
    purrr::reduce(`+`)
  return(dist_out)
}

#' Generate fit plots for a grouped tbl
#'
#' @param .data A tbl output from mix_grp_tbl, with list columns
#' @param fit_output Variable name holding mix_dist_output
#' @param proportion Variable name holding distribution as a list
#' @param size Variable name holding particle size as a list
#'
#' @return The original tbl with a list column of ggplot output
#' @export
ggfit_grp_tbl <- function(.data, fit_output, proportion, size) {
  fit_col <- deparse(substitute(fit_output))
  proportion_col <- deparse(substitute(proportion))
  size_col <- deparse(substitute(size))
  if(length(dplyr::group_size(.data)) < 2 & nrow(.data) > 1) warning(paste("There is only one group - check data groupings"))
  if(is.null(dplyr::group_size(.data))) stop("ERROR: data is not grouped")
  # browser()
  out <- dplyr::do(.data, ggfit = ggfit(.[[fit_col]],
                                        dist = .[[proportion_col]][[1]],
                                        ps = .[[size_col]][[1]]
  ))
  out_df <- bind_cols(.data, out)
}