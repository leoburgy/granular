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
  df <- data.frame(ps, fit_dist, dist) %>% 
    dplyr::mutate(log_ps = log(ps),
                  step_breaks = 
                    (log_ps + dplyr::lag(log_ps)) / 2)
  
  print(df)
  plot_out <- ggplot2::ggplot(df) +
    ggplot2::geom_step(ggplot2::aes(x = step_breaks, y = dist),
                       size = 1) +
    ggplot2::geom_line(ggplot2::aes(x = log_ps, y = fit_dist),
                       colour = "red", size = 0.6) +
    ggplot2::xlab("Log granule size (um)") +
    ggplot2::ylab("Proportion")
  
  if(!is.null(title)) {
    plot_out <- plot_out +
      ggtitle(title)
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
