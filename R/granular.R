
#' Get the heights at peaks
#'
#' @param dist A numeric vector defining the distribution
#' @param ps A numeric vector describing the granule sizes
#' @param means A named numeric vector defining the means (center) for each peak 
#'
#' @return A named vector with heights for each mean
get_heights <- function(dist, ps, means) {
  if(!is.numeric(dist)) stop("dist is not numeric")
  if(!is.numeric(ps)) stop("ps is not numeric")
  if(!is.numeric(means)) stop("means are not numeric")
  heights_out <- lapply(means, function(x) {
    dist[which.min(abs(ps - x))]
  })
  return(unlist(heights_out))
}

#' use mix() to estimate underlying distributions
#'
#' @param dist A numeric vector defining the distribution
#' @param ps A numeric vector describing the granule sizes
#' @param comp_means A named numeric vector defining the means (center) for each peak
#' @param sample_name An optional name for the sample
#' @param emnum passed to mix() - A non-negative integer specifying the number of EM steps to be performed
#' @param mixpar Optional - a data.frame defining mixpar from mixdist::mix()
#'
#' @return A list with the fit parameters for each distribution, and complete output from mixdist::mix()
#' @export
mix_dist <- function(dist,
                     ps, 
                     comp_means = NULL,
                     sample_name = NULL,
                     emnum=10,
                     mixpar = NULL
) {
  if (all(is.null(c(comp_means, mixpar))))
    stop("ERROR: There were no component means or mixpar supplied")
  if (!is.null(comp_means) & !is.numeric(comp_means))
    stop("ERROR: comp_means is non-numeric")
  if (length(ps) != length(dist)) 
    stop("ERROR: The particle size and the distribution is not correct.") 
  if (!is.numeric(ps))
    stop("ERROR: Non-numeric value in the particle size.")
  if (!is.numeric(dist))
    stop("ERROR: The distribution is not numeric")
  if (!all(ps>=0))
    stop("ERROR: Negative particle size value.")
  if (!all(dist>=0))
    stop("ERROR: Negative distribution value.")
  
  if(!is.null(mixpar)) {
    ncomp <- nrow(mixpar)
    peak_names <- paste("peak", LETTERS[1:ncomp], sep = "_")
  }
  
  if(is.null(mixpar)) {
    comp_means <- comp_means[order(comp_means)]
    
    ncomp <- length(comp_means)
    
    if(is.null(names(comp_means))) {
      names(comp_means) <- paste0("peak_", seq_len(ncomp))
      message(paste("No names supplied for means, providing default names:", 
                    names(comp_means)))
    }
    
    peak_names <- names(comp_means)
    
    #calculate initial parameters
    heights_d <- get_heights(dat[["rfreq"]], dat[["log_size"]], log(comp_means))
    comp_weights <- heights_d/(sum(heights_d))
    
    comp_sds <- rep(diff(range(dat$log_size))/ncomp, times = ncomp)
    
    mixpar <- mixdist::mixparam(log(comp_means), comp_sds, comp_weights)
  }
  
  log_ps <- log(ps)
  index_start <- min(which(dist!=0)) # to remove trailing and leading 0 entries.
  index_end <- max(which(dist!=0))
  dat <- data.frame("log_size"=log_ps[index_start:index_end],
                    "rfreq"=dist[index_start:index_end])
  
  mixFit <- mixdist::mix(dat, 
                         mixpar, 
                         emsteps=emnum)
  
  if(!is.null(sample_name)) {
    sample <- rep(sample_name, ncomp)
    theFit <- cbind(sample, peak = peak_names, mixFit$parameters, mixFit$se)
  } else {
    theFit <- cbind(peak = peak_names, mixFit$parameters, mixFit$se)
  }
  return(list(theFit, mixFit))		
}

#' A function to test how well the fit matches the data
#' 
#' @param fit_output A data frame output from mix_dist()
#' @param dist A numeric vector defining the distribution
#' @param ps A numeric vector describing the granule sizes
#'
#' @return a ggplot object
#' @export
check_fit <- function(fit_output, dist, ps) {
  fit_dist <- make_dist(fit_output, ps)
  fit_dist <- fit_dist/(sum(fit_dist))
  dist <- dist/sum(dist)
  df <- data.frame(ps, fit_dist, dist)
  ss <- sum((df$fit_dist - df$dist)^2)
  return(ss)
}

#' Title
#'
#' @param .data A tbl grouped by each distribution
#' @param proportion An unquoted variable name
#' @param size An unquoted variable name
#' @param comp_means A named vector defining the means of each component
#'
#' @return A mutated tbl with list column output
#' @export
mix_grp_tbl <- function(.data, proportion, size, comp_means = NULL, emnum = 10, mixpar = NULL) {
  proportion <- lazyeval::lazy(proportion)
  size <- lazyeval::lazy(size)
  if(all(class(.data) != "party_df")){
    if(length(dplyr::group_size(.data)) < 2) warning(paste("There is only one group - check data groupings"))
  }
  out <- purrr::by_slice(.data, ~ mix_dist(dist = lazyeval::lazy_eval(proportion, .),
                                           ps = lazyeval::lazy_eval(size, .),
                                           comp_means = comp_means,
                                           emnum = emnum,
                                           mixpar = mixpar)[[1]],
                         .to = "mix_out"
  )
}