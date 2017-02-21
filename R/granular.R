utils::globalVariables(".")
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
#' @param mu_vec A vector defining distribution means (required)
#' @param pi_vec A vector defining distribution proportions (optional)
#' @param sigma_vec A vector defining distribution dispersion (optional)
#' @param peak_names A vector defining peak names (optional)
#' @param sample_name A character string defining the sample name (optional)
#' @param emnum passed to mix() - A non-negative integer specifying the number of EM steps to be performed
#' @param log_trans Logical. Should values be log-transformed?
#' @return A list with the fit parameters for each distribution, and complete output from mixdist::mix()
#' @export
mix_dist <- function(dist,
                     ps, 
                     mu_vec, 
                     pi_vec = NULL, 
                     sigma_vec = NULL, 
                     peak_names = NULL, 
                     sample_name = NULL,
                     emnum=10,
                     log_trans=TRUE) {
  if (is.null(mu_vec))
    stop("ERROR: There were no component means supplied")
  if (!is.numeric(mu_vec))
    stop("ERROR: mu_vec (component means) supplied is non-numeric")
  if (length(ps) != length(dist)) 
    stop("ERROR: The particle size and the distribution are not the same length") 
  if (!is.numeric(ps))
    stop("ERROR: Non-numeric value in the particle size.")
  if (!is.numeric(dist))
    stop("ERROR: The distribution is not numeric")
  if (!all(ps>=0))
    stop("ERROR: Negative particle size value.")
  if (!all(dist>=0))
    stop("ERROR: Negative distribution value.")
  if(!identical(mu_vec, mu_vec[order(mu_vec)]))
    stop("ERROR: mu_vec needs to be in ascending order")
  
  #Get number of peaks
  ncomp <- length(mu_vec)
  
  #Check _vec names
  names_list <- list(
    names(mu_vec),
    names(pi_vec),
    names(sigma_vec),
    peak_names
  )
  
  names_disagreement <- lapply(names_list, function(x) {
    setdiff(x, names_list[[1]])
  })
  
  if(any(lapply(names_disagreement, length) > 0))
    stop("ERROR: There is disagreement in the peak names")
  
  if(all(lapply(names_list, length) == 0)) {
    peak_names <- paste0("peak_", seq_len(ncomp))
    message(paste("No names supplied for means, providing default names:", 
                  peak_names))
  } else {
    peak_names <- names_list[[which.max(lapply(names_list, length))]]
  }
  
  if(log_trans) ps <- log(ps)
  index_start <- min(which(dist!=0)) # to remove trailing and leading 0 entries.
  index_end <- max(which(dist!=0))
  dat <- data.frame("size"=ps[index_start:index_end],
                    "rfreq"=dist[index_start:index_end])
  
  #calculate initial parameters
  if(is.null(pi_vec)) {
    if(log_trans) {
      heights_d <- get_heights(dat[["rfreq"]], dat[["size"]], log(mu_vec))
    } else heights_d <- get_heights(dat[["rfreq"]], dat[["size"]], mu_vec)
    pi_vec <- heights_d/(sum(heights_d))
  }
  
  if(is.null(sigma_vec)) {
    sigma_vec <- rep(diff(range(dat$size))/ncomp, times = ncomp)
  }
  
  if(log_trans) {
    mixpar <- mixdist::mixparam(log(mu_vec), sigma_vec, pi_vec)
  } else mixpar <- mixdist::mixparam(mu_vec, sigma_vec, pi_vec)
  
  
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
#' @param mu_vec A vector defining distribution means (required)
#' @param pi_vec A vector defining distribution proportions (optional)
#' @param sigma_vec A vector defining distribution dispersion (optional)
#' @param peak_names A vector defining peak names (optional)
#' @param emnum passed to mix() - A non-negative integer specifying the number of EM steps to be performed
#' @param size An unquoted variable name
#' @param log_trans Logical. Should values be log-transformed?
#' @param parallel Logical. Should multidplyr be used to run in parallel? (EXPERIMENTAL)
#'
#' @return A mutated tbl with list column output
#' @export
mix_grp_tbl <- function(.data,
                        proportion, 
                        size, 
                        mu_vec, 
                        pi_vec = NULL, 
                        sigma_vec = NULL, 
                        peak_names = NULL, 
                        emnum = 10,
                        log_trans = TRUE,
                        parallel = FALSE) {
  proportion_col <- deparse(substitute(proportion))
  size_col <- deparse(substitute(size))
  if(length(dplyr::group_size(.data)) < 2) warning(paste("There is only one group - check data groupings"))
  if(!parallel) {
    out <- dplyr::do(.data, 
                     mix_out = mix_dist(dist = .[[proportion_col]],
                                        ps = .[[size_col]],
                                        mu_vec = mu_vec,
                                        pi_vec = pi_vec,
                                        sigma_vec = sigma_vec,
                                        emnum = emnum,
                                        log_trans = log_trans,
                                        peak_names = peak_names)[[1]],
                     proportion = list(.[[proportion_col]]),
                     size = list(.[[size_col]])
    )
  }
  
  if(parallel) {
    if(!requireNamespace("multidplyr", quietly = TRUE)) stop("package multidplyr is not installed")
    
    cluster <- multidplyr::create_cluster()
    
    groups <- lazyeval::as.lazy_dots(dplyr::groups(.data))
    
    clust_dat <- multidplyr::partition_(.data, groups, cluster = cluster)
    multidplyr::cluster_library(clust_dat, "granular")
    multidplyr::cluster_assign_value(clust_dat, "mu_vec", mu_vec)
    multidplyr::cluster_assign_value(clust_dat, "pi_vec", pi_vec)
    multidplyr::cluster_assign_value(clust_dat, "sigma_vec", sigma_vec)
    multidplyr::cluster_assign_value(clust_dat, "peak_names", peak_names)
    multidplyr::cluster_assign_value(clust_dat, "proportion_col", proportion_col)
    multidplyr::cluster_assign_value(clust_dat, "size_col", size_col)
    multidplyr::cluster_assign_value(clust_dat, "log_trans", log_trans)
    multidplyr::cluster_assign_value(clust_dat, "emnum", emnum)
    
    clust_mix <- dplyr::do(clust_dat, 
                           mix_out = mix_dist(dist = .[[proportion_col]],
                                              ps = .[[size_col]],
                                              mu_vec = mu_vec,
                                              pi_vec = pi_vec,
                                              sigma_vec = sigma_vec,
                                              emnum = emnum,
                                              log_trans = log_trans,
                                              peak_names = peak_names)[[1]],
                           proportion = list(.[[proportion_col]]),
                           size = list(.[[size_col]])
    )
    
    out <- tibble::as_tibble(dplyr::collect(clust_mix))
    
  }
  
  return(out)
}