library(mixdist)
library(dplyr)
library(tidyr)
library(purrr)

#' Get the heights at peaks
#'
#' @param dist A numeric vector defining the distribution
#' @param ps A numeric vector describing the granule sizes
#' @param means A named numeric vector defining the means (center) for each peak 
#'
#' @return
#'
#' @examples
heights <- function(dist, ps, means) {
  heights_out <- lapply(means, function(x) {
    dist[which.min(abs(ps - x))]
  })
  return(unlist(heights_out))
}

mixDist <- function(ps, 		# ps: vector of the particle size bin values
                    Dist, 		# Dist: the relative frequency in corresponding bin
                    ncomp=3,
                    comp_ids = c("A", "B", "C"),
                    comp_means,
                    # comp_sds = NULL,
                    # comp_weights = NULL,
                    # initial_values=mixdist::mixparam(data.frame("pi"=c(0.02344, 0.39337, 0.58319), 
                    #                           "mu"=c(-0.303, 1.843, 3.059), 
                    #                           "sigma"=c(0.2675, 0.9121, 0.4002)),
                    printFit=TRUE,
                    printPlot=TRUE,
                    emnum=5
) {
  # checks
  if (is.null(comp_means))
    stop("ERROR: There were no component means supplied")
  if (length(ps) != nrow(Dist)) 
    stop("ERROR: The particle size and the distribution is not correct.") 
  if (!is.numeric(ps))
    stop("ERROR: Non-numeric value in the particle size.")
  if (!all(apply(Dist, 2, is.numeric)))
    stop("ERROR: Non-numeric value in the distribution.")
  if (!all(ps>=0))
    stop("ERROR: Negative particle size value.")
  if (!all(Dist>=0))
    stop("ERROR: Negative distribution value.")
  
  log_ps <- log(ps)
  nline <- ncol(Dist)	
  returnFit <- NULL
  for (name in names(Dist)) {
    rfreq <- Dist[[name]]
    index_start <- min(which(rfreq!=0)) # to remove trailing and leading 0 entries.
    index_end <- max(which(rfreq!=0))
    dat <- data.frame("log_size"=log_ps[index_start:index_end],
                      "rfreq"=rfreq[index_start:index_end])
    
    #calculate initial parameters
    heights_d <- heights(dat$log_size, dat$rfreq, log(comp_means))
    print(heights_d)
    comp_weights <- heights_d/(sum(heights_d))
    print(comp_weights)
    
    spread <- max(dat$log_size) - min(dat$log_size)
    comp_sds <- rep(spread/ncomp, times = ncomp)
    comp_sds <- setNames(comp_sds, names(comp_means))
    
    initial_values <- mixdist::mixparam(log(comp_means), comp_sds, comp_weights)
    
    print(paste("these are the initial_values", initial_values))
    mixFit <- mix(dat, 
                  initial_values, 
                  emsteps=emnum)
    if (printPlot) {
      par(mar=c(4, 6, 1, 1) + 0.1, ask=TRUE)
      plot(mixFit, 
           xlab=expression(paste("Log of particle size diameter (", mu, "m)")), 				 cex=2, cex.axis=2, cex.lab=2,
           main=paste("Fit", name))
    }
    if (printFit) {
      print(paste("Fit", name))
      print(mixFit)
    }
#    browser()
    line <- rep(name, ncomp)
    theFit <- cbind(line, peak = names(comp_means), mixFit$parameters, mixFit$se)
    returnFit <- rbind(returnFit, theFit)
  }
  return(returnFit)		
}