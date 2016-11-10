library(mixdist)
library(dplyr)
library(tidyr)
library(purrr)

#get heights at peaks
heights <- function(ps, Dist, peaks) {
  heights_out <- lapply(peaks, function(x) {
    Dist[which.min(abs(ps - x))]
  })
  return(unlist(heights_out))
}

make_dist <- function(x, means, sds, weights) {
  dists <- lapply(names(means), function(y) {
    dnorm(x, means[[y]], sds[[y]]) * weights[[y]]
    })
  names(dists) <- names(means)
  
  dists_df <- as.data.frame(dists)
  dists_df$sum <- rowSums(dists_df)
  dists_df$size <- x
  return(dists_df)
}

heights2weights <- function(heights) {
  heights/(sum(heights))
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
  for (iline in 1:nline) {
    rfreq <- Dist[[iline]]
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
           main=paste("Fit", iline))
    }
    if (printFit) {
      print(paste("Fit", iline))
      print(mixFit)
    }
    line <- rep(iline, ncomp)
    theFit <- cbind(line, mixFit$parameters, mixFit$se)
    returnFit <- rbind(returnFit, theFit)
  }
  return(returnFit)		
}