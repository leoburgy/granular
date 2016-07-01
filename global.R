library(mixdist)
#js function to override values parameter from ionrangeslider
#selection made on id

jsCode <- '
shinyjs.slideVals = function(params) {
var defaultParams = {
id : null,
vals : [0, 1]
};
params = shinyjs.getParams(params, defaultParams);

$("#" + params.id).data("ionRangeSlider").update({"values":params.vals});
}'

mixDist <- function(ps, 		# ps: vector of the particle size bin values
                    Dist, 		# Dist: the relative frequency in corresponding bin
                    ncomp=3, 
                    initial_values=data.frame("pi"=c(0.02344, 0.39337, 0.58319), 
                                              "mu"=c(-0.303, 1.843, 3.059), 
                                              "sigma"=c(0.2675, 0.9121, 0.4002)),
                    printFit=TRUE,
                    printPlot=TRUE,
                    emnum=5
) {
  # checks
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