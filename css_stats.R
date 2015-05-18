library(lmtest)
library(Hmisc)
library(foreach)

# code by dean eckles and eytan bakshy

freq.weighted.vcov <- function (object, vcov. = vcov, freq = NA, ...) 
{
    if (missing(freq)) {
        freq <- object$weights
    }
    if (class(vcov) == "function") {
        vcov. <- vcov.(object, ...)
    }
    vcov. * length(freq)/sum(freq)
}
coeftest.freq <- function (object, vcov. = vcov, freq = NA, df = NA, ...) 
{
    if (!require(lmtest)) {
        stop("This requires the 'lmtest' package.")
    }
    if (missing(freq)) {
        freq <- object$weights
    }
    if (is.na(df)) {
        df <- sum(freq) - (length(freq) - df.residual(object))
    }
    coeftest(object, freq.weighted.vcov(object, vcov., freq, 
        ...), df)
}

## the bootstrap
r.double.or.nothing <- function(n) {
  2 * rbinom(n, 1, .5)
}

clustered.bootstrap <- function(data, clusters, statistic, .R=250,
                    .verbose = FALSE,
                    .RNG = r.double.or.nothing,
                    .progress = "none",
                    .parallel = FALSE,
                    .combine=c,
                    ...) {
  if(length(clusters) == 0) {
  	return(iid.bootstrap(data, statistic, .R, .RNG, ...))
  }
  # lists containing information for each group
  cluster.ids <- list()
  num.clusters <- list()
  for(i in clusters) {
    groups.factor <- as.factor(data[[i]])
    cluster.ids[[i]] <- as.numeric(groups.factor)
    num.clusters[[i]] <- length(levels(groups.factor))
  }

  replicates <- foreach(r=1:.R, .combine=.combine) %do% {
    # generate and multiply weight vectors
    w <- foreach(i = clusters, .combine = `*`) %do%
      {.RNG(num.clusters[[i]])[cluster.ids[[i]]]}
    statistic(mutate(data, .weights=w), ...)
  }
  replicates
}

iid.bootstrap <- function(data, statistic, .R=250,
  .RNG = r.double.or.nothing) {
   replicates <- foreach(r=1:.R, .combine=c) %do% {
   	statistic(mutate(data, .weights=.RNG(nrow(data))), ...)
   }
   replicates
 }

norm.intervals <- function(x, alpha=0.05) {
  data.frame(
	lower=qnorm(alpha/2, mean(x), sd(x)),
	mean=mean(x),
	upper=qnorm(1-alpha/2, mean(x), sd(x)))
}
