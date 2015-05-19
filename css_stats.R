library(lmtest)
library(Hmisc)
library(foreach)
library(sandwich)

# code by dean eckles and eytan bakshy

sandwich.lm <- function(fit) {
 bread.value <- bread(fit)

 # get the meat
 psi <- as.matrix(estfun(fit))
 weights <- fit$weights
 m <- NROW(psi)
 # psi is already multiplied times the weights
 rval <- t(psi) %*% (psi / weights) / m^2
 rownames(rval) <- colnames(rval) <- colnames(psi)
 meat.value <- rval

 bread.value %*% meat.value %*% bread.value
}

# weighted linear regression coeftest with sandwich standard errors
weighted.lm.coeftest <- function(formula, data, ...) {
  m <- lm(formula, data=data, weights=.weights, ...)
  coeftest(m, vcov=sandwich.lm)
}


## the bootstrap
r.double.or.nothing <- function(n) {
  2 * rbinom(n, 1, .5)
}

clustered.bootstrap <- function(data, clusters, statistic, .R=250,
                    .verbose = FALSE,
                    .RNG = r.double.or.nothing,
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
    statistic(mutate(data, .weights=w, .r=r), ...)
  }
  replicates
}

iid.bootstrap <- function(data, statistic, .R=250,
  .RNG = r.double.or.nothing, .combine=c, ...) {
   replicates <- foreach(r=1:.R, .combine=.combine) %do% {
   	statistic(mutate(data, .weights=.RNG(nrow(data)), .r=r), ...)
   }
   replicates
 }

norm.intervals <- function(x, alpha=0.05) {
  data.frame(
	lower=qnorm(alpha/2, mean(x), sd(x)),
	mean=mean(x),
	upper=qnorm(1-alpha/2, mean(x), sd(x)))
}
