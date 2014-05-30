
library('plyr')
library('dplyr')

library('ggplot2')
library(foreach)
library('lmtest')
library('sandwich')
library('Hmisc')
library('data.table')

# In an experiment we're interested in the difference of two averages.
# Our certainty about the value of an average depends on the number of
# samples we have. For example, consider the mean of a bernoulli random
# variable.

N <- 1e3
p <- 0.2
replications <- replicate(2000, mean(rbinom(N, 1, p)))
hist(replications, xlim=c(0,0.4))

# The above plot is called the sampling distribution.
# The standard error is the standard deviation of the sampling distribution.

# 95% of our estimates of p fall within this range:
quantile(replications, c(0.025, 0.975))

# for large sample sizes, we can approximate this using
se = sqrt(p*(1-p)/N)        # normal approximation
c(p - 1.96*se, p + 1.96*se) # norm approx 95% CI

# let's construct the confidence intervals for p for a few values of p, N

d <- expand.grid(p=c(0.05, 0.25, 0.5), n=seq(100, 1e4, 100))
d <- mutate(d, se=sqrt(p*(1-p)/n))
qplot(n, p, data=d, color=factor(p)) + geom_errorbar(aes(ymin=p-1.96*se, ymax=p+1.96*se))

qplot(n, se, data=d, color=factor(p))


N <- 1e5
ate <- 2
my.experiment <- data.frame(y0=rnorm(N, mean=10, sd=3), D=rbinom(N, 1, 0.5))
my.experiment <- mutate(my.experiment, y1=y0+ate, y=D*y1 + (1-D)*y0)
qplot(y, geom='histogram', data=my.experiment, facets=D~.)

options(digits=3)

N <- 1e6
mu <- -0.05

gdata <- data.frame(
  id=1:N,
  generosity=rnorm(N, mean=0.0, sd=0.02)
)

gdata <- mutate(gdata,
  num.friends=rnorm(N, 360, 20),
  gifts.received=rbinom(N, floor(0.015*num.friends/6),
    pnorm(mu + generosity + rnorm(N, 0, 0.01))),
  gift.utility=mu+generosity + 0.05*gifts.received + rnorm(N, 0, 0.01),
  gave.gift=ifelse(gift.utility>0, 1, 0)
)


hist(gdata$gifts.received)
hist(gdata$gift.utility)
mean(gdata$gifts.received)
mean(gdata$gave.gift)


summary(lm(gave.gift ~ I(gifts.received>0), data=gdata))

d <- gdata %>% group_by(gifts.received) %>%
  summarise(p=mean(gave.gift), n=n()) %>%
  mutate(se=sqrt(p*(1-p)/n))
d

qplot(gifts.received, p, data=d) + geom_pointrange(aes(ymin=p-1.96*se,ymax=p+1.96*se))

0.49-0.01 # naive.effect
0.49/0.011


## What is the effect of receiving a gift on the kinds of users who receive gifts?
# The experiment: prevent users from receiving gifts on Facebook when they
# have a friend who wants to buy them a gift

# use coeftest, sandwich SEs
summary(lm(like ~ num.shown, data=like.data))
summary(lm(share ~ num.sharing.friends, data= sharing.data))

summary(lm(share ~ factor(num.sharing.friends) - 1, data= sharing.data))

summary(lm(like ~ factor(num.shown), data=like.data))


n <- 1e5
prop.test(c(0.1117446, 0.1117446/1.1)*n, c(n,n ))

p1 <- 0.1117446
power.df <- expand.grid(p0=p1/c(1.01, 1.1, 1.5), n=seq(1e2, 1e4, 1e2))
power.df <- mutate(power.df, se=sqrt(p0*(1-p0)/n)) %>%
  do(function(df) {data.frame(p.value=with(df, prop.test(c(p1, p0)*n, c(n,n))$p.value))})
  
power.df <- foreach(i=1:nrow(power.df), .combine=rbind) %do% {
  mutate(power.df[i,], p.value=prop.test(c(p1, p0)*n, c(n,n))$p.value)
}

qplot(n, p.value, data=power.df, color=factor(p0), log='x') + geom_hline(aes(yintercept=c(0.05, 0.01)))


qplot(n, se, data=power.df, color=factor(p0), log='x') + geom_hline(aes(yintercept=p1-p0, color=factor(p0)))

qplot(n, se, data=power.df, color=factor(p0), log='x') + geom_pointrange(aes(ymin=p0



## the bootstrap
r.double.or.nothing <- function(n) {
  2 * rbinom(n, 1, .5)
}
boot.df <- function(data, clusters, statistic, R=250,
                    verbose = FALSE,
                    RNG = r.double.or.nothing,
                    .progress = "none",
                    .parallel = FALSE,
                    ...) {
  # lists containing information for each group
  cluster.ids <- list()
  num.clusters <- list()
  for(i in clusters) {
    groups.factor <- as.factor(data[[i]])
    cluster.ids[[i]] <- as.numeric(groups.factor)
    num.clusters[[i]] <- length(levels(groups.factor))
  }

  replicates <- foreach(r=1:R, .combine=c) %do% {
    # generate and multiply weight vectors
    w <- foreach(i = clusters, .combine = `*`) %do%
      {RNG(num.clusters[[i]])[cluster.ids[[i]]]}
    statistic(mutate(data, .weights=w), ...)
  }
  replicates
}

norm.intervals <- function(x, alpha=0.05) {
  data.frame(
    se1=qnorm(alpha/2, 0, sd(x)),
    se2=qnorm(1-alpha/2, 0, sd(x)),
	lower1=qnorm(alpha/2, 0, sd(x)) + mean(x),
	lower2=qnorm(alpha/2, mean(x), sd(x)),
	mean=mean(x),
	upper1=qnorm(1-alpha/2, 0, sd(x)) + mean(x),
	upper2=qnorm(1-alpha/2, mean(x), sd(x)))

}

baseline.diff <- function(data) {
	y0 <- with(subset(data, num.shown==0), wtd.mean(like, .weights))
	y1 <- with(subset(data, num.shown==1), wtd.mean(like, .weights))
	y1-y0
}

b <- boot.df(like.data, 'page', baseline.diff)



hist(b)
norm.intervals(b)