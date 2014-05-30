# Last updated May 29 2014, 11:09pm PST.

# First time users: install packages by uncommenting the following line.
#install.packages('plyr', 'dplyr', 'ggplot2', 'foreach', 'lmtest', 'sandwich', 'Hmisc', 'data.table')

library('plyr')
library('dplyr')
library('ggplot2')
library('foreach')
library('lmtest')
library('sandwich')
library('Hmisc')
library('data.table')

options(digits=3)

#######################################################
##### Sampling distributions and standard errors ######
#######################################################

# In an experiment we're interested in the difference of two averages.
# Our certainty about the value of an average depends on the number of
# samples we have. For example, consider the mean of a bernoulli random
# variable.

N <- 1e3
p <- 0.2
num.replications <- 2000
replications <- replicate(num.replications, mean(rbinom(N, 1, p)))
# replications <- rbinom(N, num.replications, p)/num.replications  # equivalently?
est.p <- mean(replications)
hist(replications, xlim=c(0,0.4))

# The above plot is called the sampling distribution.
# The standard error is the standard deviation of the sampling distribution.

# 95% of our estimates of p fall within this range:
quantile(replications, c(0.025, 0.975))

# for large sample sizes, we can approximate this using the following
se = sqrt(est.p*(1-est.p)/N)        # normal approximation
c(est.p - 1.96*se, est.p + 1.96*se) # norm approx 95% CI

se = sd(replications)
c(est.p - 1.96*se, est.p + 1.96*se)


# let's construct the confidence intervals for p for a few values of p, N
d <- expand.grid(p=c(0.05, 0.25, 0.5), n=seq(100, 1e4, 100))
d <- mutate(d, se=sqrt(p*(1-p)/n))
qplot(n, p, data=d, color=factor(p), ylab='p (with 95% confidence interval)') + geom_errorbar(aes(ymin=p-1.96*se, ymax=p+1.96*se))

# you can see that the standard error rapidly diminishes with N
qplot(n, se, data=d, color=factor(p), geom='line', ylab='standard error')



##################################################################
##### Confidence intervals for an average treatment effect #######
##################################################################

# Let's consider an experiment an experiment with a binary treatment, D.

N <- 1e3
ate <- 2.0
my.experiment <- data.frame(y0=rnorm(N, mean=10, sd=3), D=rbinom(N, 1, 0.5))
my.experiment <- mutate(my.experiment, y1=y0+ate, y=D*y1 + (1-D)*y0)
qplot(y, geom='histogram', data=my.experiment, facets=D~.)

# We can compute the difference in means
treated.subjects <- subset(my.experiment, D==1)
control.subjects <- subset(my.experiment, D==0)
mean(treated.subjects$y) - mean(control.subjects$y)

# ??????????????? Compute CIs via t.test
t.test(treated.subjects, control.subjects)

# or via linear regression
m <- lm(y ~ D, data=my.experiment)
summary(m)
# take the SE for D, multiply by 1.96, e.g.,
est.ate <- coeftest(m)['D', 'Estimate']
est.se <- coeftest(m)['D', 'Std. Error']

c(est.ate-1.96*est.se, est.ate+1.96*est.se)


# Here is a function that returns confidence intervals for a binary
# treatment D.

get.ci <- function(experiment) {
  t <- coeftest(lm(y ~ D, data=experiment))
  est.ate <- t['D', 'Estimate']
  est.se <- t['D', 'Std. Error']
  p.value <- t['D', 'Pr(>|t|)']
  return(list(est.ate=est.ate, est.se=est.se, p.value=p.value))
}

get.ci(my.experiment)

# The above is just one way of obtaining confidence intervals, and it
# should work well for reasonably large N for any type of data.


#################################################
##### Type I, Type II, and Type M Errors  #######
#################################################

# To investigate the variation we get when repeating a hypothetical experiment
# multiple times, we simulate. Here is one simple simulation where the potential
# outcome under the control is norm(10,3), an additive treatment effect of 'ate'.
# Subjects are assigned to the treatment condition (D) with equal probability.
# The function finally returns a dataframe with a row for each subject, her
# potential outcomes, treatment status, and observed outcome.

run.trial <- function(n, ate) {
  my.experiment <- data.frame(
    y0=rnorm(n, mean=10, sd=3))
  mutate(my.experiment,
    y1=y0 + ate,
    D=rbinom(n, 1, 0.5),
    y=D*y1 + (1-D)*y0)
}

run.trial(n=8, ate=5)

# This function runs an experiment multiple times (num.replications),
# and generates a dataframe containing estimates of the treatment effects and
# standard errors for each experiment, sorted by the size of the effect.
replicate.experiment <- function(n, ate, num.replications=100) {
  # Note: you can parallelize this process below by using %dopar% if
  # you have the 'doMC' package installed.
  exps <- foreach(replication=1:num.replications, .combine=rbind) %do% {
  	as.data.frame(get.ci(run.trial(n, ate)))
  }
  exps <- exps[order(exps$est.ate),]
  exps <- mutate(exps,
    rank=1:nrow(exps),
    significant=sign(est.ate-1.96*est.se)==sign(est.ate+1.96*est.se)
  )
}

#####
# Type I error: detecting a "statistically significant" effect when
# there is no effect.

# run 100 trials of an experiment with no ATE
null.experiments <- replicate.experiment(n=1e3, ate=0.0, num.replications=100)

# plot the results of the experiments, rank-ordered by effect size, along
# with their confidence intervals. Experiments that are "significant" have
# confidence intervals that do not cross zero and are highlighted in blue.
qplot(rank, est.ate, data=null.experiments, color=significant,
  xlab='experiment number (ranked by effect size)', ylab='estimated ATE',
  main='demonstration of Type I errors') +
  geom_pointrange(aes(ymin=est.ate-1.96*est.se, ymax=est.ate+1.96*est.se)) +
  geom_point()

# This is the Type I error rate, and it is 0.05 because we use an
# appropriate method for computing confidence intervals.
mean(null.experiments$significant)

# exercise: use R to show that the above is not significantly different than 0.05

### Type II errors

# this experiment has a true effect of +0.5 (out of an average, 10)
sad.experiments <- replicate.experiment(n=250, ate=0.5, num.replications=100)

# plot the results of the experiments, rank-ordered by effect size, along
# with their confidence intervals. Experiments that are "significant" have
# confidence intervals that do not cross zero and are highlighted in blue.
qplot(rank, est.ate, data=sad.experiments, color=significant,
  xlab='experiment number (ranked by effect size)', ylab='estimated ATE',
  main='demonstration of Type II errors') +
  geom_pointrange(aes(ymin=est.ate-1.96*est.se, ymax=est.ate+1.96*est.se)) +
  geom_point()

# Fraction of experiments that are statistically significant.
# This is called the statistical power of the experiment.
mean(sad.experiments$significant)

### Type M errors

# when experiments are underpowered, only reporting on significant experiments
# can massively overstate effects.
with(subset(sad.experiments, significant==TRUE), mean(est.ate))


############### Power calculations with real data  ###########

options(digits=3)

N <- 1e5
mu <- -0.05

gdata <- data.frame(
  id=1:N,
  generosity=rnorm(N, mean=0.0, sd=0.02)
)

gdata <- mutate(gdata,
  num.friends=rnorm(N, 360, 20),
  gifts.received=rbinom(N, floor(0.015*num.friends/6),
    pnorm(mu + generosity + rnorm(N, 0, 0.01))),
  gift.utility=mu + generosity + rnorm(N, 0, 0.01),
  gave.gift=ifelse(gift.utility + 0.05*gifts.received>0, 1, 0),
  gave.gift.no.peer=ifelse(gift.utility > 0, 1, 0),
  gave.gift.forced.peer=ifelse(gift.utility + 0.05 > 0, 1, 0)
)

qplot(gift.utility, data=gdata, xlim=c(-0.15, 0.05)) + facet_grid(facets=gave.gift~., scales='free')


hist(gdata$gifts.received)
hist(gdata$gift.utility)
mean(gdata$gifts.received)
mean(gdata$gave.gift.no.peer)
mean(gdata$gave.gift.forced.peer)

mean(pnorm(gdata$gave.gift.forced.peer)) - mean(pnorm(gdata$gave.gift.no.peer))
mean(pnorm(gdata$gave.gift.forced.peer)) - mean(pnorm(gdata$gave.gift.no.peer))


qplot(gift.utility, data=gdata, fill=factor(gave.gift))


qplot(gift.utility, data=gdata, xlim=c(-0.15, 0.05)) + facet_grid(facets=gave.gift~., scales='free')
qplot(gift.utility, data=gdata, xlim=c(-0.15, 0.05)) + facet_grid(facets=gave.gift0~., scales='free')



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