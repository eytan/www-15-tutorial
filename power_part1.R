# Last updated May 29 2014, 11:09pm PST.

# First time users: install packages by uncommenting the following line.
#install.packages(c('dplyr', 'ggplot2', 'foreach', 'lmtest', 'sandwich', 'Hmisc', 'data.table', 'doMC'))

library('plyr')
library('dplyr')
library('ggplot2')
library('foreach')
library('lmtest')
library('sandwich')
library('Hmisc')
library('data.table')

2

options(digits=3)

setwd('~/website/icwsm-14-tutorial')

#######################################################
##### Sampling distributions and standard errors ######
#######################################################

# In an experiment we're interested in the difference of two averages.
# Our certainty about the value of an average depends on the number of
# samples we have. Before we dive into the variation we see in the
# difference of two means, let's look at the variance of a single variable.

# Consider the mean of a bernoulli random variable, where we flip a coin
# that lands on 1 with probability p, and 0 with probability (1-p).

# We flip a coin N times, observe the average number of times it lands on 1,
# and take measurements of this mean num.replications times.
N <- 1e2
p <- 0.2
num.replications <- 2000
replications <- replicate(num.replications, mean(rbinom(N, 1, p)))


#### Sampling distributions and standard errors

# Here is the variability in the estimates.
# This is called the sampling distribution.
hist(replications, xlim=c(0,0.4))

# The standard error is the standard deviation of the sampling distribution.
# 95% of our estimates of p fall within this range:
quantile(replications, c(0.025, 0.975))

# Alternatively, we can use the standard deviation of the sampling distribution.
# which is approximately normal for sufficiently large N.
est.se <- sd(replications)
mean.est.p <- mean(replications)
mean.est.p
est.se
c(mean.est.p - 1.96*est.se, mean.est.p + 1.96*est.se)

# For large sample sizes, we can approximate this using the following,
# without explicitly constructing the sampling distribution.
single.trial <- rbinom(N, 1, p)
p.single.trial <- mean(single.trial)

# normal approximation of SE
se <- sqrt(p.single.trial*(1-p.single.trial)/N)
se

# norm approx 95% CI
c(p.single.trial - 1.96*se, p.single.trial + 1.96*se)

# This is close to what comes out of a linear regression.
d <- data.frame(y=single.trial)
coeftest(lm(y ~ 1, data=d))


#### Standard errors and N.

# let's construct the confidence intervals for p for a few values of p, N
d <- expand.grid(p=c(0.05, 0.25, 0.5), n=seq(100, 2000, 100))
d <- mutate(d, se=sqrt(p*(1-p)/n))
head(d)

qplot(n, p, data=d, color=factor(p),
  ylab='p (with 95% confidence interval)') +
  geom_errorbar(aes(ymin=p-1.96*se, ymax=p+1.96*se))

# you can see that the standard error rapidly diminishes with N.
# (sqrt(1/n), in particular).
qplot(n, se, data=d, color=factor(p), geom='line', ylab='standard error')



##################################################################
##### Confidence intervals for an average treatment effect #######
##################################################################

# Let's consider a simulated experiment with a binary treatment, D.

# The function returns a dataframe with a row for each subject, her
# potential outcomes, treatment status, and observed outcome.
# The inputs are the number of subjects and the ATE.
run.experiment <- function(n, ate) {
  my.experiment <- data.frame(
    y0=rnorm(n, mean=10, sd=3))
  mutate(my.experiment,
    y1=y0 + ate,
    D=rbinom(n, 1, 0.5),
    y=D*y1 + (1-D)*y0)
}

my.experiment <- run.experiment(500, 2.0)

head(my.experiment)

qplot(y, geom='histogram', data=my.experiment, facets=D~.) + theme_bw()

compute.ate <- function(exp) {
  mean(exp[exp$D==1,]$y) - mean(exp[exp$D==0,]$y)
}

# We can compute the difference in means for a single experiment
compute.ate(my.experiment)

# Now, let's compute the diffence in means for 2000 replications
# of our experiment. This is the sampling distribution of the ATE.

num.replications <- 2e3
ates <- replicate(
  num.replications,
  compute.ate(run.experiment(n=1e3, 2.0)))

hist(ates, main='sampling distribution of ATE')

# 95% of the estimated treated effects lie within this range
quantile(ates, c(0.025, 0.975))

# Large sample approximations let us make statements about the
# variance of the ATE over different (similar) populations (or
# possible randomizations).

compute.ate(my.experiment)
with(my.experiment, t.test(y[D==1], y[D==0]))

# or via linear regression
m <- lm(y ~ D, data=my.experiment)
coeftest(m)
# take the SE for D, multiply by 1.96, e.g.,
est.ate <- coeftest(m)['D', 'Estimate']
est.se <- coeftest(m)['D', 'Std. Error']

c(est.ate-1.96*est.se, est.ate+1.96*est.se)

# Here is a function that returns confidence intervals for a dataframe
# with an outcome y and a binary treatment D. We will use this later.

get.ci <- function(experiment) {
  t <- coeftest(lm(y ~ D, data=experiment))
  est.ate <- t['D', 'Estimate']
  est.se <- t['D', 'Std. Error']
  p.value <- t['D', 'Pr(>|t|)']
  return(list(est.ate=est.ate, est.se=est.se, p.value=p.value))
}

get.ci(my.experiment)

# For smaller sample sizes and/or heavily skewed outcomes, or non-iid data,
# it is recommended to use other methods like the bootstrap (covered later),
# randomization inference, or robust cluster standard errors.

#################################################
##### Type I, Type II, and Type M Errors  #######
#################################################

# To investigate the variation we get when repeating a hypothetical experiment
# multiple times, we simulate multiple replications of a randomized trial.

run.experiment(n=8, ate=5)

# This function runs an experiment multiple times (num.replications),
# and generates a dataframe containing estimates of the treatment effects and
# standard errors for each experiment, sorted by the size of the effect.
replicate.experiment <- function(sim.func, params, num.replications=100) {
  # Note: you can parallelize this process below by using %dopar% if
  # you have the 'doMC' package installed.
  exps <- foreach(replication=1:num.replications, .combine=rbind) %do% {
  	as.data.frame(get.ci(do.call(sim.func, params)))
  }
  exps <- exps[order(exps$est.ate),]
  exps <- mutate(exps,
    rank=1:nrow(exps),
    significant=sign(est.ate-1.96*est.se)==sign(est.ate+1.96*est.se)
  )
  exps
}

#####
# Type I error: detecting a "statistically significant" effect when
# there is no effect.

# run 100 trials of an experiment with no ATE
null.experiments <- replicate.experiment(run.experiment,
  list(n=1e3, ate=0.0),
  num.replications=100)

qplot(est.ate, data=null.experiments, main="sampling distribution of ATE")

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

# exercise: use R to show that the above proportion not significantly different from 0.05

### Type II errors

# this experiment has a true effect of +0.5 (out of an average, 10)
sad.experiments <- replicate.experiment(run.experiment,
  list(n=250, ate=0.5),
  num.replications=100)

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



###################################
##### Power analysis           ####
###################################

# In earlier slides we came up with a few expressions for the standard
# error.  Let's consider that again.

# Let's add two new agruments, the proportion treated, and the
# standard deviation of the outcome variable.
run.experiment2 <- function(n, ate, prop.treated=0.5, y.sd=3) {
  my.experiment <- data.frame(
    y0=rnorm(n, mean=10, sd=y.sd))
  mutate(my.experiment,
    y1=y0 + ate,
    D=rbinom(n, 1, prop.treated),
    y=D*y1 + (1-D)*y0)
}

# Let's simulate a few replications with varying proportions of
# users assigned to treatment and control.
exps <- foreach(p=seq(0.05, 0.95, 0.05), .combine=rbind) %do% {
  varying.prop = list(n=1e3, ate=1, prop.treated=p, y.sd=3)
  replications <- replicate.experiment(
    sim.func=run.experiment2,
    params=varying.prop,
    num.replications=250)
  data.frame(
    prop.treated=p,
    sim.se=sd(replications$est.ate),
    sim.ate=mean(replications$est.ate),
    norm.est.se=head(replications$est.se, 1))
}

qplot(prop.treated, 1.96*sim.se, data=exps,
      ylab='95% CI width', xlab='proportion of subjects in treatment',
      ylim=c(0, 2.0), main='Precision of estimated ATE (simulation)')

qplot(prop.treated, 1.96*norm.est.se, data=exps,
       ylab='95% CI width', xlab='proportion of subjects in treatment',
       ylim=c(0,2.0), main='Precision of estimated ATE (approximation)')


# We will use a stripped down version that doesn't use replications for
# demonstration purposes, using get.ci() with a single trial for each
# parameterization.

n <- 1e3
true.ate <- 0.1
p=0.5
exps <- foreach(n=seq(100, 1e3, 1e2), .combine=rbind) %do% {
  foreach(my.sd=c(1, 2, 3), .combine=rbind) %do% { 
    my.experiment <- run.experiment2(
      n=n, ate=1, prop.treated=0.5, y.sd=my.sd)
    my.ses <- get.ci(my.experiment)
    data.frame(n=n, sd=my.sd,
               est.ate=my.ses$est.ate,
               est.se=my.ses$est.se)
  }
}

qplot(n, 1.96*est.se, data=exps, color=factor(sd), geom=c('line', 'point')) + geom_hline(aes(yintercept=true.ate), linetype='dashed')


## Exercise 2: Do the same thing with a skewed distribution, like the lognorm
##             (you can generate a log-norm with rlnorm().
##             Do you notice any difference between how big your N has to be
##             in order for the 
