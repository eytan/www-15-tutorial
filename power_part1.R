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
# samples we have. Before we dive into the variation we see in the
# difference of two means, let's look at the variance of a single variable.

# Consider the mean of a bernoulli random variable, where we flip a coin
# that lands on 1 with probability p, and 0 with probability (1-p).

# We flip a coin N times, observe the average number of times it lands on 1,
# and take measurements of this mean num.replications times.
N <- 1e4
p <- 0.2
num.replications <- 2000
replications <- replicate(num.replications, mean(rbinom(N, 1, p)))
# replications <- rbinom(N, num.replications, p)/num.replications  # equivalently?


#### Sampling distributions and standard errors

# Here is the variability in the estimates.
# This is called the sampling distribution.
hist(replications, xlim=c(0,0.4))

# It converges to p.
est.p <- mean(replications)

# The standard error is the standard deviation of the sampling distribution.
# 95% of our estimates of p fall within this range:
quantile(replications, c(0.025, 0.975))

# Alternatively, we can use the standard deviation.
se = sd(replications)
se
c(est.p - 1.96*se, est.p + 1.96*se)

# For large sample sizes, we can approximate this using the following,
# without explicitly constructing the sampling distribution.
p.single.trial = mean(rbinom(N, 1, p))
se = sqrt(p.single.trial*(1-p.single.trial)/N)        # normal approximation
se
c(p.single.trial - 1.96*se, p.single.trial + 1.96*se) # norm approx 95% CI

# This is equivalent to what comes out of a linear regression.
d <- data.frame(y=rbinom(N, 1, p))
coeftest(lm(y ~ 1, data=d))


#### Standard errors and N.

# let's construct the confidence intervals for p for a few values of p, N
d <- expand.grid(p=c(0.05, 0.25, 0.5), n=seq(100, 1e4, 100))
d <- mutate(d, se=sqrt(p*(1-p)/n))
qplot(n, p, data=d, color=factor(p), ylab='p (with 95% confidence interval)') + geom_errorbar(aes(ymin=p-1.96*se, ymax=p+1.96*se))

# you can see that the standard error rapidly diminishes with N.
# (sqrt(1/n), in particular).
qplot(n, se, data=d, color=factor(p), geom='line', ylab='standard error')


## Exercise 1: Do the same thing with a normal distribution
## Exercise 2: Do the same thing with a skewed distribution, like the lognorm
##             (you can generate a log-norm with rlnorm().
##             Do you notice any difference between how big your N has to be
##             in order for the 


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
## @sjt Doesn't seem to work, what am I doing wrong?
t.test(treated.subjects, control.subjects)

# or via linear regression: should probably use HC errors
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
null.experiments <- replicate.experiment(
  run.trial, list(n=1e3, ate=0.0), num.replications=100)

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
sad.experiments <- replicate.experiment(run.trial, list(n=250, ate=0.5),
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

# In earlier slides we came up with a few expressions for the standard error
# Let's consider that again.

run.trial2 <- function(n, ate, prop.treated=0.5, y.sd=3) {
  my.experiment <- data.frame(
    y0=rnorm(n, mean=10, sd=y.sd))
  mutate(my.experiment,
    y1=y0 + ate,
    D=rbinom(n, 1, prop.treated),
    y=D*y1 + (1-D)*y0)
}

reps <- replicate.experiment(run.trial2,
  list(n=1e3, ate=1, prop.treated=0.5, y.sd=3),
  num.replications=5)
reps


hist(reps$est.ate)

exps <- foreach(p=seq(0.01, 0.2, 0.1), .combine=rbind) %dopar% {
  varying.prop = list(n=1e3, ate=1, prop.treated=p, y.sd=3)
  replications <- replicate.experiment(
    sim.func=run.trial2,
    params=varying.prop,
    num.replications=250)
  data.frame(
    prop.treated=p,
    sim.se=sd(replications$est.ate),
    sim.ate=mean(replications$est.ate),
    mean.est.se=mean(replications$est.se))
}
