#################################################################
##### The bootstrap: an alternative way of computing SEs  #######
#################################################################
library(foreach)
library(dplyr)

num.items <- 9
num.subjects <- 250
num.items.to.rate <- 3

source('css_scripts.R')

mu <- num.items.to.rate/num.items

item.interestingness <- rnorm(num.items, 0, 0.02)
# subjects are mostly liberal
subject.alignment <- sample(c(-1, 0, 1), size=num.subjects, prob=c(0.5, 0.3, 0.2), replace=T)


results <- foreach(subject=1:num.subjects, .combine=rbind) %do% {
  permuted.sources <- sample(rep(c(-1,0,1), num.items/3), size=num.items)
  permuted.items <- sample(1:num.items, size=num.items)

  selections <- foreach(item=permuted.items, .combine=rbind) %do% {
    exposure <- data.frame(subject=subject, item=item,
      subject.alignment=subject.alignment[subject],
      source.alignment=permuted.sources[item])
    exposure <- mutate(exposure,
      source.agreement=sign(subject.alignment) == sign(source.alignment),
      # treatment effect is ~ +20% increase over the average rate
      p.select=mu + item.interestingness[item] + mu*0.2* source.agreement +
        rnorm(1, 0, 0.02),
      select=rbinom(1, 1, p.select)
     )
    exposure
  }
  selected <- sample(selections$item, size=num.items.to.rate, prob=selections$p.select)
  selections <- mutate(selections, select=item %in% selected)
  selections
}

summary(lm(select ~ source.agreement, data=results))


baseline.diff <- function(data) {
	y0 <- with(subset(data, source.agreement==FALSE), wtd.mean(select, .weights))
	y1 <- with(subset(data, source.agreement==TRUE), wtd.mean(select, .weights))
	y1-y0
}

b <- clustered.bootstrap(results, c('subject', 'item'), baseline.diff, .RNG=rexp)
norm.intervals(b)

