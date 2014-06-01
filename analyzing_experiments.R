
library('plyr')
source('css_stats.R')

Exp <- read.csv('experiment_data.csv', stringsAsFactors=F)

Survey <- read.csv('survey_data.csv', stringsAsFactors=F)
Survey <- subset(Survey, !is.na(response))

## uniquify the survey responses, take only the first resp
Survey <- ddply(Survey, .(userid), summarise,
                response = response[1],
                reversed_scale = reversed_scale[1])

## convert the test to a binary outcome variable
Exp$select <- with(Exp, as.numeric(summary != ''))

## join to survey data
results <- join(Exp, Survey, by = 'userid', type = 'inner')

## left == -1, right == 1
results$source.ideology <- with(results, ifelse(source == 'foxnews', -1, 1))
results$subject.ideology <- with(results, ifelse(response < 2, -1, 1))

## treatment dummy
results$source.agreement <- with(results, as.numeric(source.ideology * subject.ideology > 0))


## linear model without covariates (anti-conservative)
summary(lm(select ~ source.agreement, data=results))

## linear model with covariates
summary(lm(select ~ factor(position) + source.agreement, data=results))

baseline.diff <- function(data) {
	y0 <- with(subset(data, source.agreement==FALSE), wtd.mean(select, .weights))
	y1 <- with(subset(data, source.agreement==TRUE), wtd.mean(select, .weights))
	y1-y0
}

## user only bootstrap
b <- clustered.bootstrap(results, c('userid'), baseline.diff, .RNG=rexp)
norm.intervals(b)


## user and item bootstrap
b <- clustered.bootstrap(results, c('userid', 'story_key'), baseline.diff, .RNG=rexp)
norm.intervals(b)


## bootstrap with regression
reg.ate <- function(data) {
  m <- lm(select ~ factor(position) + source.agreement, data=data, weights = .weights)
  coef(m)['source.agreement']
}

b <- clustered.bootstrap(results, c('userid', 'story_key'), reg.ate, .RNG=rexp)
norm.intervals(b)
