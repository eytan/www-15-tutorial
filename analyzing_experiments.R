
library('plyr')
source('css_stats.R')


Exp <- read.csv('webapp/experiment_data.csv', stringsAsFactors=F)

## uniquify the survey responses, take only the first resp
Survey <- read.csv('webapp/survey_data.csv', stringsAsFactors=F)
Survey <- subset(Survey, !is.na(response))
Survey <- ddply(Survey, .(userid), summarise,
                response = response[1],
                reversed_scale = reversed_scale[1])
Survey$subject.ideology <- Survey$response - 3
## convert the test to a binary outcome variable
Exp$select <- with(Exp, as.numeric(summary != ''))
## join to survey data
results <- join(unique(Exp), Survey, by = 'userid', type = 'inner')

src2ideology <- c('foxnews'=1,'msnbc'=-1,'cnn'=0)
results <- mutate(results,
  source.ideology=src2ideology[source],
  source.agreement=sign(source.ideology)==sign(subject.ideology),
  subject.ideology=factor(sign(subject.ideology)))


position.fx <- results %>% group_by(position) %>% summarise(frac.selected=mean(select))
qplot(position, frac.selected, data=position.fx)

position.fx <- results %>% group_by(position, source.ideology) %>% summarise(frac.selected=mean(select))
position.fx
qplot(position, frac.selected, data=position.fx, color=factor(source.ideology))

rating.freqs <- results %>% group_by(userid) %>% summarise(num.selected=sum(select)) %>% group_by(num.selected) %>% summarise(freq=n())


## linear model without covariates (anti-conservative)
summary(lm(select ~ source.agreement*subject.ideology + factor(position), data=subset(results, position==0)))

## linear model with covariates
summary(lm(select ~ factor(position), data=results))

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
