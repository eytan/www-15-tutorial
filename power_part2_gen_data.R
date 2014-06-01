############### Power calculations with real data  ###########

options(digits=3)

N <- 1e5
mu <- 0.05
ate <- 0.02
gdata <- data.frame(
  id=1:N,
  is.generous=rbinom(N, 1, 0.1),
  generosity=0
)
gdata[gdata$is.generous==0,]$generosity  <- rnorm(sum(gdata$is.generous==0), mean=0.01, sd=0.03)
gdata[gdata$is.generous==1,]$generosity  <- rnorm(sum(gdata$is.generous==1), mean=0.6, sd=0.04)
gdata <- mutate(gdata,
  num.friends=rnorm(N, 360, 30),
  gifts.received=rbinom(N, floor(0.015*num.friends/6),
    pmin(1, pmax(0, mu + generosity + rnorm(N, 0, 0.05)))),
  gift.utility=mu + generosity + rnorm(N, 0, 0.005),
  D=rbinom(N, 1, 0.5),
  gave.gift=rbinom(N, 1, pmin(pmax(0, gift.utility + ate*gifts.received), 1)),
)

summary(lm(gave.gift ~ gifts.received, data=gdata))



write.csv(file='gift_data.csv', gdata[,c('num.friends', 'gifts.received', 'gave.gift')])
t <- read.csv('gift_data.csv')
