# testing to see what deviation from seasonality actually means

day_of_year <- 1:366
seasonal_effect_sin <- sin(2*pi*day_of_year/366)
seasonal_effect_cos <- cos(2*pi*day_of_year/366)
deviation_from_season <- rnorm(366)
error <- rnorm(366)

d <- data.table(
  day_of_year,
  seasonal_effect_sin,
  seasonal_effect_cos,
  deviation_from_season,
  error)
d[,deviation_from_season:=rnorm(.N)]

d[,x:=seasonal_effect+deviation_from_season]
d[,y:=deviation_from_season+error]

fit <- lm(x~seasonal_effect_sin+seasonal_effect_cos,data=d)
d[,expected_seasonal:=predict(fit)]
d[,calculated_deviation_from_expected:=x-expected_seasonal]


q <- ggplot(d,aes(x=day_of_year))
q <- q + geom_point(mapping=aes(y=x),colour="black")
q <- q + geom_point(mapping=aes(y=seasonal_effect),colour="red")
q

summary(lm(y~deviation_from_season,data=d))
summary(lm(y~x,data=d))
summary(lm(y~x+seasonal_effect_sin+seasonal_effect_cos,data=d))
summary(lm(y~calculated_deviation_from_expected,data=d))



