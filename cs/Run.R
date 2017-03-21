RAWmisc::InitialiseProject(
  PROJHOME = "/analyses/code_minor/cs/",
  PROJRAW = "/analyses/data_raw/cs/",
  PROJCLEAN = "/analyses/data_clean/cs",
  PROJBAKED = "/analyses/results_baked/cs/",
  PROJFINAL = "/analyses/results_final/cs/",
  PROJSHARED = "/dropbox/results_shared/cs/")

suppressWarnings(suppressMessages(library(data.table)))
suppressWarnings(suppressMessages(library(ggplot2)))
library(rstan)
library(foreach)
suppressMessages(library(doRedis))

tryCatch({
  registerDoRedis("cs",host="redis", nodelay=TRUE)
}, error=function(err){
  registerDoRedis("cs",host="redis", nodelay=TRUE)
})

if(getDoParWorkers()==1){
  suppressMessages(startLocalWorkers(n=parallel::detectCores(), queue = "cs", host="redis", nodelay=FALSE))
}
getDoParWorkers()
setProgress(TRUE)

#rstan_options(auto_write = TRUE)
#options(mc.cores = parallel::detectCores())

assign("RUN_ALL", TRUE, envir=globalenv())

UpdateLastWeekData()
UpdateOdds()

d <- ProcessData()
o <- ProcessOdds()

dates <- unique(d$Date)
numDatesWithOdds <- length(unique(o$Date))
stanExe = readRDS("stan/worldcup_matt_3.rda")

options(mc.cores = parallel::detectCores())
today <- foreach(i=(length(dates)-(numDatesWithOdds-1)):length(dates)) %do% RunModel(as.data.frame(d[Date %in% dates[(i-199):(i)]]))

todayEstimates <- rbindlist(today)
todayEstimates <- todayEstimates[,
                                 .(estWin=mean(estWin),
                                   winsA=sum(win),
                                   winsB=sum(1-win)),
                                 by=.(Date,gameNum,matchNum,teamA,teamB)]

#today <- RunModel(as.data.frame(d[Date %in% dates[(length(dates)-199):length(dates)]]))
todayEstimates[,estOdds:=1/estWin]

nrow(todayEstimates)
sum(todayEstimates$Date=="2017-03-20")
todayEstimates[Date=="2017-03-20"]
o[Date=="2017-03-20"]
todayEstimates <- merge(todayEstimates,o,by=c("Date","teamA","teamB","matchNum"))
sum(todayEstimates$Date=="2017-03-20")
nrow(todayEstimates)


setorder(todayEstimates,-time,-gameNum)

todayEstimates[,win:=0]
todayEstimates[winsA>winsB,win:=1]
todayEstimates[,bookValue:=1/oddsA+1/oddsB]
todayEstimates[,EV_A:=estWin*(oddsA-1)-(1-estWin)*1]
todayEstimates[,EV_B:=(1-estWin)*(oddsB-1)-(estWin)*1]
todayEstimates[,moneyA:=-1]
todayEstimates[win==1,moneyA:=oddsA-1]

todayEstimates[,moneyB:=-1]
todayEstimates[win==0,moneyB:=oddsB-1]

print(todayEstimates)

sum(todayEstimates[timeUntil<0]$moneyA)
sum(todayEstimates[timeUntil<0 & EV_A>0]$moneyA)
sum(todayEstimates[timeUntil<0 & EV_A>0 & estWin>0.5]$moneyA)
sum(todayEstimates[timeUntil<0 & EV_A> -0.05 & estWin>0.5]$moneyA)
sum(todayEstimates[timeUntil<0 & EV_A> -0.1 & estWin>0.5]$moneyA)
sum(todayEstimates[timeUntil<0 & EV_A> -0.15 & estWin>0.5]$moneyA)

todayEstimates[timeUntil<0 & EV_A>0 & estWin>0.5]
todayEstimates[timeUntil<0 & EV_B>0]
todayEstimates[EV_A>0 & estWin>0.5]
todayEstimates[timeUntil<0 & EV_A> -0.1 & estWin>0.5]
sum(todayEstimates[timeUntil<0 & EV_A> -0.2 & estWin>0.5]$moneyA)




mean(todayEstimates[timeUntil<0]$moneyA)
mean(todayEstimates[timeUntil<0 & EV_A>0]$moneyA)
mean(todayEstimates[timeUntil<0 & EV_A>0 & estWin>0.5]$moneyA)
mean(todayEstimates[timeUntil<0 & EV_A> -0.1 & estWin>0.5]$moneyA)

mean(todayEstimates[timeUntil<0 & estWin>0.5]$moneyA)
sum(todayEstimates[timeUntil<0 & estWin>0.5]$moneyA)
sum(todayEstimates[timeUntil<0]$moneyA)

todayEstimates[timeUntil<0 & EV_A>0 & estWin>0.5]
todayEstimates[timeUntil>0 & EV_A>0 & estWin>0.5]


sum(todayEstimates$moneyB)

options(mc.cores = 1)

print(lubridate::now())
x <- foreach(i=199:400) %dopar% RunModel(as.data.frame(d[Date %in% dates[(i-199):(i)]]))
saveRDS(x,file=file.path(RPROJ$PROJCLEAN,"est.RDS"))
x <- readRDS(file=file.path(RPROJ$PROJCLEAN,"est.RDS"))
# results with 100 days of data
# estWinBlock    n       win
# 1:          NA 2000 0.7360000
# 2:         0.3    3 0.6666667
# 3:         0.4    6 0.8333333
# 4:         0.5   38 0.6315789
# 5:         0.6  270 0.6851852
# 6:         0.7  465 0.7419355
# 7:         0.8  701 0.8373752
# 8:         0.9  157 0.8726115

df <- rbindlist(x)

logit <- function(p){
  log(p/(1-p))
}

invlogit <- function(p){
  1/(1+exp(-p))
}

df[,estWinlogit:=logit(estWin)]
summary(fit <- glm(win~splines::ns(estWin,df=2)*firstObs_a,family="binomial",data=df[1:1000]))
p <- predict(fit,df)
df[,estWin2:=invlogit(p)]
df[1:1000,estWin2:=NA]

summary(fit <- glm(win~splines::ns(estWin,df=2)*firstObs_a,family="binomial",data=df[1:2000]))
p <- predict(fit,df)
df[,estWin3:=invlogit(p)]
df[1:2000,estWin3:=NA]

df[,estWinBlock:=round(estWin*10)/10]
res <- df[,.(n=.N,win=mean(win)),by=estWinBlock]
setorder(res,estWinBlock)
res[,p:=n/sum(n)]
res

df[,estWinBlock:=round(estWin2*10)/10]
res <- df[,.(n=.N,win=mean(win)),by=estWinBlock]
setorder(res,estWinBlock)
res[,p:=n/sum(n)]
res

df[,estWinBlock:=round(estWin3*10)/10]
res <- df[,.(n=.N,win=mean(win)),by=estWinBlock]
setorder(res,estWinBlock)
res[,p:=n/sum(n)]
res





q <- ggplot(d,aes(y=sprintf("%s vs %s",teamA,teamB)))
q <- q + geom_errorbarh(aes(x=estDif,xmin=estDifL1,xmax=estDifL2))
q <- q + geom_point(aes(x=estDif))
q <- q + geom_point(aes(x=scoreDif),col="red")
q
png("worldcup8.png", height=1000, width=500)
coefplot ((score1 - score2)[new_order]*flip, sds=rep(0, ngames),
          lower.conf.bounds=sim_quantiles[new_order,1]*flip, upper.conf.bounds=sim_quantiles[new_order,2]*flip, 
          varnames=ifelse(flip==1, paste(teams[team1[new_order]], "vs.", teams[team2[new_order]]),
                          paste(teams[team2[new_order]], "vs.", teams[team1[new_order]])),
          main="Game score differentials\ncompared to 95% predictive interval from model\n",
          mar=c(0,7,6,2), xlim=c(-6,6))
dev.off()



