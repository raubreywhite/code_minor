RAWmisc::InitialiseProject(
  PROJHOME = "/analyses/code_minor/cs/",
  PROJRAW = "/home/rstudio/analyses/data_raw/cs",
  PROJCLEAN = "/analyses/data_clean/cs",
  PROJBAKED = "/analyses/results_baked/cs/",
  PROJFINAL = "/analyses/results_final/cs/",
  PROJSHARED = "/dropbox/results_shared/cs/")

suppressWarnings(suppressMessages(library(data.table)))
suppressWarnings(suppressMessages(library(ggplot2)))
suppressWarnings(suppressMessages(library(pomp)))
library(rstan)
library(foreach)
suppressMessages(library(doRedis))

tryCatch({
  registerDoRedis("cs",host="redis", nodelay=TRUE)
}, error=function(err){
  registerDoRedis("cs",host="redis", nodelay=TRUE)
})
#doRedis::removeQueue("cs")

if(getDoParWorkers()<=1){
  suppressMessages(startLocalWorkers(n=parallel::detectCores(), queue = "cs", host="redis", nodelay=FALSE))
}
getDoParWorkers()
setProgress(TRUE)

assign("RUN_ALL", F, envir=globalenv())

UpdateLastWeekData()
UpdateOdds()

d <- ProcessData()
o <- ProcessOdds()

a <- RunWithOdds(d, o, RunModel=RunModelInternal4, model="stan/worldcup_matt_4", today=T)
sum(a[timeUntil<0 & EV_A>0 & estWin>0.5]$moneyA)
sum(a[timeUntil<0 & EV_A>0 & estWin>0.5]$moneyA)
a[timeUntil>0 & EV_A>0 & estWin>0.5]

b <- RunWithOdds(d, o, RunModel=RunModelInternal4, model="stan/worldcup_matt_4", today=F)


bets <- b[timeUntil<0 & EV_A>0.00 & estWin>0.50 & matchNum==1]
f <- lm(win~x,data=bets[1:40])
#bets[,estWin:=predict(f,bets)]
#
#bets[,estWin:=bookValue/oddsA]
#bets[,estWin:=estWin*mean(bets[1:30]$win)/mean(bets[1:30]$estWin)]
#bets <- bets[31:.N]
mean(bets$win)
mean(bets$estWin)

#bets[,EV_A:=estWin*(oddsA-1)-(1-estWin)*1]
#bets[,kellyA:=round(0.95*(EV_A/(oddsA-1)),3)]

#bets[,kellyA:=kellyA*0.2]
#bets[kellyA>0.1,kellyA:=0.1]
bets[,.(money=sum(moneyA),N=.N),by=Date]
sum(bets$moneyA)
sum(bets$EV_A)
bets <- bets[.N:1]
money <- 100
for(i in 1:nrow(bets)){
  #if(bets$kellyA[i]<0) next
  bet <- money[i]*bets$kellyA[i]
  result <- bet*bets$moneyA[i]
  money <- c(money,money[i]+result)
}
money[length(money)]
length(money)

bets <- b[timeUntil<0 & estWin<0.5 & EV_B>0.0 & matchNum>=1]
#bets[,kellyB:=kellyB/2]
bets[kellyB>0.05,kellyB:=0.05]
bets[,.(perc=mean(moneyB>0),money=sum(moneyB),N=.N),by=Date]
sum(bets$moneyB)
sum(bets$EV_B)
bets <- bets[.N:1]
money <- 100
for(i in 1:nrow(bets)){
  bet <- money[i]*bets$kellyB[i]
  #if(bets$matchNum[i]==2) bet <- bet/4
  result <- bet*bets$moneyB[i]
  money <- c(money,money[i]+result)
}
money[length(money)]
length(money)



bets[,estA:=1/oddsA]
summary(lm(win~-1+estA,data=bets[matchNum==2]))
summary(lm(win~-1+estWin,data=bets[matchNum==2]))
summary(lm(win~-1+estWin+estA,data=bets[matchNum==2]))

length(money)

mean(bets$moneyA)
(money[length(money)]-money[1])/nrow(bets)/money[1]

sum(b[timeUntil<0 & EV_A>0 & estWin>0.5]$moneyA)
mean(b[timeUntil<0 & EV_A>0 & estWin>0.5]$moneyA)
length(b[timeUntil<0 & EV_A>0 & estWin>0.5]$moneyA)






#stanExe = readRDS("stan/worldcup_matt_3.rda")

x <- RunWithoutOdds(d,RunModel=RunModelInternal3,model="stan/worldcup_matt_3",suffix="_3")
x <- readRDS(file.path(RPROJ$PROJCLEAN,"est_3.RDS"))
est3 <- rbindlist(x)

x <- RunWithoutOdds(d,RunModel=RunModelInternal4,model="stan/worldcup_matt_4",suffix="_4")
x <- readRDS(file.path(RPROJ$PROJCLEAN,"est_4.RDS"))
est4 <- rbindlist(x)


logit <- function(p){
  log(p/(1-p))
}

invlogit <- function(p){
  1/(1+exp(-p))
}

ProcessEst <- function(x,regression=FALSE){
  df <- rbindlist(x)
  
  summary(fit <- glm(win~splines::ns(logit(estWin),df=3),family="binomial",data=df[1:1000]))
  p <- predict(fit,df)
  df[,estWin2:=invlogit(p)]
  df[1:1000,estWin2:=NA]

  if(regression){
    df[,estWinBlock:=round(estWin2*10)/10]
  } else {
    df[,estWinBlock:=round(estWin*10)/10]
  }
  res <- df[,.(n=.N,win=mean(win)),by=estWinBlock]
  setorder(res,estWinBlock)
  res[,p:=n/sum(n)]
  return(res)
}

print(ProcessEst(readRDS(file.path(RPROJ$PROJCLEAN,"est_3.RDS"))))
print(ProcessEst(readRDS(file.path(RPROJ$PROJCLEAN,"est_4.RDS"))))

print(ProcessEst(readRDS(file.path(RPROJ$PROJCLEAN,"est_3.RDS")),TRUE))
print(ProcessEst(readRDS(file.path(RPROJ$PROJCLEAN,"est_4.RDS")),TRUE))

model = "stan/worldcup_matt_3"
suffix = "_3"
RunModel <- RunModelInternal3

dates <- unique(d$Date)

options(mc.cores = 1)
print("Checking if model is compiled")
RunModelInternal3(dataframe=as.data.frame(d[Date %in% dates[(100-9):(100)]]),model=model)
stanExe = readRDS("stan/worldcup_matt_3.RDS")

print("Running in parallel")
x <- foreach(i=199:200) %dopar% RunModelInternal3(dataframe=as.data.frame(d[Date %in% dates[(i-99):(i)]]),stanExe=stanExe)

saveRDS(x,file=file.path(RPROJ$PROJCLEAN,paste0("est",suffix,".RDS")))
x <- readRDS(file=file.path(RPROJ$PROJCLEAN,paste0("est",suffix,".RDS")))

RunWithoutOdds(d,RunModel=RunModelInternal3,model="stan/worldcup_matt_3",suffix="_3")
doRedis::removeQueue("cs")





sum(todayEstimates[Date=="2017-03-22" & EV_A>0 & estWin>0.5]$moneyA)

todayEstimates[timeUntil>0 & EV_A>0 & estWin>0.5]

sum(todayEstimates[timeUntil<0 & EV_A>0 & estWin>0.5]$moneyA)
mean(todayEstimates[timeUntil<0 & EV_A>0 & estWin>0.5]$moneyA)
length(todayEstimates[timeUntil<0 & EV_A>0 & estWin>0.5]$moneyA)

sum(todayEstimates[timeUntil<0]$moneyA)
sum(todayEstimates[timeUntil<0 & EV_A>0]$moneyA)
sum(todayEstimates[timeUntil<0 & EV_A>0 & estWin>0.5]$moneyA)
sum(todayEstimates[timeUntil<0 & EV_A> 0.05 & estWin>0.5]$moneyA)
sum(todayEstimates[timeUntil<0 & EV_A> 0.1 & estWin>0.5]$moneyA)
sum(todayEstimates[timeUntil<0 & EV_A> 0.15 & estWin>0.5]$moneyA)

sum(todayEstimates[timeUntil<0 & EV_A>0 & estWin>0.3]$moneyA)
sum(todayEstimates[timeUntil<0 & EV_A>0 & estWin>0.4]$moneyA)
sum(todayEstimates[timeUntil<0 & EV_A>0 & estWin>0.5]$moneyA)
sum(todayEstimates[timeUntil<0 & EV_A>0 & estWin>0.6]$moneyA)
sum(todayEstimates[timeUntil<0 & EV_A>0 & estWin>0.7]$moneyA)
sum(todayEstimates[timeUntil<0 & EV_A>0 & estWin>0.8]$moneyA)

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



