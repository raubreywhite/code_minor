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

stan_run <- function(stanModel, ...) {
  if(class(stanModel) == "stanfit") {
    stanExe <- stanModel
  } else {
    stanModel.rda <- gsub("stan$", "rda", stanModel)
    if(!file.exists(stanModel.rda) || file.info(stanModel.rda)$mtime < file.info(stanModel)$mtime) {
      cat("Model",stanModel,"needs recompilation.\n")
      args <- modifyList(list(...), list(file=stanModel, iter=0, warmup=0, chains=0))
      stanExe <- do.call(stan, args)
      saveRDS(stanExe, file=stanModel.rda)
    } else {
      cat("Loading cached stan model", stanModel, ".\n")
      stanExe = readRDS(stanModel.rda)
    }
  }
  # This bit with the seed is for debugging purposes; once we figure out why Stan is crashing R we can remove it.
  seed <- sample.int(.Machine$integer.max, 1)
  write (seed, file="stan_seed.txt")
  stan(fit=stanExe, seed=seed, ...)
}

f <- list.files(file.path(RPROJ$PROJRAW))
d <- vector("list",length(f))
for(i in 1:length(f)) d[[i]] <- readRDS(file.path(RPROJ$PROJRAW,f[i]))
d <- rbindlist(d)
d[,href:=NULL]

blah <- copy(d)
blah[,winA:=0]
blah[,winB:=0]
blah[scoreA>scoreB,winA:=1]
blah[scoreA<scoreB,winB:=1]
blah[,scoreA:=NULL]
blah[,scoreB:=NULL]
blah[,scores:=NULL]
a <- blah[,c("Date","gameNum","teamA","winA"),with=F]
b <- blah[,c("Date","gameNum","teamB","winB"),with=F]
names(a) <- c("Date","gameNum","team","win")
names(b) <- names(a)
a[,type:="a"]
b[,type:="b"]
blah <- rbind(a,b)
setorder(blah,team,-Date,gameNum)
blah[,lastWin:=shift(win,type="lead"),by=team]
blah[,firstObs:=0]
blah[is.na(lastWin),firstObs:=1]
blah[is.na(lastWin),lastWin:=0]
blah[,team:=NULL]
blah[,win:=NULL]
blah <- dcast.data.table(blah,Date+gameNum~type,value.var=c("lastWin","firstObs"))

d[,games:=stringr::str_count(scores,",")+1]
numGames <- max(d$games)
d[,paste0("S",1:numGames) := tstrsplit(scores, ",", fixed=TRUE)]
d[is.na(S1),S1:=paste0(scoreA,":",scoreB)]
d[,scoreA:=NULL]
d[,scoreB:=NULL]
d[,scores:=NULL]
d[,games:=NULL]
d <- melt.data.table(d,id=c("Date","gameNum","teamA","teamB"))
d[,c("score1","score2") := tstrsplit(value, ":", fixed=TRUE)]
d[,score1:=as.numeric(score1)]
d[,score2:=as.numeric(score2)]
d[,scoreDif:=score1-score2]
d <- na.omit(d)
d[,numA:=0]
d[,numB:=0]

d[,win:=0]
d[scoreDif>0,win:=1]

d <- merge(d,blah,by=c("Date","gameNum"))

RunModel <- function(dataframe){
  library(data.table)
  library(rstan)
  d <- data.table(dataframe)
  
  teams <- teamsFit <- sort(unique(c(d[Date!=max(Date)]$teamA,d[Date!=max(Date)]$teamB)))
  teamsPred <- sort(unique(c(d[Date==max(Date)]$teamA,d[Date==max(Date)]$teamB)))
  teamsAll <- unique(c(teamsFit,teamsPred))
  length(teamsFit)
  length(teamsAll)
  nteams <- length(teams)
  ngames <- nrow(d[Date!=max(Date)])
  ngamesAll <- nrow(d)
  
  team1 <- match (d[Date!=max(Date)]$teamA, teams)
  score1 <- as.vector(d[Date!=max(Date)]$score1)
  team2 <- match (d[Date!=max(Date)]$teamB, teams)
  score2 <- as.vector(d[Date!=max(Date)]$score2)
  
  team1All <- match(d$teamA,teamsAll)
  team2All <- match(d$teamB,teamsAll)
  
  lastWin_a <- as.vector(d[Date!=max(Date)]$lastWin_a)
  lastWin_b <- as.vector(d[Date!=max(Date)]$lastWin_b)
  
  lastWin_aAll <- as.vector(d$lastWin_a)
  lastWin_bAll <- as.vector(d$lastWin_b)
  
  
  firstObs_a <- as.vector(d[Date!=max(Date)]$firstObs_a)
  firstObs_b <- as.vector(d[Date!=max(Date)]$firstObs_b)
  
  firstObs_aAll <- as.vector(d$firstObs_a)
  firstObs_bAll <- as.vector(d$firstObs_b)
  
  df <- 7
  
  data <- c("nteams","ngames","team1","score1","team2","score2","lastWin_a","lastWin_b","firstObs_a","firstObs_b","df")
  
  trash <- capture.output(fit <-  suppressMessages(suppressWarnings(stan(fit=stanExe, seed=4, chains=4, iter=5000, data=list(
    "nteams"=nteams,
    "ngames"=ngames,
    "team1"=team1,
    "score1"=score1,
    "team2"=team2,
    "score2"=score2,
    "lastWin_a"=lastWin_a,
    "lastWin_b"=lastWin_b,
    "firstObs_a"=firstObs_a,
    "firstObs_b"=firstObs_b,
    "df"=df),
    refresh=0
  ))))
  #fit <- stan_run("stan/worldcup_matt_3.stan", data=data, chains=4, iter=5000)
  #print(fit)
  
  sims <- extract(fit)
  a_sims <- sims$a
  a_hat <- colMeans(a_sims)
  a_se <- apply(a_sims,2,sd)
  
  
  nsims <- length(sims$sigma_y)
  ncols <- ncol(sims$a)
  extraCols <- length(teamsAll)-ncols
  
  #simsA <- cbind(sims$a,matrix(sample(c(sims$a),size=extraCols*nsims,replace=T),ncol=extraCols,nrow=nsims))
  simsA <- cbind(sims$a,matrix(0,ncol=extraCols,nrow=nsims))
  
  random_outcome <- array(NA, c(nsims,ngamesAll))
  for (s in 1:nsims){
    random_outcome[s,] <- (
      sims$est_lastWin_a[s]*lastWin_aAll+
      sims$est_lastWin_b[s]*lastWin_bAll+
      sims$est_firstObs_a[s]*firstObs_aAll+
      sims$est_firstObs_b[s]*firstObs_bAll+
      sims$homeAdvantage[s]+
      simsA[s,team1All] - 
      simsA[s,team2All]) + 
      rt(ngamesAll,df)*sims$sigma_y[s]
  }
  res <- apply(random_outcome,2,function(x){mean(x>0)})
  d[,estWin:=res]
  return(d[(ngames+1):ngamesAll])
}

dates <- unique(d$Date)
stanExe = readRDS("stan/worldcup_matt_3.rda")

options(mc.cores = parallel::detectCores())
today <- RunModel(as.data.frame(d[Date %in% dates[(length(dates)-199):length(dates)]]))
today[,c("gameNum","teamA","teamB","win","estWin"),with=F]
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



