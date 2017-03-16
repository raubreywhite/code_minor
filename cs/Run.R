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
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

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

f <- list.files(file.path(RPROJ$PROJRAW))[1:200]
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

teams <- sort(unique(c(d$teamA,d$teamB)))
nteams <- length(teams)
nmatchesperteam <- rep(0,nteams)
prior_score <- rep(0,nteams)

for(i in 1:length(teams)){
  nmatchesperteam[i] <- sum(d$teamA==teams[i]) + sum(d$teamB==teams[i])
  d[teamA==teams[i],numA:=nmatchesperteam[i]]
  d[teamB==teams[i],numB:=nmatchesperteam[i]]
}

ngames <- nrow(d)

team1 <- match (d$teamA, teams)
score1 <- as.vector(d$score1)
team2 <- match (d$teamB, teams)
score2 <- as.vector(d$score2)

lastWin_a <- as.vector(d$lastWin_a)
lastWin_b <- as.vector(d$lastWin_b)

df <- 7

data <- c("nteams","ngames","team1","score1","team2","score2","lastWin_a","lastWin_b","df")

fit <- stan_run("stan/worldcup_matt_2.stan", data=data, chains=4, iter=5000)
print(fit)

sims <- extract(fit)
a_sims <- sims$a
a_hat <- colMeans(a_sims)
a_se <- apply(a_sims,2,sd)

new_order <- order(a_hat[team1] - a_hat[team2])

#q <- 
#png ("worldcup7.png", height=500, width=500)
#coefplot (rev(a_hat), rev(a_se), CI=1, varnames=rev(teams), main="Team quality (estimate +/- 1 s.e.)\n", cex.var=.9, mar=c(0,4,5.1,2))
#dev.off()

nsims <- length(sims$sigma_y)
random_outcome <- array(NA, c(nsims,ngames))
for (s in 1:nsims){
  random_outcome[s,] <- (
    sims$est_lastWin_a[s]*lastWin_a+
    sims$est_lastWin_b[s]*lastWin_b+
    sims$homeAdvantage[s]+
    sims$a[s,team1] - 
    sims$a[s,team2]) + 
    rt(ngames,df)*sims$sigma_y[s]
}
sim_quantiles <- array(NA,c(ngames,4))
for (i in 1:ngames){
  sim_quantiles[i,1:3] <- quantile(random_outcome[,i], c(.025,0.5,.975))
  sim_quantiles[i,4] <- mean(random_outcome[,i]>0)
}
d[,estDifL1:=sim_quantiles[,1]]
d[,estDif:=sim_quantiles[,2]]
d[,estDifL2:=sim_quantiles[,3]]
d[,estWin:=sim_quantiles[,4]]

summary(fit <- glm(win~splines::ns(estWin,df=3),family="binomial",data=d))
d[,estWin2:=1/(1+exp(-predict(fit,d)))]

d[,estWinBlock:=round(estWin2*10)/10]
res <- d[,.(n=.N,win=mean(win)),by=estWinBlock]
setorder(res,estWinBlock)
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



