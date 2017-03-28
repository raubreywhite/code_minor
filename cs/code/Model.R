
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

ProcessData <- function(){
  f <- list.files(file.path(RPROJ$PROJRAW,"games"))
  if(gsub("\\.RDS","",max(f))<as.character(as.Date(lubridate::today()-1,origin="1970-01-01"))){
    GetLastWeekData()
    f <- list.files(file.path(RPROJ$PROJRAW,"games"))
  }
  
  d <- vector("list",length(f))
  for(i in 1:length(f)){
    d[[i]] <- readRDS(file.path(RPROJ$PROJRAW,"games",f[i]))
  }
  d <- rbindlist(d)
  d[,href:=NULL]
  d <- rbind(d,GetToday())
  d[,teamA:=gsub("[ ]*$","",teamA)]
  d[,teamA:=gsub("^[ ]*","",teamA)]
  d[,teamB:=gsub("[ ]*$","",teamB)]
  d[,teamB:=gsub("^[ ]*","",teamB)]
  
  d[,teamB:=gsub(" \\([0-9]-[0-9]\\)$","",teamB)]
  
  d[,teamA:=toupper(teamA)]
  d[,teamB:=toupper(teamB)]
  
  d[,identifier:=paste0(teamA,teamB)]
  d[teamA<teamB,identifier:=paste0(teamB,teamA)]
  d[,matchNum:=.N:1,by=.(identifier,Date)]
  
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
  d <- melt.data.table(d,id=c("Date","gameNum","teamA","teamB","matchNum"))
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
  return(d)
}

ProcessOdds <- function(){
  f <- list.files(file.path(RPROJ$PROJRAW,"odds"))
  d <- vector("list",length(f))
  for(i in 1:length(f)){
    d[[i]] <- readRDS(file.path(RPROJ$PROJRAW,"odds",f[i]))
  }
  d <- rbindlist(d)
  
  o <- GetOdds()[Date<=lubridate::today()]
  d <- rbind(o,d)
  d <- unique(d,by=c("teamA","teamB","time"))
  
  d[,teamA:=toupper(teamA)]
  d[,teamB:=toupper(teamB)]
  
  d[,identifier:=paste0(teamA,teamB)]
  d[teamA<teamB,identifier:=paste0(teamB,teamA)]
  d[,matchNum:=.N:1,by=.(identifier,Date)]
  
  d[,identifier:=NULL]

  b2 <- copy(d)
  b2[,teamA:=d$teamB]
  b2[,teamB:=d$teamA]
  b2[,oddsA:=d$oddsB]
  b2[,oddsB:=d$oddsA]
  d <- rbind(d,b2)
  return(d)
}

RunModelInternal3 <- function(dataframe, model=NULL, stanExe=NULL){
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
  #return(3)
  if(!is.null(stanExe)){
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
  } else {
    
    trash <- capture.output(fit <-  suppressMessages(suppressWarnings(stan(file=paste0(model,".stan"), seed=4, chains=4, iter=5000, data=list(
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
    saveRDS(fit, paste0(model,".RDS"))
  }
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


RunModelInternal4 <- function(dataframe, model=NULL, stanExe=NULL){
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
  #return(3)
  if(!is.null(stanExe)){
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
  } else {
    
    trash <- capture.output(fit <-  suppressMessages(suppressWarnings(stan(file=paste0(model,".stan"), seed=4, chains=4, iter=5000, data=list(
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
    saveRDS(fit, paste0(model,".RDS"))
  }
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
        sims$homeAdvantage[s]+
        simsA[s,team1All] - 
        simsA[s,team2All]) + 
      rt(ngamesAll,df)*sims$sigma_y[s]
  }
  res <- apply(random_outcome,2,function(x){mean(x>0)})
  d[,estWin:=res]
  return(d[(ngames+1):ngamesAll])
}

