GetOdds <- function(){
  a <- jsonlite::fromJSON("https://egb.com/bets?st=0&ut=0&fg=1&f=",flatten=T)
  b <- data.table(a$bets)
  b <- b[game=="Counter-Strike"]
  b[,time:=as.POSIXct(date, origin="1970-01-01")]
  b[,now:=as.POSIXct(a$user_time, origin="1970-01-01")]
  b[,timeUntil:=as.numeric(difftime(time,now,units="mins"))]
  b[,Date:=format(time,"%Y-%m-%d")]
  b <- b[live==FALSE,c("Date","time","timeUntil","live","gamer_1.nick","gamer_2.nick","coef_1","coef_2"),with=F]
  b[,gamer_1.nick:=stringr::str_replace_all(gamer_1.nick," \\([mM]ap [0-9]\\)$","")]
  b[,gamer_2.nick:=stringr::str_replace_all(gamer_2.nick," \\([mM]ap [0-9]\\)$","")]
  setnames(b,c("gamer_1.nick","gamer_2.nick","coef_1","coef_2"),c("teamA","teamB","oddsA","oddsB"))
  b[,oddsA:=as.numeric(oddsA)]
  b[,oddsB:=as.numeric(oddsB)]
  
  b <- b[,.(oddsA=mean(oddsA),oddsB=mean(oddsB),time=min(time),timeUntil=min(timeUntil)),by=.(teamA,teamB,Date)]
  setorder(b,-time)
  
  b[,teamA:=stringr::str_replace_all(teamA," \\(Game [0-9]\\)$","")]
  b[,teamB:=stringr::str_replace_all(teamB," \\(Game [0-9]\\)$","")]
  
  return(b)
}

UpdateOdds <- function(){
  o <- GetOdds()
  o <- o[timeUntil < -10]
  for(i in unique(o$Date)){
    f <- file.path(RPROJ$PROJRAW,"odds",paste0(i,".RDS"))
    nd <- o[Date==i]
    if(file.exists(f)){
      od <- readRDS(file.path(RPROJ$PROJRAW,"odds",paste0(i,".RDS")))
      md <- rbind(nd,od)
      md <- unique(md,by=c("teamA","teamB","time"))
      saveRDS(md,file.path(RPROJ$PROJRAW,"odds",paste0(i,".RDS")))
    } else {
      saveRDS(nd,file.path(RPROJ$PROJRAW,"odds",paste0(i,".RDS")))
    }
  }
}