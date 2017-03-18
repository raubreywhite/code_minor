GetOdds <- function(){
  a <- jsonlite::fromJSON("https://egb.com/bets?st=0&ut=0&fg=1&f=",flatten=T)
  b <- data.table(a$bets)
  b <- b[game=="Counter-Strike"]
  b[,time:=as.POSIXct(date, origin="1970-01-01")]
  b[,now:=as.POSIXct(a$user_time, origin="1970-01-01")]
  b[,timeUntil:=as.numeric(difftime(time,now,units="mins"))]
  b[,Date:=format(time,"%Y-%m-%d")]
  b <- b[,c("Date","timeUntil","live","gamer_1.nick","gamer_2.nick","coef_1","coef_2"),with=F]
  b[live==TRUE,gamer_1.nick:=stringr::str_replace_all(gamer_1.nick," \\(Live\\)$","")]
  b[live==TRUE,gamer_2.nick:=stringr::str_replace_all(gamer_2.nick," \\(Live\\)$","")]
  b[,gamer_1.nick:=stringr::str_replace_all(gamer_1.nick," \\(Map [0-9]\\)$","")]
  b[,gamer_2.nick:=stringr::str_replace_all(gamer_2.nick," \\(Map [0-9]\\)$","")]
  
}

GetUpcoming <- function(){
  f <- tempfile()
  system(sprintf("curl -sS 'http://www.hltv.org/matches/' > %s",f))
  a <- XML::htmlParse(f)
  tableNodes = XML::getNodeSet(a, "//div")
  classes <- unlist(sapply(tableNodes,function(x){
    retval <- XML::xmlGetAttr(x,"class")[[1]]
    if(is.null(retval)) retval <- "x"
    return(retval)
  }))
  dates <- which(classes=="matchListDateBox")
  team1 <- which(classes=="matchTeam1Cell")
  team2 <- which(classes=="matchTeam2Cell")
  res <- data.frame("Date"=rep(NA,100),"teamA"=NA,"teamB"=NA)
  for(j in 1:length(team1)){
    if(team1[j]>dates[3]) break
    matches <- tableNodes[[team1[j]]]
    vals <- XML::xmlValue(matches)
    vals <- gsub("\n","",vals)
    vals <- gsub("[ ]*$","",vals)
    res$teamA[j] <- vals
  }
  for(j in 1:length(team2)){
    if(team2[j]>dates[3]) break
    matches <- tableNodes[[team2[j]]]
    vals <- XML::xmlValue(matches)
    vals <- gsub("\n","",vals)
    vals <- gsub("[ ]*$","",vals)
    res$teamB[j] <- vals
  }
  for(i in 1:2){
    
  }
  
  
  
  data <- XML::readHTMLTable(f)
  
}