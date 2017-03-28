
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
    if(team1[j]<dates[2]){
      res$Date[j] <- as.character(lubridate::today())
    } else {
      res$Date[j] <- as.character(as.Date(lubridate::today()+1,origin="1970-01-01"))
    }
    matches <- tableNodes[[team1[j]]]
    vals <- XML::xmlValue(matches)
    vals <- gsub("\n","",vals)
    vals <- gsub("[ ]*$","",vals)
    vals <- gsub("^[ ]*","",vals)
    res$teamA[j] <- vals
  }
  for(j in 1:length(team2)){
    if(team2[j]>dates[3]) break
    matches <- tableNodes[[team2[j]]]
    vals <- XML::xmlValue(matches)
    vals <- gsub("\n","",vals)
    vals <- gsub("[ ]*$","",vals)
    vals <- gsub("^[ ]*","",vals)
    res$teamB[j] <- vals
  }
  
  res <- na.omit(res)
  res$scoreA <- 1
  res$scoreB <- 0
  res$scores <- "1:0"
  res <- res[nrow(res):1,]
  res$gameNum <- nrow(res):1
  return(res[res$Date==min(res$Date),])
}

GetToday <- function(){
  a <- data.table(GetUpcoming())
  try({
    b <- GetMatches(date=as.character(lubridate::today()))
    if(nrow(b)>0){
      b <- data.table(b)
      b[,href:=NULL]
      a <- rbind(a,b)
    }
  },TRUE)
  a[,gameNum:=.N:1]
  return(a)
}