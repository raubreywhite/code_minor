
GetScores <- function(href){
  system(sprintf("curl -sS '%s' > /git/code_minor/cs/test.txt",href))
  a <- XML::htmlParse("/git/code_minor/cs/test.txt")
  tableNodes = XML::getNodeSet(a, "//div")
  classes <- unlist(sapply(tableNodes,function(x){
    retval <- XML::xmlGetAttr(x,"class")[[1]]
    if(is.null(retval)) retval <- "x"
    return(retval)
  }))
  matches <- tableNodes[[which(classes=="hotmatchbox")[1]]]
  vals <- XML::xmlValue(matches)
  vals <- stringr::str_extract_all(vals,"[0-9]*:[0-9]*\n")[[1]]
  vals <- vals[vals!=":\n"]
  vals <- gsub("\n","",vals)
  vals <- paste0(vals,collapse=",")
  return(vals)
}

GetMatches <- function(date="2016-11-01"){
  fakeDate <- paste0(format.Date(as.Date(date),"%Y"),"-",
                     gsub("^0","",format.Date(as.Date(date),"%m")),"-",
                     format.Date(as.Date(date),"%d"))
  system(sprintf("curl -sS 'http://www.hltv.org/?pageid=324&filter=1&clean=1' --data 'vod=false&intersect=false&highlight=false&stats=false&demo=false&offset=0&daterange=%s+to+%s' > /git/code_minor/cs/test.txt",fakeDate,fakeDate))
  
  data <- XML::readHTMLTable("/git/code_minor/cs/test.txt")[[1]]
  data <- data[,1:2]
  for(i in 1:2){
    data[,i] <- as.character(data[,i])
  }
  data[,1] <- date
  data$teamA <- ""
  data$teamB <- ""
  data$scoreA <- as.numeric(gsub("\\(","",stringr::str_extract(data$Teams,"\\([0-9]*")))
  data$scoreB <- as.numeric(gsub("\\)","",stringr::str_extract(data$Teams,"[0-9]*\\)")))
  data$temp <- stringr::str_extract(data$Teams,"[0-9]*-[0-9]*")
  data$Teams <- apply(data,1,function(x){gsub(paste0(" \\(",x[7],"\\)"),"",x[2])})
  data$Teams <- gsub("  "," ",data$Teams)
  data$temp <- NULL
  x <- stringr::str_split(data$Teams," vs ")
  for(i in 1:nrow(data)){
    data$teamA[i] <- x[[i]][1]
    data$teamB[i] <- x[[i]][2]
  }
  data$Teams <- NULL
  
  a <- XML::htmlParse("/git/code_minor/cs/test.txt")
  tableNodes = XML::getNodeSet(a, "//table//td//a")
  
  data$scores <- ""
  data$href <- ""
  for(i in 1:nrow(data)){
    data$href[i] <- paste0("http://www.hltv.org/",XML::xmlGetAttr(tableNodes[[i*2]], "href"))
    if(data$scoreA[i] + data$scoreB[i]<5){
      data$scores[i] <- GetScores(data$href[i])
    }
  }
  data$gameNum <- nrow(data):1
  return(data)
}

GetData <- function(){
  today <- lubridate::today()
  days <- as.character(as.Date(as.Date("2013-01-01"):(today-1),origin="1970-01-01"))
  for(day in days){
    if(file.exists(file.path(RPROJ$PROJRAW,paste0(day,'.RDS')))) next
    print(day)
    try({
      d <- GetMatches(date=day)
      print(d)
      saveRDS(d,file=file.path(RPROJ$PROJRAW,paste0(day,'.RDS')))
      print("Success")
    },TRUE)
  }
}

UpdateLastWeekData <- function(){
  today <- lubridate::today()
  days <- as.character(as.Date((today-7):(today-1),origin="1970-01-01"))
  for(day in days){
    if(file.exists(file.path(RPROJ$PROJRAW,"games",paste0(day,'.RDS')))) next
    print(day)
    try({
      d <- GetMatches(date=day)
      print(d)
      saveRDS(d,file=file.path(RPROJ$PROJRAW,"games",paste0(day,'.RDS')))
      print("Success")
    },TRUE)
  }
}

