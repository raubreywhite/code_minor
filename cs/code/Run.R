RunWithOdds <- function(d,o,RunModel,model=NULL,stanExe=NULL,today=F){
  dates <- unique(d$Date)
  
  if(RUN_ALL){
    unlink(paste0(model,".RDS"))
  }
  if(is.null(stanExe)){
    if(!file.exists(paste0(model,".RDS"))){
      print("Compiling model")
      RunModel(dataframe=as.data.frame(d[Date %in% dates[(100-9):(100)]]),model=model)  
    }
    stanExe = readRDS(paste0(model,".RDS"))
  } 
  
  numDatesWithOdds <- length(unique(o$Date))
  if(today) numDatesWithOdds <- 1
  
  options(mc.cores = parallel::detectCores())
  today <- foreach(i=(length(dates)-(numDatesWithOdds-1)):length(dates)) %do% RunModel(as.data.frame(d[Date %in% dates[(i-299):(i)]]), stanExe=stanExe)
  options(mc.cores = 1)
  todayEstimates <- rbindlist(today)
  todayEstimates <- todayEstimates[,
                                   .(estWin=mean(estWin),
                                     winsA=sum(win),
                                     winsB=sum(1-win)),
                                   by=.(Date,gameNum,matchNum,teamA,teamB)]
  
  #today <- RunModel(as.data.frame(d[Date %in% dates[(length(dates)-199):length(dates)]]))
  todayEstimates[,estOdds:=1/estWin]
  
  todayEstimates <- merge(todayEstimates,o,by=c("Date","teamA","teamB","matchNum"))
  
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
  todayEstimates[,kellyA:=round(0.5*(EV_A/(oddsA-1)),3)]
  
  todayEstimates[,kellyB:=round(0.5*(EV_B/(oddsB-1)),3)]
  
  return(todayEstimates)
}

RunWithoutOdds <- function(d,RunModel,model=NULL,stanExe=NULL,suffix="_1"){
  dates <- unique(d$Date)
  options(mc.cores = 1)
  
  if(RUN_ALL){
    unlink(paste0(model,".RDS"))
  }
  if(is.null(stanExe)){
    if(!file.exists(paste0(model,".RDS"))){
      print("Compiling model")
      RunModel(dataframe=as.data.frame(d[Date %in% dates[(100-9):(100)]]),model=model)  
    }
    stanExe = readRDS(paste0(model,".RDS"))
  } 
  
  print("Running in parallel")
  x <- foreach(i=199:400) %dopar% RunModel(dataframe=as.data.frame(d[Date %in% dates[(i-199):(i)]]), stanExe=stanExe)
  saveRDS(x,file=file.path(RPROJ$PROJCLEAN,paste0("est",suffix,".RDS")))
  x <- readRDS(file=file.path(RPROJ$PROJCLEAN,paste0("est",suffix,".RDS")))
  return(x)
}

  