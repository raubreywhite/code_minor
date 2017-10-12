RAWmisc::AllowFileManipulationFromInitialiseProject()
RAWmisc::InitialiseProject(
  HOME = "/git/code_minor/2017/ulf_path_analysis/",
  RAW = "/analyses/data_raw/code_minor/2017/ulf_path_analysis/",
  CLEAN = "/analyses/data_clean/code_minor/2017/ulf_path_analysis",
  BAKED = "/analyses/results_baked/code_minor/2017/ulf_path_analysis/",
  FINAL = "/analyses/results_final/code_minor/2017/ulf_path_analysis/",
  SHARED = "/dropbox/results_shared/code_minor/2017/ulf_path_analysis/"
)

fileSources = file.path("code", list.files("code", pattern = "*.[rR]$"))
sapply(fileSources, source, .GlobalEnv)

library(data.table)
library(ggplot2)
library(mice)

Colours <- function(n){
  retval <- c()
  for(i in 1:ceiling(n/8)){
    if(i==1){
      retval <- c(retval,RColorBrewer::brewer.pal(8,"Set2"))
    } else {
      retval <- c(retval,RColorBrewer::brewer.pal(8,"Set1"))
    }
  }
  return(retval[1:n])
}

pop <- data.table(readxl::read_excel(file.path(RAWmisc::PROJ$RAW,"livebirths_by_year.xlsx")))
setnames(pop,c("FODAR","pop"))
pop[,FODAR:=as.numeric(FODAR)]

masterdata <- fread(file.path(RAWmisc::PROJ$RAW,"pathData20170717.csv"))

weights <- masterdata[,.(N=.N),by=.(TYPE,FODAR)]
weights[,denom:=sum(N),by=FODAR]
weights <- weights[TYPE=="CONTROL"]
weights[,propControl:=N/denom]
weights[,cases:=denom-N]
setnames(weights,"N","controls")
weights <- merge(weights,pop,by="FODAR")
weights[,controlsInPop:=pop-cases]
weights[,weight:=controlsInPop/controls]
weights <- weights[,c("FODAR","pop","cases","controls","controlsInPop","weight")]

openxlsx::write.xlsx(weights,file=file.path(RAWmisc::PROJ$SHARED_TODAY,"weights.xlsx"))

masterdata <- merge(masterdata,weights[,c("FODAR","weight")],by="FODAR")
masterdata[TYPE=="CASE",weight:=1]

nrow(masterdata)
xtabs(~masterdata$n_maltreatmentSyndrome)


#n_harnskakning

outcome <- "n_maltreatmentSyndrome"
varMaster <- c(
  "n_markerIntracranial",
  "n_retinalBlodning",
  "n_subduralBlodning",
  "n_skallfraktur",
  "n_hjarnskakning",
  "n_kramperAlla",
  "n_markerLongbone",
  "n_RevbenAlla",
  "motherSwedish",
  "new_preterm",
  "new_small"
)

locations <- c("sweden","new_southern","new_southEast","new_western","new_uppsalaOrebro","new_stockholm","new_northern")

masterdata[,location:=as.character(NA)]
for(l in locations){
  if(l=="sweden") next
  masterdata[get(l)==1,location:=l]
}


d <- copy(masterdata)
d <- na.omit(d[FODAR>=2001, c(outcome,varMaster,"location","period","FODAR","weight"), with=F])
q <- AttributableMixed(d,varMaster,outcome)
RAWmisc::saveA4(q[["cases"]],filename=file.path(RAWmisc::PROJ$SHARED_TODAY,"2001_2014_cases.png"))
RAWmisc::saveA4(q[["percentage"]],filename=file.path(RAWmisc::PROJ$SHARED_TODAY,"2001_2014_percentage.png"))

d <- copy(masterdata)
d <- na.omit(d[FODAR>=2008, c(outcome,varMaster,"location","period","FODAR","weight"), with=F])
q <- AttributableMixed(d,varMaster,outcome)
RAWmisc::saveA4(q[["cases"]],filename=file.path(RAWmisc::PROJ$SHARED_TODAY,"2008_2014_cases.png"))
RAWmisc::saveA4(q[["percentage"]],filename=file.path(RAWmisc::PROJ$SHARED_TODAY,"2008_2014_percentage.png"))


if(FALSE) for(l in locations){
  d <- copy(masterdata)
  d <- na.omit(d[FODAR>=2001, c(outcome,varMaster,locations[-1],"period","FODAR","weight"), with=F])
  if(l!="sweden"){
    d <- d[get(l)==1]
  }
  q <- Attributable(d,varMaster,outcome)
  RAWmisc::saveA4(q,filename=file.path(RAWmisc::PROJ$SHARED_TODAY,sprintf("attributable_%s.png",l)))
}


