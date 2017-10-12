RAWmisc::AllowFileManipulationFromInitialiseProject()
RAWmisc::InitialiseProject(
  HOME = "/git/code_minor/2017/ulf_path_analysis/",
  RAW = "/analyses/data_raw/code_minor/2017/ulf_path_analysis/",
  CLEAN = "/analyses/data_clean/code_minor/2017/ulf_path_analysis",
  BAKED = "/analyses/results_baked/code_minor/2017/ulf_path_analysis/",
  FINAL = "/analyses/results_final/code_minor/2017/ulf_path_analysis/",
  SHARED = "/dropbox/results_shared/code_minor/2017/ulf_path_analysis/"
)
dir.create(RAWmisc::PROJ$SHARED_TODAY)

library(data.table)
library(ggplot2)
library(mice)

masterdata <- data.table(openxlsx::read.xlsx(file.path(RAWmisc::PROJ$RAW,"pathData20170707.xlsx")))

nrow(masterdata)
xtabs(~masterdata$n_maltreatmentSyndrome)

outcome <- "n_maltreatmentSyndrome"
var <- c(
  "n_markerIntracranial",
  "n_retinalBlodning",
  "n_subduralBlodning",
  "n_skallfraktur",
  
  "n_kramper",
  "n_markerLongbone",
  "n_frakturLarben",
  "n_frakturRevben",
  "motherSwedish",
  "new_preterm",
  "new_small",
  "new_southern",
  "new_southEast",
  "new_western",
  "new_uppsalaOrebro",
  "new_stockholm",
  "new_northern"
)

d <- copy(masterdata)
d <- na.omit(d[period!=">2013", c(outcome,var,"period"), with=F])

xtabs(~d$period+d$n_maltreatmentSyndrome)

interactions <- vector("list",length(var))
for(i in 1:length(var)){
  fit <- glm(
    as.formula(
      sprintf("%s~%s+period:%s",outcome,paste0(var,collapse="+"),var[i])
    ), data=d
  )
  x <- as.data.frame(coef(summary(fit)))
  x$var <- row.names(x)
  interactions[[i]] <- x[nrow(x),]
}
interactions <- rbindlist(interactions)
setnames(interactions,c("est","se","t","p","var"))
interactions[,var:=stringr::str_replace(var,":period2008_2013","")]
interactions[p<0.05]

res <- vector("list",length(var)*3)
for(j in 1:3){
  if(j==1){
    fitData <- d[period=="1997_2007"]
    modelPeriod <- "1997_2007"
  } else if(j==2){
    fitData <- d[period=="2008_2013"]
    modelPeriod <- "2008_2013"
  } else {
    fitData <- d
    modelPeriod <- "1997_2013"
  }
  
  fit <- glm(
    as.formula(
      sprintf("%s~%s",outcome,paste0(var,collapse="+"))
    ), data=fitData
  )
  for(i in 1:length(var)){
    index <- (i-1)*3+j
    
    p1 <- predict(fit,fitData,type="response")
    dx <- copy(fitData)
    dx[[var[i]]] <- 0
    p0 <- predict(fit,dx,type="response")
    dx[,p1:=p1]
    dx[,p0:=p0]
    res[[index]] <- dx[,.(p1=sum(p1,na.rm=T),p0=sum(p0,na.rm=T)),by=.(period)]
    res[[index]][,var:=var[i]]
    x <- as.data.frame(coef(summary(fit)))
    x$var <- row.names(x)
    pval <- x[which(x$var==var[i]),4]
    
    res[[index]][,pval := pval]
    res[[index]][,modelPeriod:=modelPeriod]
  }
}
res <- rbindlist(res)

res[,usePeriodSpecific:=0]
res[var %in% interactions[p<0.05]$var, usePeriodSpecific:=1]
res[,useAttribution:=0]
res[pval<0.05 & modelPeriod=="1997_2013" & usePeriodSpecific==0, useAttribution:=1]
res[pval<0.05 & modelPeriod!="1997_2013" & usePeriodSpecific==1, useAttribution:=1]
res[,attr:=0]
res[useAttribution==1,attr:=abs(p1-p0)]

resx <- res[,.(attr=sum(attr,na.rm=T),p1=mean(p1,na.rm=T)),by=.(period)]
resx[,var:="UNEXPLAINED"]
resx[,attr:=p1-attr]
resx[attr<0,attr:=0]
resx[,usePeriodSpecific:=0]
res2 <- res[,c("period","var","attr","usePeriodSpecific")]
resx <- resx[,names(res2),with=F]
res2 <- rbind(res2,resx)
res2[,total:=sum(attr),by=.(period,usePeriodSpecific)]

res2[,keep:=sum(attr),by=var]
res2 <- res2[keep>0]

Colours <- function(n){
  retval <- c()
  for(i in 1:(n%%8)){
    if(i==1){
      retval <- c(retval,RColorBrewer::brewer.pal(8,"Set2"))
    } else {
      retval <- c(retval,RColorBrewer::brewer.pal(8,"Set1"))
    }
  }
  return(retval[1:n])
}

saveA4 <- function(q,filename,landscape=T){
  ggsave(filename,plot=q,width=297,height=210, units="mm")
}

q <- ggplot(res2,aes(x=var,fill=period,y=attr))
q <- q + geom_bar(stat="identity",colour="black",pos="dodge",alpha=0.75)
q <- q + geom_text(data=res2[usePeriodSpecific==1],mapping=aes(y=60),label="Different\nassociation")
q <- q + scale_fill_brewer(palette="Set1")
q <- q + scale_y_continuous("Number attributable to each risk factor")
q <- q + theme_grey(16)
q <- q + theme(axis.text.x= element_text(angle=90,hjust=1,vjust=0.5))
saveA4(q,file.path(RAWmisc::PROJ$SHARED_TODAY,"risk_factors.png"))



