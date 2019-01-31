AnalysesFigure3Inger <- function(d,tag="normal"){
  assign("modelData",d[!is.na(First_pregnancy_all) & group==1], envir=.GlobalEnv)
  fit <- survival::coxph(survival::Surv(Time_to_first_pregnancy, First_pregnancy_all) ~ 
                           Age_diagnosis_cat + 
                           birth_periods +
                           Country_of_birth +
                           Education,
                         data = modelData)
  fit
  tab <- CoxToDF(fit)
  openxlsx::write.xlsx(tab,file.path(org::PROJ$SHARED_TODAY,
                                     "F3",
                                     tag,
                                     "hr_F3_time_to_first_preg_pcos_only.xlsx"))
  
  # KM curves and cloglog
  pd <- list()
  for(v in c("Age_diagnosis_cat","birth_periods","Country_of_birth", "Education")){
    s <- survminer::surv_summary(survival::survfit(
      as.formula(
        sprintf("survival::Surv(Time_to_first_pregnancy, First_pregnancy_all) ~ %s",v)
      ), data=modelData))
    setDT(s)
    s[,y:=log(-log(surv))]
    s[,x:=log(time)]
    s[,var:=v]
    pd[[v]] <- s
  }
  pd <- rbindlist(pd)
  q <- ggplot(pd,aes(x=x,y=y,colour=strata))
  q <- q + geom_line()
  q <- q + facet_wrap(~var)
  q <- q + scale_x_continuous("log(t)")
  q <- q + scale_y_continuous("log(-log(s(t)))")
  RAWmisc::saveA4(q,file.path(org::PROJ$SHARED_TODAY,
                              "F3",
                              tag,
                              "loglog_F2_time_to_first_preg_pcos_only.png"))
  
  ## survival curves from cox model
  
  agg <- na.omit(modelData[,.(
    N=.N
  ),by=.(
    Age_diagnosis_cat,
    birth_periods,
    Country_of_birth,
    Education
  )])
  agg[,strata:=1:.N]
  
  aggSurv <- survminer::surv_summary(survival::survfit(fit, newdata=agg))
  nrow(aggSurv)
  aggSurv <- merge(aggSurv, agg, by="strata")
  nrow(aggSurv)
  setDT(aggSurv)
  aggSurv[,numAlive:=N*surv]
  
  # Age_diagnosis_cat
  res <- aggSurv[,.(
    N=sum(N),
    numAlive=sum(numAlive)
  ),keyby=.(
    Age_diagnosis_cat,
    time
  )]
  res[,surv:=numAlive/N]
  res <- haven::zap_labels(res)
  
  q <- ggplot(res,aes(x=time,y=(1-surv),colour=factor(Age_diagnosis_cat)))
  q <- q + geom_step(direction = "hv")
  q <- q + scale_y_continuous(lim=c(0,1),breaks=seq(0,1,0.2))
  q
  RAWmisc::saveA4(q,file.path(org::PROJ$SHARED_TODAY,
                              "F3",
                              tag,
                              "surv_F3_Age_diagnosis_cat.png"))
  
  # birth_periods
  res <- aggSurv[,.(
    N=sum(N),
    numAlive=sum(numAlive)
  ),keyby=.(
    birth_periods,
    time
  )]
  res[,surv:=numAlive/N]
  res <- haven::zap_labels(res)
  
  q <- ggplot(res,aes(x=time,y=(1-surv),colour=factor(birth_periods)))
  q <- q + geom_step(direction = "hv")
  q <- q + scale_y_continuous(lim=c(0,1),breaks=seq(0,1,0.2))
  q
  RAWmisc::saveA4(q,file.path(org::PROJ$SHARED_TODAY,
                              "F3",
                              tag,
                              "surv_F3_birth_periods.png"))
  
  # Country_of_birth
  res <- aggSurv[,.(
    N=sum(N),
    numAlive=sum(numAlive)
  ),keyby=.(
    Country_of_birth,
    time
  )]
  res[,surv:=numAlive/N]
  res <- haven::zap_labels(res)
  
  q <- ggplot(res,aes(x=time,y=(1-surv),colour=factor(Country_of_birth)))
  q <- q + geom_step(direction = "hv")
  q <- q + scale_y_continuous(lim=c(0,1),breaks=seq(0,1,0.2))
  q
  RAWmisc::saveA4(q,file.path(org::PROJ$SHARED_TODAY,
                              "F3",
                              tag,
                              "surv_F3_Country_of_birth.png"))
  
  # education
  res <- aggSurv[,.(
    N=sum(N),
    numAlive=sum(numAlive)
  ),keyby=.(
    Education,
    time
  )]
  res[,surv:=numAlive/N]
  res <- haven::zap_labels(res)
  
  q <- ggplot(res,aes(x=time,y=(1-surv),colour=factor(Education)))
  q <- q + geom_step(direction = "hv")
  q <- q + scale_y_continuous(lim=c(0,1),breaks=seq(0,1,0.2))
  q
  RAWmisc::saveA4(q,file.path(org::PROJ$SHARED_TODAY,
                              "F3",
                              tag,
                              "surv_F3_education.png"))
  
}