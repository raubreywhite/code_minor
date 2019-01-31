AnalysesFigure2 <- function(d,tag="normal",isMixed=FALSE){
  
  if(isMixed){
    fn <- survival::coxph
    folder <- "mixed_effects"
    tagMixed <- "2mixed"
  } else {
    fn <- survival::coxph
    folder <- "original_analyses"
    tagMixed <- "1original"
  }
  
  # time to first preg
  print(1)
  print(Sys.time())
  form <- "survival::Surv(Time_to_first_pregnancy, First_pregnancy_all) ~ 
  group + 
  birth_periods +
  Country_of_birth +
  Education"
  if(isMixed) form <- sprintf("%s + survival::frailty.gaussian(lopnr_fall)",form)
  
  f1 <- Sys.time()
  assign("modelData",d[!is.na(First_pregnancy_all) & !is.na(Time_to_first_pregnancy)], envir=.GlobalEnv)
  fit <- fn(as.formula(form),data = modelData)
  saveRDS(fit,file.path(org::PROJ$SHARED_TODAY,
                        "F2",
                        tag,
                        sprintf("1model_F2_time_to_first_preg_all_%s.RDS", tagMixed)))
  f2 <- Sys.time()
  f2-f1
  print(fit)
  tab <- CoxToDF(fit)
  openxlsx::write.xlsx(tab,file.path(org::PROJ$SHARED_TODAY,
                                     "F2",
                                     tag,
                                     sprintf("1hr_F2_time_to_first_preg_all_%s.xlsx", tagMixed)))
  
  if(!isMixed){
    # survival curves
    agg <- na.omit(modelData[,.(
      N=.N
    ),by=.(
      group,
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
    
    res <- aggSurv[,.(
      N=sum(N),
      numAlive=sum(numAlive)
    ),keyby=.(
      group,
      time
    )]
    res[,surv:=numAlive/N]
    res <- haven::zap_labels(res)
    
    q <- ggplot(res,aes(x=time,y=(1-surv),colour=factor(group)))
    q <- q + geom_step(direction = "hv")
    q <- q + scale_y_continuous(lim=c(0,1),breaks=seq(0,1,0.2))
    q
    RAWmisc::saveA4(q,file.path(org::PROJ$SHARED_TODAY,
                                "F2",
                                tag,
                                sprintf("1surv_F2_time_to_first_preg_all_%s.png", tagMixed)))
    
    # KM curves and cloglog
    pd <- list()
    for(v in c("group","birth_periods","Country_of_birth", "Education")){
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
                                "F2",
                                tag,
                                sprintf("1loglog_F2_time_to_first_preg_all_%s.png", tagMixed)))
  }
  
  # time to first preg spontaneous
  print(2)
  print(Sys.time())
  form <- "survival::Surv(Time_to_first_pregnancy, First_spontaneous_pregnancy) ~ 
  group + 
  birth_periods +
  Country_of_birth +
  Education"
  if(isMixed) form <- sprintf("%s + survival::frailty.gaussian(lopnr_fall)",form)
  assign("modelData",d[!is.na(First_pregnancy_all) & !is.na(Time_to_first_pregnancy) & born>=1982], envir=.GlobalEnv)
  fit <- fn(as.formula(form),data = modelData)
  saveRDS(fit,file.path(org::PROJ$SHARED_TODAY,
                        "F2",
                        tag,
                        sprintf("2model_F2_time_to_first_preg_spontaneous_%s.RDS", tagMixed)))
                        
  print(fit)
  tab <- CoxToDF(fit)
  openxlsx::write.xlsx(tab,file.path(org::PROJ$SHARED_TODAY,
                                     "F2",
                                     tag,
                                     sprintf("2hr_F2_time_to_first_preg_spontaneous_%s.xlsx", tagMixed)))
  
  if(!isMixed){
    agg <- na.omit(modelData[,.(
      N=.N
    ),by=.(
      group,
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
    
    res <- aggSurv[,.(
      N=sum(N),
      numAlive=sum(numAlive)
    ),keyby=.(
      group,
      time
    )]
    res[,surv:=numAlive/N]
    res <- haven::zap_labels(res)
    
    q <- ggplot(res,aes(x=time,y=(1-surv),colour=factor(group)))
    q <- q + geom_step(direction = "hv")
    q <- q + scale_y_continuous(lim=c(0,1),breaks=seq(0,1,0.2))
    q
    RAWmisc::saveA4(q,file.path(org::PROJ$SHARED_TODAY,
                                "F2",
                                tag,
                                sprintf("2surv_F2_time_to_first_preg_spontaneous_%s.png", tagMixed)))
    
    # KM curves and cloglog
    pd <- list()
    for(v in c("group","birth_periods","Country_of_birth", "Education")){
      s <- survminer::surv_summary(survival::survfit(
        as.formula(
          sprintf("survival::Surv(Time_to_first_pregnancy, First_spontaneous_pregnancy) ~ %s",v)
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
                                "F2",
                                tag,
                                sprintf("2loglog_F2_time_to_first_preg_spontaneous_%s.png", tagMixed)))
  }
  
  ## time from first preg to second preg
  print(3)
  print(Sys.time())
  form <- "survival::Surv(time_to_second_pregnancy, Second_pregnancy) ~ 
  group + 
  as.factor(birth_periods) +
  as.factor(Country_of_birth) +
  Education"
  if(isMixed) form <- sprintf("%s + survival::frailty.gaussian(lopnr_fall)",form)
  assign("modelData",d[First_pregnancy_all==1 & Multiple_birth_first_pregnancy==1 & !is.na(time_to_second_pregnancy)], envir=.GlobalEnv)
  fit <- fn(as.formula(form),data = modelData)
  saveRDS(fit,file.path(org::PROJ$SHARED_TODAY,
                        "F2",
                        tag,
                        sprintf("3model_F2_time_from_first_preg_to_second_preg_%s.RDS", tagMixed)))
  print(fit)
  tab <- CoxToDF(fit)
  openxlsx::write.xlsx(tab,file.path(org::PROJ$SHARED_TODAY,
                                     "F2",
                                     tag,
                                     sprintf("3hr_F2_time_from_first_preg_to_second_preg_%s.xlsx", tagMixed)))
  
  if(!isMixed){
    agg <- na.omit(modelData[,.(
      N=.N
    ),by=.(
      group,
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
    
    res <- aggSurv[,.(
      N=sum(N),
      numAlive=sum(numAlive)
    ),keyby=.(
      group,
      time
    )]
    res[,surv:=numAlive/N]
    res <- haven::zap_labels(res)
    
    q <- ggplot(res,aes(x=time,y=(1-surv),colour=factor(group)))
    q <- q + geom_step(direction = "hv")
    q <- q + scale_y_continuous(lim=c(0,1),breaks=seq(0,1,0.2))
    q
    RAWmisc::saveA4(q,file.path(org::PROJ$SHARED_TODAY,
                                "F2",
                                tag,
                                sprintf("3surv_F2_time_from_first_preg_to_second_preg_%s.png", tagMixed)))
    
    # KM curves and cloglog
    pd <- list()
    for(v in c("group","birth_periods","Country_of_birth", "Education")){
      s <- survminer::surv_summary(survival::survfit(
        as.formula(
          sprintf("survival::Surv(time_to_second_pregnancy, Second_pregnancy) ~ %s",v)
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
                                "F2",
                                tag,
                                sprintf("3loglog_F2_time_from_first_preg_to_second_preg_%s.png", tagMixed)))
  }
}

