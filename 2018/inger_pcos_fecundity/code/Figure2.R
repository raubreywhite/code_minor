AnalysesFigure2Int <- function(descriptive, modelD, tag, analysisName, isMixed, outcomeTime, outcomeEvent, censor_at_25){
  if(censor_at_25){
    censor_at_25_tag <- "censor_at_25"
  } else {
    censor_at_25_tag <- "not_censor_at_25"
  }
  
  if(isMixed){
    fn <- survival::coxph
    folder <- "mixed_effects"
    tagMixed <- "2mixed"
  } else {
    fn <- survival::coxph
    folder <- "original_analyses"
    tagMixed <- "1original"
  }
  
  dir.create(file.path(
    org::PROJ$SHARED_TODAY,
    censor_at_25_tag,
    "F2",
    tag,
    analysisName))
  
  cat(descriptive,"\n",
      file=file.path(
        org::PROJ$SHARED_TODAY,
        censor_at_25_tag,
        "F2",
        tag,
        analysisName,
        sprintf("DATA_%s.txt",tagMixed)))
  cat("n: ",nrow(modelD),
      file=file.path(
        org::PROJ$SHARED_TODAY,
        censor_at_25_tag,
        "F2",
        tag,
        analysisName,
        sprintf("DATA_%s.txt",tagMixed)), append=T)
  
  ####################
  ####################
  print(Sys.time())
  form <- sprintf("survival::Surv(%s, %s) ~ 
                  group + 
                  birth_periods +
                  Country_of_birth +
                  Education", outcomeTime, outcomeEvent)
  if(isMixed) form <- sprintf("%s + survival::frailty.gaussian(lopnr_fall)",form)
  
  f1 <- Sys.time()
  assign("modelData",modelD, envir=.GlobalEnv)
  fit <- fn(as.formula(form),data = modelData)
  saveRDS(fit,file.path(org::PROJ$SHARED_TODAY,
                        censor_at_25_tag,
                        "F2",
                        tag,
                        analysisName,
                        sprintf("data_%s.RDS", tagMixed)))
  f2 <- Sys.time()
  f2-f1
  print(fit)
  tab <- CoxToDF(fit)
  xlsx::write.xlsx(tab,file.path(org::PROJ$SHARED_TODAY,
                                 censor_at_25_tag,
                                 "F2",
                                 tag,
                                 analysisName,
                                 sprintf("hazardratio_%s.xlsx", tagMixed)),
                   row.names=F)
  
  if(!isMixed){
    # KM curves and cloglog
    pd <- list()
    km <- list()
    for(v in c("group","birth_periods","Country_of_birth", "Education")){
      s <- survminer::surv_summary(survival::survfit(
        as.formula(
          sprintf("survival::Surv(%s, %s) ~ %s",outcomeTime, outcomeEvent, v)
        ), data=modelData))
      setDT(s)
      s[,(v):=NULL]
      s[,y:=log(-log(surv))]
      s[,x:=log(time)]
      s[,var:=v]
      pd[[v]] <- s
      
      for(grp in 0:1){
        if(v=="group") next
        assign("modelDataGroup",modelData[modelData$group==grp,], envir=.GlobalEnv)
        s <- survminer::surv_summary(survival::survfit(
          as.formula(
            sprintf("survival::Surv(%s, %s) ~ %s",outcomeTime, outcomeEvent, v)
          ), data=modelDataGroup))
        setDT(s)
        s[,(v):=NULL]
        s[,y:=log(-log(surv))]
        s[,x:=log(time)]
        s[,var:=v]
        s[,group:=grp]
        km[[length(km)+1]] <- s
      }
    }
    pd <- rbindlist(pd)
    q <- ggplot(pd,aes(x=x,y=y,colour=strata))
    q <- q + geom_line()
    q <- q + facet_wrap(~var)
    q <- q + scale_x_continuous("log(t)")
    q <- q + scale_y_continuous("log(-log(s(t)))")
    q <- q + scale_color_discrete("")
    q <- q + theme_gray(16)
    RAWmisc::saveA4(q,file.path(org::PROJ$SHARED_TODAY,
                                censor_at_25_tag,
                                "F2",
                                tag,
                                analysisName,
                                sprintf("loglog_%s.png", tagMixed)))
    
    pdx <- pd[var=="group"]
    pdx[,group:=1]
    pdx[strata=="group=0",group:=0]
    km <- rbindlist(km)
    km <- rbind(pdx, km)
    
    km[,cumPregEst:=1-surv]
    km[,cumPregEstLower:=1-upper]
    km[,cumPregEstUpper:=1-lower]
    xlsx::write.xlsx(km[,c(
      "var",
      "strata",
      "group",
      "time",
      "n.risk",
      "n.event",
      "n.censor",
      "cumPregEst",
      "cumPregEstLower",
      "cumPregEstUpper")],
      file.path(org::PROJ$SHARED_TODAY,
                censor_at_25_tag,
                "F2",
                tag,
                analysisName,
                sprintf("KM_%s.xlsx", tagMixed)),
      row.names=F)
    
    plotting <- km[var=="group"]
    plotting[,labs:="Controls"]
    plotting[group==1,labs:="PCOS"]
  
    q <- ggplot(plotting,aes(x=time,y=cumPregEst,ymin=cumPregEstLower, ymax=cumPregEstUpper, fill=labs))
    q <- q + stat_stepribbon(alpha=0.5)
    q <- q + geom_step(direction="hv", mapping=aes(colour=labs))
    q <- q + scale_x_continuous("Time",lim=c(0,27),expand=c(0,0))
    q <- q + scale_y_continuous("Cumulative probability of fecundity",lim=c(0,1), expand = c(0, 0))
    q <- q + scale_color_brewer("",palette="Set1",direction = -1)
    q <- q + scale_fill_brewer("",palette="Set1",direction = -1)
    q <- q + fhiplot::theme_fhi_lines(18)
    RAWmisc::saveA4(q,file.path(org::PROJ$SHARED_TODAY,
                                censor_at_25_tag,
                                "F2",
                                tag,
                                analysisName,
                                sprintf("km_%s.png", tagMixed)))
    
  }
}

AnalysesFigure2 <- function(d,tag="normal",isMixed=FALSE, censor_at_25=FALSE){
  
  if(isMixed){
    fn <- survival::coxph
    folder <- "mixed_effects"
    tagMixed <- "2mixed"
  } else {
    fn <- survival::coxph
    folder <- "original_analyses"
    tagMixed <- "1original"
  }
  
  ####################
  ####################
  # time to first preg
  print(1)
  
  AnalysesFigure2Int(
    descriptive="!is.na(First_pregnancy_all) & !is.na(Time_to_first_pregnancy)",
    modelD=d[!is.na(First_pregnancy_all) & !is.na(Time_to_first_pregnancy)],
    tag=tag,
    analysisName="time_to_first_preg",
    isMixed=isMixed,
    outcomeTime="Time_to_first_pregnancy",
    outcomeEvent="First_pregnancy_all",
    censor_at_25=censor_at_25)
  
  ####################
  ####################
  # time to first preg spontaneous
  print(2)
  
  AnalysesFigure2Int(
    descriptive="!is.na(First_pregnancy_all) & !is.na(First_spontaneous_pregnancy) & born>=1977",
    modelD=d[!is.na(First_pregnancy_all) & !is.na(First_spontaneous_pregnancy) & born>=1977],
    tag=tag,
    analysisName="time_to_first_spontaneous",
    isMixed=isMixed,
    outcomeTime="Time_to_first_pregnancy",
    outcomeEvent="First_spontaneous_pregnancy",
    censor_at_25=censor_at_25)
  
  if(!censor_at_25){   
    ####################
    ####################
    ## time from first preg to second preg
    print(3)
    
    AnalysesFigure2Int(
      descriptive="First_pregnancy_all==1 & Multiple_birth_first_pregnancy==1 & !is.na(time_to_second_pregnancy)",
      modelD=d[First_pregnancy_all==1 & Multiple_birth_first_pregnancy==1 & !is.na(time_to_second_pregnancy)],
      tag=tag,
      analysisName="time_first_to_second_pregnancy",
      isMixed=isMixed,
      outcomeTime="time_to_second_pregnancy",
      outcomeEvent="Second_pregnancy",
      censor_at_25=censor_at_25)
  }
}

