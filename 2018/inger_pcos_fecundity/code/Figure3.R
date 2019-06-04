AnalysesFigure3Inger <- function(d,tag="normal", censor_at_25){
  if(censor_at_25){
    censor_at_25_tag <- "censor_at_25"
  } else {
    censor_at_25_tag <- "not_censor_at_25"
  }
  
  #########################
  #########################
  # outcome: time to first pregnancy
  # subset: group==1 (PCOS women)
  #########################
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
                                     censor_at_25_tag,
                                     "F3",
                                     tag,
                                     "hr_F3_time_to_first_preg_pcos_only.xlsx"))
  
  # KM curves and cloglog
  pd <- list()
  for(v in c("Age_diagnosis_cat","birth_periods","Country_of_birth", "Education")){
    # cloglog
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
  q <- q + scale_color_discrete("")
  q <- q + theme_gray(16)
  RAWmisc::saveA4(q,file.path(org::PROJ$SHARED_TODAY,
                              censor_at_25_tag,
                              "F3",
                              tag,
                              "loglog_F2_time_to_first_preg_pcos_only.png"))
  
  pd[,cumPregEst:=1-surv]
  pd[,cumPregEstLower:=1-upper]
  pd[,cumPregEstUpper:=1-lower]
  xlsx::write.xlsx(pd[,c(
    "var",
    "strata",
    "time",
    "n.risk",
    "n.event",
    "n.censor",
    "cumPregEst",
    "cumPregEstLower",
    "cumPregEstUpper")],
    file.path(org::PROJ$SHARED_TODAY,
              censor_at_25_tag,
             "F3",
             tag,
             "KM_F3_time_to_first_preg_pcos_only.xlsx"))
  
}