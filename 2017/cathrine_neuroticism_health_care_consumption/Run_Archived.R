RAWmisc::AllowFileManipulationFromInitialiseProject()
RAWmisc::InitialiseProject(
  HOME = "/git/code_minor/2017/cathrine_neuroticism_health_care_consumption/",
  RAW = "/analyses/data_raw/code_minor/2017/cathrine_neuroticism_health_care_consumption/",
  CLEAN = "/analyses/data_clean/code_minor/2017/cathrine_neuroticism_health_care_consumption",
  BAKED = "/analyses/results_baked/code_minor/2017/cathrine_neuroticism_health_care_consumption/",
  FINAL = "/analyses/results_final/code_minor/2017/cathrine_neuroticism_health_care_consumption/",
  SHARED = "/dropbox/results_shared/code_minor/2017/cathrine_neuroticism_health_care_consumption/"
)


# *Neuroticism health care consumption*
#   
# **FILTER**
# Include_new_CA
# *n=1208
# 
# 
# **EXPOSURE**
#   Neuroticism_CA
# 
# 
# **OUTCOMES**
#   *BINARY*
#   Base_prog_CA
# Hospital_admission_CA
# Prenatal_diagnostics_CA
# Sick_leave_CA
# Aurora_CA
# Annan_behandling
# 
# *SCALE*
#   Obgyn_physician_count_CA
# Midwife_ObGyn_phone_visits_CA
# Maternal_care_midwife_CA
# Ultrasounds_count_CA
# 
# 
# **CONFOUNDERS**
#   
#   *MAIN CONFOUNDER AND/OR EFFECT MODIFIER*
#   High_risk_somatic_disease_CA
# 
# *OTHER CONFOUNDERS*
#   Age_3_CA
# Parity_D_CA
# Country_CA
# Education_CA
# Working_status_CA
# Single_CA
# BMI_3_CA
# Psychiatric_disorder_medication_CA
# 


library(data.table)
library(ggplot2)
library(rms)

d <- data.table(haven::read_sav(file.path(
  RAWmisc::PROJ$RAW,"Neuroticism_health_consumption_180123.sav"
)))

dir.create(file.path(RAWmisc::PROJ$SHARED_TODAY,"tables"))
dir.create(file.path(RAWmisc::PROJ$SHARED_TODAY,"figures"))

nrow(d)
d <- d[Include_new_CA==1]
nrow(d)

OUTCOMES_BINARY <- c(
  "Base_prog_CA",
  "Hospital_admission_CA",
  "Prenatal_diagnostics_CA",
  "Sick_leave_CA",
  "Aurora_CA",
  "Annan_behandling",
  "Obstetrician_fetal_movements_D",
  "Obstetrician_contractions_D"
)

OUTCOMES_CONTINUOUS <- c(
  "Obgyn_physician_count_CA",
  "Midwife_ObGyn_phone_visits_CA",
  "Maternal_care_midwife_CA",
  "Ultrasounds_count_CA"
)

EXPOSURE <- "Neuroticism_CA"

EFFECT_MODIFIER <- "High_risk_somatic_disease_CA"

CONFOUNDERS_BINARY <- c(
  "Parity_D_CA",
  "Country_CA",
  "Education_CA",
  "Working_status_CA",
  "Single_CA",
  "Psychiatric_disorder_medication_CA"
)

CONFOUNDERS_CATEGORY <- c(
  "Age_3_CA",
  "BMI_3_CA"
)

var <- d$Base_prog_CA

SummarizeBinary <- function(var){
  x <- data.table(x=var)
  x <- x[,.(n=.N),by=.(x)]
  skeleton <- data.table(x=c(1,0,NA))
  skeleton <- merge(skeleton,x,by="x",all.x=T)
  skeleton[is.na(n),n:=0]
  skeleton[is.na(x),x:=-1]
  setorder(skeleton,-x)
  skeleton[,label:=c("True","False","Missing")]
  skeleton[x!=-1,percent:=sum(n)]
  skeleton[,percent:=sprintf("%s%%",RAWmisc::Format(n/percent*100,digits=1))]
  skeleton[x==-1,percent:="-"]
  skeleton[,x:=NULL]
  setcolorder(skeleton,c("label","n","percent"))
  
  skeleton <- rbind(skeleton[1:2],skeleton)
  for(i in names(skeleton)){
    skeleton[,(i):=as.character(get(i))]
    skeleton[1,(i):=""]
    skeleton[2,(i):=i]
  }
  return(skeleton)
}

SummarizeContinuous <- function(var){
  x <- data.table(x=var)
  x[,label:="Missing"]
  x[!is.na(x),label:="Median (IQR)"]
  x <- x[,.(
    n=.N,
    median=median(x,na.rm=T),
    p25=quantile(x,probs=0.25,na.rm=T),
    p75=quantile(x,probs=0.75,na.rm=T)
  ),by=label]
  x[,median_iqr:=sprintf("%s (%s-%s)",
                         RAWmisc::Format(median),
                         RAWmisc::Format(p25),
                         RAWmisc::Format(p75))]
  x <- x[,c("label","n","median_iqr")]
  skeleton <- data.table(label=c("Median (IQR)","Missing"))
  skeleton <- merge(skeleton,x,by="label",all.x=T)
  skeleton[is.na(n),n:=0]
  skeleton[label=="Missing",median_iqr:="-"]
  
  skeleton <- rbind(skeleton[1:2],skeleton)
  for(i in names(skeleton)){
    skeleton[,(i):=as.character(get(i))]
    skeleton[1,(i):=""]
    skeleton[2,(i):=i]
  }
  return(skeleton)
}

SummarizeCategory <- function(var){
  x <- data.table(x=var)
  x <- x[,.(n=.N),by=.(x)]
  setorder(x,-x)
  skeleton <- data.table(x=unique(c(NA,as.character(unique(var)))))
  skeleton <- merge(skeleton,x,by="x",all.x=T)
  skeleton[,x:=factor(x,levels=levels(var))]
  setorder(skeleton,-x)
  skeleton[,x:=as.character(x)]
  skeleton[is.na(n),n:=0]
  skeleton[is.na(x),x:="Missing"]
  skeleton <- skeleton[.N:1]
  
  skeleton[x!="Missing",percent:=sum(n)]
  skeleton[,percent:=sprintf("%s%%",RAWmisc::Format(n/percent*100,digits=1))]
  skeleton[x=="Missing",percent:="-"]
  
  skeleton <- rbind(skeleton[1:2],skeleton)
  for(i in names(skeleton)){
    skeleton[,(i):=as.character(get(i))]
    skeleton[1,(i):=""]
    skeleton[2,(i):=i]
  }
  return(skeleton)
}

SummarizeDispatch <- function(var,label=NULL){
  if(sum(!unique(var) %in% c(1,0,NA))==0){
    retval <- SummarizeBinary(var)
  } else if(is.factor(var)){
    retval <- SummarizeCategory(var)
  } else {
    retval <- SummarizeContinuous(var)
  }
  
  if(!is.null(label)){
    retval <- cbind(c(" ",rep(label,nrow(retval)-1)),retval)
  }
  return(retval)
}

for(i in c(
  CONFOUNDERS_CATEGORY
)){
  d[[i]] <- haven::as_factor(d[[i]])
}

res <- list()
for(i in c(
  OUTCOMES_BINARY,
  OUTCOMES_CONTINUOUS,
  EXPOSURE,
  EFFECT_MODIFIER,
  CONFOUNDERS_BINARY,
  CONFOUNDERS_CATEGORY
)){
  res[[i]] <- SummarizeDispatch(d[[i]],label=i)
}

res <- rbindlist(res)

openxlsx::write.xlsx(res, file=file.path(
  RAWmisc::PROJ$SHARED_TODAY,"tables","table_1.xlsx"
))

p25 <- quantile(d[[EXPOSURE]],prob=0.25,na.rm=T)
p25_plus_1 <- p25+1
p75 <- quantile(d[[EXPOSURE]],prob=0.75,na.rm=T)

ddist0 <- datadist(d[,c(
  OUTCOMES_BINARY,
  OUTCOMES_CONTINUOUS,
  EXPOSURE,
  EFFECT_MODIFIER,
  CONFOUNDERS_BINARY,
  CONFOUNDERS_CATEGORY
),with=F])
ddist0$limits[[EXPOSURE]][2] <- p25 ##### SETTING REFERENCE VALUE FOR NEUROTICISM
ddist0$limits[[EFFECT_MODIFIER]][2] <- 0 ##### SETTING REFERENCE VALUE FOR NEUROTICISM

ddist1 <- datadist(d[,c(
  OUTCOMES_BINARY,
  OUTCOMES_CONTINUOUS,
  EXPOSURE,
  EFFECT_MODIFIER,
  CONFOUNDERS_BINARY,
  CONFOUNDERS_CATEGORY
),with=F])
ddist1$limits[[EXPOSURE]][2] <- p25 ##### SETTING REFERENCE VALUE FOR NEUROTICISM
ddist1$limits[[EFFECT_MODIFIER]][2] <- 1 ##### SETTING REFERENCE VALUE FOR NEUROTICISM

##### RUNNING THE MODEL
stack <- data.frame(
  outcome=c(OUTCOMES_BINARY,OUTCOMES_CONTINUOUS),
  model=c(rep("logistic",length(OUTCOMES_BINARY)),rep("quasipoisson",length(OUTCOMES_CONTINUOUS)))
)

res <- vector("list",length=nrow(stack))
for(i in 1:nrow(stack)){
  OUTCOME <- stack$outcome[i]
  NUM_KNOTS <- 4
  EXPOSURE_AFTER_AIC <- EXPOSURE
  FORM <- "linear"
  if(OUTCOME %in% c("Midwife_ObGyn_phone_visits_CA","Hospital_admission_CA")){
    EXPOSURE_AFTER_AIC <- sprintf("rcs(%s,3)",EXPOSURE)
    FORM <- "rcs(3knots)"
  }
  if(stack$model[i]=="logistic"){
    fam <- binomial()
    famForAIC <- gaussian()
  } else {
    fam <- quasipoisson()
    famForAIC <- poisson()
  }
  
  options(datadist='ddist0')
  
  formulas <- list()
  
  formulas[["formula1crude_linear"]] <- sprintf("%s ~ %s",
                                                OUTCOME,
                                                EXPOSURE)
  formulas[["formula1crude_3"]] <- sprintf("%s ~ rcs(%s,%s)",
                                           OUTCOME,
                                           EXPOSURE,
                                           3)
  formulas[["formula1crude_4"]] <- sprintf("%s ~ rcs(%s,%s)",
                                           OUTCOME,
                                           EXPOSURE,
                                           4)
  formulas[["formula0adj"]] <- sprintf("%s ~ %s + %s + %s",
                                       OUTCOME,
                                       EFFECT_MODIFIER,
                                       CONFOUNDERS_BINARY,
                                       CONFOUNDERS_CATEGORY)
  formulas[["formula1adj"]] <- sprintf("%s ~ %s + %s + %s + %s",
                                       OUTCOME,
                                       EXPOSURE_AFTER_AIC,
                                       EFFECT_MODIFIER,
                                       CONFOUNDERS_BINARY,
                                       CONFOUNDERS_CATEGORY)
  formulas[["formula1int"]] <- sprintf("%s ~ %s*%s + %s + %s",
                                       OUTCOME,
                                       EXPOSURE_AFTER_AIC,
                                       EFFECT_MODIFIER,
                                       CONFOUNDERS_BINARY,
                                       CONFOUNDERS_CATEGORY)
  
  formulas[["formula0adj_CRUDE"]] <- sprintf("%s ~ 1",
                                             OUTCOME)
  formulas[["formula1adj_CRUDE"]] <- sprintf("%s ~ %s",
                                             OUTCOME,
                                             EXPOSURE_AFTER_AIC)
  
  formulas[["formula0int_CRUDE"]] <- sprintf("%s ~ %s + %s",
                                             OUTCOME,
                                             EXPOSURE_AFTER_AIC,
                                             EFFECT_MODIFIER)
  formulas[["formula1int_CRUDE"]] <- sprintf("%s ~ %s*%s",
                                             OUTCOME,
                                             EXPOSURE_AFTER_AIC,
                                             EFFECT_MODIFIER)
  
  modelResults <- list()
  for(f in names(formulas)){
    if(stringr::str_detect(f,"crude")){
      famUse <- famForAIC
    } else {
      famUse <- fam
    }
    modelResults[[f]] <- Glm(as.formula(formulas[[f]]),
                             x=TRUE, y=TRUE, data=d, family=famUse)
  }
  
  #### GETTING OUT THE ESTIMATED LOG-ODDS RATIOS AND 95% CIS FOR DIFFERENT POINTS CRUDE
  fit <- Glm(as.formula(formulas[["formula1adj_CRUDE"]]),
             x=TRUE, y=TRUE, data=d, family=fam)
  px <- Predict(fit,
                Neuroticism_CA=sort(c(p25,p25_plus_1,p75,seq(250,350,10))),
                ref.zero=T)
  px <- data.table(data.frame(px))
  C_est_together_1 <- px[Neuroticism_CA==p25_plus_1]$yhat
  C_est_together_p75 <- px[Neuroticism_CA==p75]$yhat
  
  C_l_together_1 <- px[Neuroticism_CA==p25_plus_1]$lower
  C_l_together_p75 <- px[Neuroticism_CA==p75]$lower
  
  C_u_together_1 <- px[Neuroticism_CA==p25_plus_1]$upper
  C_u_together_p75 <- px[Neuroticism_CA==p75]$upper
  
  options(datadist='ddist0')
  fit <- Glm(as.formula(formulas[["formula1int_CRUDE"]]),
             x=TRUE, y=TRUE, data=d, family=fam)
  p0 <- Predict(fit,
                Neuroticism_CA=sort(c(p25,p25_plus_1,p75,seq(250,350,10))),
                ref.zero=T)
  p0 <- data.table(data.frame(p0))
  
  C_est_lowrisk_1 <- p0[Neuroticism_CA==p25_plus_1]$yhat
  C_est_lowrisk_p75 <- p0[Neuroticism_CA==p75]$yhat
  
  C_l_lowrisk_1 <- p0[Neuroticism_CA==p25_plus_1]$lower
  C_l_lowrisk_p75 <- p0[Neuroticism_CA==p75]$lower
  
  C_u_lowrisk_1 <- p0[Neuroticism_CA==p25_plus_1]$upper
  C_u_lowrisk_p75 <- p0[Neuroticism_CA==p75]$upper
  
  options(datadist='ddist1')
  fit <- Glm(as.formula(formulas[["formula1int_CRUDE"]]),
             x=TRUE, y=TRUE, data=d, family=fam)
  p1 <- Predict(fit,
                Neuroticism_CA=sort(c(p25,p25_plus_1,p75,seq(250,350,10))),
                ref.zero=T)
  p1 <- data.table(data.frame(p1))
  
  C_est_highrisk_1 <- p1[Neuroticism_CA==p25_plus_1]$yhat
  C_est_highrisk_p75 <- p1[Neuroticism_CA==p75]$yhat
  
  C_l_highrisk_1 <- p1[Neuroticism_CA==p25_plus_1]$lower
  C_l_highrisk_p75 <- p1[Neuroticism_CA==p75]$lower
  
  C_u_highrisk_1 <- p1[Neuroticism_CA==p25_plus_1]$upper
  C_u_highrisk_p75 <- p1[Neuroticism_CA==p75]$upper
  
  #### GETTING OUT THE ESTIMATED LOG-ODDS RATIOS AND 95% CIS FOR DIFFERENT POINTS ADJUSTED
  fit <- Glm(as.formula(formulas[["formula1adj"]]),
             x=TRUE, y=TRUE, data=d, family=fam)
  px <- Predict(fit,
                Neuroticism_CA=sort(c(p25,p25_plus_1,p75,seq(250,350,10))),
                ref.zero=T)
  px <- data.table(data.frame(px))
  est_together_1 <- px[Neuroticism_CA==p25_plus_1]$yhat
  est_together_p75 <- px[Neuroticism_CA==p75]$yhat
  
  l_together_1 <- px[Neuroticism_CA==p25_plus_1]$lower
  l_together_p75 <- px[Neuroticism_CA==p75]$lower
  
  u_together_1 <- px[Neuroticism_CA==p25_plus_1]$upper
  u_together_p75 <- px[Neuroticism_CA==p75]$upper
  
  options(datadist='ddist0')
  fit <- Glm(as.formula(formulas[["formula1int"]]),
             x=TRUE, y=TRUE, data=d, family=fam)
  p0 <- Predict(fit,
                Neuroticism_CA=sort(c(p25,p25_plus_1,p75,seq(250,350,10))),
                ref.zero=T)
  p0 <- data.table(data.frame(p0))
  
  est_lowrisk_1 <- p0[Neuroticism_CA==p25_plus_1]$yhat
  est_lowrisk_p75 <- p0[Neuroticism_CA==p75]$yhat
  
  l_lowrisk_1 <- p0[Neuroticism_CA==p25_plus_1]$lower
  l_lowrisk_p75 <- p0[Neuroticism_CA==p75]$lower
  
  u_lowrisk_1 <- p0[Neuroticism_CA==p25_plus_1]$upper
  u_lowrisk_p75 <- p0[Neuroticism_CA==p75]$upper
  
  options(datadist='ddist1')
  fit <- Glm(as.formula(formulas[["formula1int"]]),
             x=TRUE, y=TRUE, data=d, family=fam)
  p1 <- Predict(fit,
                Neuroticism_CA=sort(c(p25,p25_plus_1,p75,seq(250,350,10))),
                ref.zero=T)
  p1 <- data.table(data.frame(p1))
  
  est_highrisk_1 <- p1[Neuroticism_CA==p25_plus_1]$yhat
  est_highrisk_p75 <- p1[Neuroticism_CA==p75]$yhat
  
  l_highrisk_1 <- p1[Neuroticism_CA==p25_plus_1]$lower
  l_highrisk_p75 <- p1[Neuroticism_CA==p75]$lower
  
  u_highrisk_1 <- p1[Neuroticism_CA==p25_plus_1]$upper
  u_highrisk_p75 <- p1[Neuroticism_CA==p75]$upper
  
  options(datadist='ddist0')
  p <- rbindlist(list(p0,p1))
  # converting from log-odds ratio to odds ratio
  p$yhat <- exp(p$yhat)
  p$lower <- exp(p$lower)
  p$upper <- exp(p$upper)
  print(p)
  
  q <- ggplot(p,aes(x=Neuroticism_CA,
                    y=yhat,
                    ymax=upper,
                    ymin=lower,
                    colour=factor(High_risk_somatic_disease_CA),
                    fill=factor(High_risk_somatic_disease_CA),
                    group=factor(High_risk_somatic_disease_CA)))
  q <- q + geom_ribbon(alpha=0.2,colour=NA)
  q <- q + geom_line()
  q <- q + geom_vline(xintercept=258,col="red")
  q <- q + geom_hline(yintercept=1,col="red")
  #q <- q + labs(caption=sprintf("Interaction p-value: %s",
  #                              RAWmisc::Format(retval$pval_interaction,3)))
  RAWmisc::saveA4(q,filename = file.path(
    RAWmisc::PROJ$SHARED_TODAY,"figures",sprintf("graph_%s.png",OUTCOME)
  ))
  
  #fit1int
  
  aic_linear=modelResults[["formula1crude_linear"]]$aic
  aic_3=modelResults[["formula1crude_3"]]$aic
  aic_4=modelResults[["formula1crude_4"]]$aic
  
  retval <- data.table(
    outcome=OUTCOME,
    modeltype=stack$model[i],
    form=FORM,
    aic_3_minus_linear=RAWmisc::Format(aic_3-aic_linear),
    aic_4_minus_linear=RAWmisc::Format(aic_4-aic_linear),
    est_together_1_unit=sprintf("%s (%s, %s)",
                                RAWmisc::Format(exp(est_together_1)),
                                RAWmisc::Format(exp(l_together_1)),
                                RAWmisc::Format(exp(u_together_1))
    ),
    est_together_p75_unit=sprintf("%s (%s, %s)",
                                  RAWmisc::Format(exp(est_together_p75)),
                                  RAWmisc::Format(exp(l_together_p75)),
                                  RAWmisc::Format(exp(u_together_p75))
    ),
    est_lowrisk_1_unit=sprintf("%s (%s, %s)",
                               RAWmisc::Format(exp(est_lowrisk_1)),
                               RAWmisc::Format(exp(l_lowrisk_1)),
                               RAWmisc::Format(exp(u_lowrisk_1))
    ),
    est_lowrisk_p75_unit=sprintf("%s (%s, %s)",
                                 RAWmisc::Format(exp(est_lowrisk_p75)),
                                 RAWmisc::Format(exp(l_lowrisk_p75)),
                                 RAWmisc::Format(exp(u_lowrisk_p75))
    ),est_highrisk_1_unit=sprintf("%s (%s, %s)",
                                  RAWmisc::Format(exp(est_highrisk_1)),
                                  RAWmisc::Format(exp(l_highrisk_1)),
                                  RAWmisc::Format(exp(u_highrisk_1))
    ),
    est_highrisk_p75_unit=sprintf("%s (%s, %s)",
                                  RAWmisc::Format(exp(est_highrisk_p75)),
                                  RAWmisc::Format(exp(l_highrisk_p75)),
                                  RAWmisc::Format(exp(u_highrisk_p75))
    ),
    pval_adj=rms::lrtest(modelResults[["formula0adj"]],modelResults[["formula1adj"]])$stats[["P"]],
    pval_interaction=rms::lrtest(modelResults[["formula1adj"]],modelResults[["formula1int"]])$stats[["P"]],
    
    C_est_together_1_unit=sprintf("%s (%s, %s)",
                                  RAWmisc::Format(exp(C_est_together_1)),
                                  RAWmisc::Format(exp(C_l_together_1)),
                                  RAWmisc::Format(exp(C_u_together_1))
    ),
    C_est_together_p75_unit=sprintf("%s (%s, %s)",
                                    RAWmisc::Format(exp(C_est_together_p75)),
                                    RAWmisc::Format(exp(C_l_together_p75)),
                                    RAWmisc::Format(exp(C_u_together_p75))
    ),
    C_est_lowrisk_1_unit=sprintf("%s (%s, %s)",
                                 RAWmisc::Format(exp(C_est_lowrisk_1)),
                                 RAWmisc::Format(exp(C_l_lowrisk_1)),
                                 RAWmisc::Format(exp(C_u_lowrisk_1))
    ),
    C_est_lowrisk_p75_unit=sprintf("%s (%s, %s)",
                                   RAWmisc::Format(exp(C_est_lowrisk_p75)),
                                   RAWmisc::Format(exp(C_l_lowrisk_p75)),
                                   RAWmisc::Format(exp(C_u_lowrisk_p75))
    ),
    C_est_highrisk_1_unit=sprintf("%s (%s, %s)",
                                  RAWmisc::Format(exp(C_est_highrisk_1)),
                                  RAWmisc::Format(exp(C_l_highrisk_1)),
                                  RAWmisc::Format(exp(C_u_highrisk_1))
    ),
    C_est_highrisk_p75_unit=sprintf("%s (%s, %s)",
                                    RAWmisc::Format(exp(C_est_highrisk_p75)),
                                    RAWmisc::Format(exp(C_l_highrisk_p75)),
                                    RAWmisc::Format(exp(C_u_highrisk_p75))
    ),
    C_pval_adj=rms::lrtest(modelResults[["formula0adj_CRUDE"]],modelResults[["formula1adj_CRUDE"]])$stats[["P"]],
    C_pval_interaction=rms::lrtest(modelResults[["formula0int_CRUDE"]],modelResults[["formula1int_CRUDE"]])$stats[["P"]]
  )
  
  res[[i]] <- retval
  
}

res <- rbindlist(res)

res[,C_pval_adj:=RAWmisc::Format(C_pval_adj,digits=3)]
res[,C_pval_interaction:=RAWmisc::Format(C_pval_interaction,digits=3)]
res[,sig:=""]
res[C_pval_adj<="0.050",sig:="*"]
res[C_pval_adj=="0.000",C_pval_adj:="<0.001"]
res[,C_pval_adj:=sprintf("%s%s",C_pval_adj,sig)]
res[,sig:=NULL]

res[,sig:=""]
res[C_pval_interaction<="0.050",sig:="*"]
res[C_pval_interaction=="0.000",C_pval_interaction:="<0.001"]
res[,C_pval_interaction:=sprintf("%s%s",C_pval_interaction,sig)]
res[,sig:=NULL]


res[,pval_adj:=RAWmisc::Format(pval_adj,digits=3)]
res[,pval_interaction:=RAWmisc::Format(pval_interaction,digits=3)]

res[,sig:=""]
res[pval_adj<="0.050",sig:="*"]
res[pval_adj=="0.000",pval_adj:="<0.001"]
res[,pval_adj:=sprintf("%s%s",pval_adj,sig)]
res[,sig:=NULL]

res[,sig:=""]
res[pval_interaction<="0.050",sig:="*"]
res[pval_interaction=="0.000",pval_interaction:="<0.001"]
res[,pval_interaction:=sprintf("%s%s",pval_interaction,sig)]
res[,sig:=NULL]
res

table2 <- res[,c(
  "outcome",
  "modeltype",
  "form",
  "aic_3_minus_linear",
  "aic_4_minus_linear"
)]

openxlsx::write.xlsx(table2, file=file.path(
  RAWmisc::PROJ$SHARED_TODAY,"tables","table2_aic.xlsx"
))

############

table3 <- res[,c(
  "outcome","est_together_1_unit","est_together_p75_unit","pval_adj"
)]
table3[outcome %in% c("Midwife_ObGyn_phone_visits_CA","Hospital_admission_CA"),est_together_1_unit:="-"]

openxlsx::write.xlsx(table3, file=file.path(
  RAWmisc::PROJ$SHARED_TODAY,"tables","adj_table3_main_effect.xlsx"
))

table3 <- res[,c(
  "outcome","C_est_together_1_unit","C_est_together_p75_unit","C_pval_adj"
)]
table3[outcome %in% c("Midwife_ObGyn_phone_visits_CA","Hospital_admission_CA"),C_est_together_1_unit:="-"]

openxlsx::write.xlsx(table3, file=file.path(
  RAWmisc::PROJ$SHARED_TODAY,"tables","crude_table3_main_effect.xlsx"
))

############

table4 <- res[,c(
  "outcome",
  "est_lowrisk_1_unit",
  "est_highrisk_1_unit",
  "est_lowrisk_p75_unit",
  "est_highrisk_p75_unit",
  "pval_interaction"
)]
#table4[!stringr::str_detect(pval_interaction,"\\*"),est_lowrisk_1_unit:="-"]
#table4[!stringr::str_detect(pval_interaction,"\\*"),est_highrisk_1_unit:="-"]
#table4[!stringr::str_detect(pval_interaction,"\\*"),est_lowrisk_p75_unit:="-"]
#table4[!stringr::str_detect(pval_interaction,"\\*"),est_highrisk_p75_unit:="-"]

table4[outcome %in% c("Midwife_ObGyn_phone_visits_CA","Hospital_admission_CA"),est_lowrisk_1_unit:="-"]
table4[outcome %in% c("Midwife_ObGyn_phone_visits_CA","Hospital_admission_CA"),est_highrisk_1_unit:="-"]

openxlsx::write.xlsx(table4, file=file.path(
  RAWmisc::PROJ$SHARED_TODAY,"tables","adj_table4_stratified_effect.xlsx"
))

table4 <- res[,c(
  "outcome",
  "C_est_lowrisk_1_unit",
  "C_est_highrisk_1_unit",
  "C_est_lowrisk_p75_unit",
  "C_est_highrisk_p75_unit",
  "C_pval_interaction"
)]
#table4[!stringr::str_detect(C_pval_interaction,"\\*"),C_est_lowrisk_1_unit:="-"]
#table4[!stringr::str_detect(C_pval_interaction,"\\*"),C_est_highrisk_1_unit:="-"]
#table4[!stringr::str_detect(C_pval_interaction,"\\*"),C_est_lowrisk_p75_unit:="-"]
#table4[!stringr::str_detect(C_pval_interaction,"\\*"),C_est_highrisk_p75_unit:="-"]

table4[outcome %in% c("Midwife_ObGyn_phone_visits_CA","Hospital_admission_CA"),C_est_lowrisk_1_unit:="-"]
table4[outcome %in% c("Midwife_ObGyn_phone_visits_CA","Hospital_admission_CA"),C_est_highrisk_1_unit:="-"]

openxlsx::write.xlsx(table4, file=file.path(
  RAWmisc::PROJ$SHARED_TODAY,"tables","crude_table4_stratified_effect.xlsx"
))





