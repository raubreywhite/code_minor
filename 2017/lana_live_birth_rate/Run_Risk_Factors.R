RAWmisc::AllowFileManipulationFromInitialiseProject()
RAWmisc::InitialiseProject(
  HOME = "/git/code_minor/2017/lana_live_birth_rate/",
  RAW = "/analyses/data_raw/code_minor/2017/lana_live_birth_rate/",
  CLEAN = "/analyses/data_clean/code_minor/2017/lana_live_birth_rate",
  BAKED = "/analyses/results_baked/code_minor/2017/lana_live_birth_rate/",
  FINAL = "/analyses/results_final/code_minor/2017/lana_live_birth_rate/",
  SHARED = "/dropbox/results_shared/code_minor/2017/lana_live_birth_rate/"
)

dir.create(RAWmisc::PROJ$SHARED_TODAY)

library(data.table)
library(ggplot2)
library(mice)

lanaOutcomes <- data.table(openxlsx::read.xlsx(file.path(RAWmisc::PROJ$RAW,"Lifestyle_IVF_cases170912.xlsx"))) 
#lanaOutcomes <- lanaOutcomes[,c("")]
lana <- data.table(openxlsx::read.xlsx(file.path(RAWmisc::PROJ$RAW,"pek4-IVFcases170816buppstart.xlsx")))
lana <- data.table(openxlsx::read.xlsx(file.path(RAWmisc::PROJ$RAW,"Lifestyle_IVF_cases170912.xlsx"))) 
#variablesFirst[!variablesFirst %in% names(lana)]
for(i in 1:ncol(lana)) lana[[i]] <- as.numeric(lana[[i]])

#lana[,pos_preg_all:=sample(c(0,1),nrow(lana),replace=T)]
#lana[,pos_preg_ET:=sample(c(0,1),nrow(lana),replace=T)]
#lana[,Livebirth_all:=sample(c(0,1),nrow(lana),replace=T)]
#lana[,livebirth_ET:=sample(c(0,1),nrow(lana),replace=T)]

lana[,n_mature_egg:=n_matur_egg]
lana[,INF_INTERCOURSE_3MNT_FRQ:=INF_INTERCOURSE_3MNT_FRQ2]

#################
lana[,gravid:=gravid_previous_delivery]
lana[gravid==99,gravid:=NA]
lana[gravid>1,gravid:=NA]

lana[depression_ever==99,depression_ever:=NA]
lana[,depression_ever:=2-depression_ever]

median(lana$caffeine_daily,na.rm=T)
lana[,caffeine_daily:=caffeine_daily*1.436]

lana[,phs_length_m:=phs_length_m/100]

lana[,SMOKE_DAILY_NOW:=SMOKE_DAILY_NOW_UPPSTART]
lana[,alcohol:=round(alcohol)]

#################

lana[,PAL_total:=PAL_TOTAL]
lana[,AFC_TOTAL:=AFC_ULJ]
lana[,reason_infert_Q:=reason_infert_Q_uppstart]

lana[,BMI_Category:=cut(BMI,breaks=c(0,18.5,25,100))]

#outcomes
lana[,pos_preg_all:=pos_preg]
lana[,Livebirth_all:=livbirth_all]

hist(lana$egg_asp)
hist(lana$ASP_EGG)
hist(lana$n_embryo_created)
hist(lana$n_embryo_used)
hist(lana$n_mature_egg)
hist(lana$ET)


hist(lana$pos_preg_all)
hist(lana$pos_preg_ET)
hist(lana$miscarriage)
hist(lana$Livebirth_all)
hist(lana$livebirth_ET)

masterdata <- data.table(openxlsx::read.xlsx(file.path(RAWmisc::PROJ$RAW,"Uppstart-predicition_alcohol.xlsx")))
nrow(masterdata)
names(masterdata)
masterdata <- masterdata[SEX==2]
for(i in 1:ncol(masterdata)) masterdata[[i]] <- as.numeric(masterdata[[i]])

median(masterdata$caffeine_daily,na.rm=T)
masterdata[,BMI_Category:=cut(BMI,breaks=c(0,18.5,25,100))]

masterdata[,ULJ_HOGER_AFC:=as.numeric(stringr::str_replace_all(ULJ_HOGER_AFC," ",""))]
masterdata[,ULJ_VANSTER_AFC:=as.numeric(stringr::str_replace_all(ULJ_VANSTER_AFC," ",""))]
masterdata[is.na(ULJ_HOGER_AFC),ULJ_HOGER_AFC:=0]
masterdata[is.na(ULJ_VANSTER_AFC),ULJ_VANSTER_AFC:=0]

masterdata[,AFC_TOTAL:=ULJ_HOGER_AFC+ULJ_VANSTER_AFC]

masterdata[final_trying_time_years>100,final_trying_time_years:=NA]
masterdata[,alcohol:=round(alcohol)]

# ADDITIONAL CLEANING
masterdata[Livebirth_all==2,Livebirth_all:=NA]
masterdata[livebirth_ET==2,livebirth_ET:=NA]

#"HEALTH_HYPERTH",
#"HEALTH_HYPOTH",
#"HEALTH_MUSCLE_PAIN",

masterdata[,INF_INTERCOURSE_3MNT_FRQ:=as.factor(INF_INTERCOURSE_3MNT_FRQ)]
lana[,INF_INTERCOURSE_3MNT_FRQ:=as.factor(INF_INTERCOURSE_3MNT_FRQ)]

masterdata[,reason_infert_Q:=as.factor(reason_infert_Q)]
lana[,reason_infert_Q:=as.factor(reason_infert_Q)]

masteranastasia <- copy(masterdata)
masterlana <- copy(lana)

for(REASON_INFERT in c(-1)){
  ####
  # changing lana <-> masterdata
  #x <- copy(lana)
  #y <- copy(masterdata)
  
  #masterdata <- copy(x)
  #lana <- copy(y)
  
  
  ####
  
  outcomes <- c("egg_asp",
                "ASP_EGG",
                "n_embryo_created",
                "n_embryo_used",
                "n_mature_egg",
                "ET",
                "pos_preg_all",
                "pos_preg_ET",
                "miscarriage",
                "Livebirth_all",
                "livebirth_ET")
  
  variablesFirst <- c(
    "X_AGE",
    "INF_INTERCOURSE_3MNT_FRQ",
    "education",
    "SMOKE_DAILY_NOW",
    "alcohol",
    "gravid",
    "BMI",
    "depression_ever",
    "reason_infert_Q",
    "PAL_total",
    "final_trying_time_years",
    "caffeine_daily",
    "AMH",
    "AFC_TOTAL",
    "phs_length_m"
  )
  
  variablesSecond <- c(
    "X_AGE",
    "INF_INTERCOURSE_3MNT_FRQ",
    "education",
    "SMOKE_DAILY_NOW",
    "alcohol",
    "gravid",
    "BMI",
    "depression_ever",
    "reason_infert_Q",
    "PAL_total",
    "final_trying_time_years",
    "caffeine_daily",
    "AMH",
    "phs_length_m",
    "AFC_TOTAL",
    "ASP_EGG",
    "n_embryo_created",
    "n_embryo_used"
  )
  
  if(REASON_INFERT==-1){
    folder <- RAWmisc::PROJ$SHARED_TODAY
    
    masterdata <- copy(masteranastasia)
    lana <- copy(masterlana)
  } else {
    folder <- file.path(RAWmisc::PROJ$SHARED_TODAY,sprintf("infert_%s",REASON_INFERT))
    
    masterdata <- masteranastasia[reason_infert_Q==REASON_INFERT]
    lana <- masterlana[reason_infert_Q==REASON_INFERT]
    
    variablesFirst <- variablesFirst[variablesFirst!="reason_infert_Q"]
    variablesSecond <- variablesSecond[variablesSecond!="reason_infert_Q"]
  }
  #unlink(folder,force=T,recursive=T)
  dir.create(folder)
  
  
  ###### TABLE 1
  Desc <- function(name){
    xvar <- c(masterdata[[name]],lana[[name]])
    groupvar <- c(rep("Anastasia",nrow(masterdata)),
                  rep("Lana",nrow(lana)))

    if(length(unique(na.omit(xvar)))==2) xvar <- factor(xvar)
    if(is.factor(masterdata[[name]])) xvar <- factor(xvar)
    if(is.factor(xvar)){
      tab <- rbind(table(masterdata[[name]]),table(lana[[name]]))
      pval <- chisq.test(tab)$p.value
    } else {
      f <- na.omit(data.table(xvar,groupvar))
      pval <- kruskal.test(f$xvar,as.factor(f$groupvar))$p.value
    }
    x1 <- Gmisc::getDescriptionStatsBy(x = xvar, 
                                      by = groupvar,
                                      html=F,
                                      statistics=FALSE,
                                      useNA="no",
                                      continuous_fn = Gmisc::describeMedian,
                                      show_all_values=TRUE)
    x2 <- Gmisc::getDescriptionStatsBy(x = xvar, 
                                      by = groupvar,
                                      html=F,
                                      statistics=FALSE,
                                      useNA="always",
                                      continuous_fn = Gmisc::describeMedian,
                                      show_all_values=TRUE)
    x1 <- as.matrix(x1)
    x2 <- as.matrix(x2)
    retval1 <- data.table(x1)
    retval1[,detail:=row.names(x1)]
    
    retval2 <- data.table(x2)
    retval2[,detail:=row.names(x2)]
    retval2 <- retval2[.N]
    retval2[,Anastasia:=stringr::str_replace_all(Anastasia," \\([0-9.\\\\%]*\\)","")]
    retval2[,Lana:=stringr::str_replace_all(Lana," \\([0-9.\\\\%]*\\)","")]
    
    retval <- data.table(rbind(data.frame(retval1),data.frame(retval2)))
    retval[,var:=name]
    
    retval[,Pvalue:=pval]
    retval[,Anastasia:=stringr::str_replace_all(Anastasia,"\\\\pm","±")]
    retval[,Anastasia:=stringr::str_replace_all(Anastasia,"\\\\%","%")]
    retval[,Anastasia:=stringr::str_replace_all(Anastasia,"\\$","")]
    retval[,Lana:=stringr::str_replace_all(Lana,"\\\\pm","±")]
    retval[,Lana:=stringr::str_replace_all(Lana,"\\\\%","%")]
    retval[,Lana:=stringr::str_replace_all(Lana,"\\$","")]
    return(retval[,c("var","detail","Anastasia","Lana","Pvalue")])
  }
  
  descriptives <- unique(c(outcomes,variablesSecond,"BMI_Category"))
  T1 <- vector("list",length=length(descriptives))
  for(i in 1:length(descriptives)){
    T1[[i]] <- Desc(descriptives[i])
  }
  T1 <- rbindlist(T1)
  T1[,Pvalue:=RAWmisc::Format(Pvalue,3)]
  openxlsx::write.xlsx(T1,file=file.path(folder,"Table_1.xlsx"))
  
  ########################
  
  l <- list()
  d <- list()
  d[["egg_asp"]] <- masterdata[!is.na(egg_asp),c(
    "egg_asp",
    variablesFirst
  ),with=F]
  l[["egg_asp"]] <- lana[!is.na(egg_asp),c(
    "egg_asp",
    variablesFirst
  ),with=F]
  ##
  d[["ASP_EGG"]] <- masterdata[!is.na(ASP_EGG),c(
    "ASP_EGG",
    variablesFirst
    ),with=F]
  l[["ASP_EGG"]] <- lana[!is.na(ASP_EGG),c(
    "ASP_EGG",
    variablesFirst
  ),with=F]
  ##
  d[["n_embryo_created"]] <- masterdata[!is.na(n_embryo_created),c(
    "n_embryo_created",
    variablesFirst
  ),with=F]
  l[["n_embryo_created"]] <- lana[!is.na(n_embryo_created),c(
    "n_embryo_created",
    variablesFirst
  ),with=F]
  ##
  d[["n_embryo_used"]] <- masterdata[!is.na(n_embryo_used),c(
    "n_embryo_used",
    variablesFirst
  ),with=F]
  l[["n_embryo_used"]] <- lana[!is.na(n_embryo_used),c(
    "n_embryo_used",
    variablesFirst
  ),with=F]
  ##
  d[["n_mature_egg"]] <- masterdata[!is.na(n_mature_egg),c(
    "n_mature_egg",
    variablesFirst
  ),with=F]
  l[["n_mature_egg"]] <- lana[!is.na(n_mature_egg),c(
    "n_mature_egg",
    variablesFirst
  ),with=F]
  ##
  d[["ET"]] <- masterdata[!is.na(ET),c(
    "ET",
    variablesFirst
  ),with=F]
  l[["ET"]] <- lana[!is.na(ET),c(
    "ET",
    variablesFirst
  ),with=F]
  ##
  d[["pos_preg_all"]] <- masterdata[!is.na(pos_preg_all),c(
    "pos_preg_all",
    variablesFirst
  ),with=F]
  l[["pos_preg_all"]] <- lana[!is.na(pos_preg_all),c(
    "pos_preg_all",
    variablesFirst
  ),with=F]
  ##
  d[["pos_preg_ET"]] <- masterdata[!is.na(pos_preg_ET),c(
    "pos_preg_ET",
    variablesSecond
  ),with=F]
  l[["pos_preg_ET"]] <- lana[!is.na(pos_preg_ET),c(
    "pos_preg_ET",
    variablesSecond
  ),with=F]
  ##
  d[["miscarriage"]] <- masterdata[!is.na(miscarriage),c(
    "miscarriage",
    variablesFirst
  ),with=F]
  l[["miscarriage"]] <- lana[!is.na(miscarriage),c(
    "miscarriage",
    variablesFirst
  ),with=F]
  ##
  d[["Livebirth_all"]] <- masterdata[!is.na(Livebirth_all),c(
    "Livebirth_all",
    variablesFirst
  ),with=F]
  l[["Livebirth_all"]] <- lana[!is.na(Livebirth_all),c(
    "Livebirth_all",
    variablesFirst
  ),with=F]
  ##
  d[["livebirth_ET"]] <- masterdata[!is.na(livebirth_ET),c(
    "livebirth_ET",
    variablesSecond
  ),with=F]
  l[["livebirth_ET"]] <- lana[!is.na(livebirth_ET),c(
    "livebirth_ET",
    variablesSecond
  ),with=F]
  
  
  ldata <- data <- list()
  for(i in outcomes){
    data[[i]] <- mice::mice(d[[i]], m=5, method="pmm")
    if(!is.null(l[[i]])) ldata[[i]] <- mice::mice(l[[i]], m=5, method="pmm")
  }
  
  
  models1 <- vector("list",length=100)
  
  #### egg_asp
  modelsIndex <- 1
  models1[[modelsIndex]] <- data.frame(
    "outcome"="egg_asp",
    "method"="regression",
    "family"="binomial",
    stringsAsFactors = FALSE)
  
  #### ASP_EGG
  modelsIndex <- modelsIndex + 1
  models1[[modelsIndex]] <- data.frame(
    "outcome"="ASP_EGG",
    "method"="regression",
    "family"="quasipoisson",
    stringsAsFactors = FALSE)
  
  #### n_embryo_created
  modelsIndex <- modelsIndex + 1
  models1[[modelsIndex]] <- data.frame(
    "outcome"="n_embryo_created",
    "method"="regression",
    "family"="quasipoisson",
    stringsAsFactors = FALSE)
  
  #### n_embryo_used
  modelsIndex <- modelsIndex + 1
  models1[[modelsIndex]] <- data.frame(
    "outcome"="n_embryo_used",
    "method"="regression",
    "family"="quasipoisson",
    stringsAsFactors = FALSE)
  
  #### n_mature_egg
  modelsIndex <- modelsIndex + 1
  models1[[modelsIndex]] <- data.frame(
    "outcome"="n_mature_egg",
    "method"="regression",
    "family"="quasipoisson",
    stringsAsFactors = FALSE)
  
  #### ET
  modelsIndex <- modelsIndex + 1
  models1[[modelsIndex]] <- data.frame(
    "outcome"="ET",
    "method"="regression",
    "family"="binomial",
    stringsAsFactors = FALSE)
  
  #### pos_preg_all
  modelsIndex <- modelsIndex + 1
  models1[[modelsIndex]] <- data.frame(
    "outcome"="pos_preg_all",
    "method"="regression",
    "family"="binomial",
    stringsAsFactors = FALSE)
  
  #### pos_preg_ET
  modelsIndex <- modelsIndex + 1
  models1[[modelsIndex]] <- data.frame(
    "outcome"="pos_preg_ET",
    "method"="regression",
    "family"="binomial",
    stringsAsFactors = FALSE)
  
  #### miscarriage
  modelsIndex <- modelsIndex + 1
  models1[[modelsIndex]] <- data.frame(
    "outcome"="miscarriage",
    "method"="regression",
    "family"="binomial",
    stringsAsFactors = FALSE)
  
  #### Livebirth_all
  modelsIndex <- modelsIndex + 1
  models1[[modelsIndex]] <- data.frame(
    "outcome"="Livebirth_all",
    "method"="regression",
    "family"="binomial",
    stringsAsFactors = FALSE)
  
  #### livebirth_ET
  modelsIndex <- modelsIndex + 1
  models1[[modelsIndex]] <- data.frame(
    "outcome"="livebirth_ET",
    "method"="regression",
    "family"="binomial",
    stringsAsFactors = FALSE)
  
  
  retval1 <- vector("list",length=1000)
  predictiveAbility <- vector("list",length=1000)
  pb <- RAWmisc::ProgressBarCreate(min=0,max=modelsIndex)
  index <- 1
  for(i in 1:modelsIndex){
    RAWmisc::ProgressBarSet(pb, i)
    
    outcome <- models1[[i]]$outcome
    family <- models1[[i]]$family
    modelType <- models1[[i]]$modelType
    for(datasource in c("A","L")){
      if(datasource=="A"){
        useData <- data[[outcome]]
      } else {
        useData <- ldata[[outcome]]
      }
      
      exposures <- names(useData$data)[-1]
      includedExposures <- c()
      
      explan <- vector("list",length=length(exposures))
      
      formulaFull <- sprintf("%s~%s",outcome,paste0(exposures,collapse="+"))
      
      fitFull <- tryCatch({
        with(useData, glm(as.formula(formulaFull), family=family))
      }, warning=function(err){
          NULL
      })
      if(!is.null(fitFull)){
        x <- data.frame(summary(pool(fitFull)))
        x$var <- row.names(x)
        resA <- x[,c("est","se","Pr...t..","var")]
        names(resA) <- c("Abeta","Ase","Apsingle","var")
        resA <- data.table(resA)
        resA <- resA[var!="(Intercept)" & abs(Abeta)<3]
      } else {
        resA <- NULL
      }
      
      resC <- vector("list",length(exposures))
      for(j in 1:length(exposures)){
        formulaFullMinus1 <- sprintf("%s~%s",outcome,paste0(exposures[-j],collapse="+"))
        fitFullMinus1 <- tryCatch({
          with(useData, glm(as.formula(formulaFullMinus1), family=family))
        }, warning=function(err){
          NULL
        })
        
        Ap <- 9
        try({
          if(!is.null(fitFull) & !is.null(fitFullMinus1)){
            pc <- mice::pool.compare(fitFull, fitFullMinus1, method="Wald")
            Ap <- as.numeric(pc$pvalue)
          }
        }, TRUE)
        
        fit1 <- tryCatch({
          with(useData, glm(as.formula(sprintf("%s~%s",outcome,exposures[j])), family=family))
        }, warning=function(err){
          NULL
        })
        fit0 <- tryCatch({
          with(useData, glm(as.formula(sprintf("%s~1",outcome)), family=family))
        }, warning=function(err){
          NULL
        })
        
        Cp <- 1
        try({
          if(!is.null(fit1) & !is.null(fit0)){
            pc <- mice::pool.compare(fit1, fit0, method="Wald")
            Cp <- as.numeric(pc$pvalue)
          }
        }, TRUE)
        
        if(!is.null(fit1)){
          x <- data.frame(summary(pool(fit1)))
          x$var <- row.names(x)
          if(abs(x$est[2])<3){
            resC[[j]] <- x[,c("est","se","Pr...t..","var")]
            names(resC[[j]]) <- c("Cbeta","Cse","Cpsingle","var")
            resC[[j]]$Cp <- Cp
            resC[[j]]$Ap <- Ap
          }
        }
      }
      resC <- rbindlist(resC)
      resC <- resC[var!="(Intercept)"]
      
      if(!is.null(resA)){
        tempResult <- merge(resA, resC, by="var", all = T)
        tempResult[,adjusted:=RAWmisc::FormatEstCIFromEstSE(Abeta,Ase)]
        tempResult[Ap<0.05,adjusted:=paste0(adjusted,"*")]
        tempResult[,Apsingle:=RAWmisc::Format(Apsingle,3)]
        tempResult[,Ap:=RAWmisc::Format(Ap,3)]
      } else if(is.null(resA)){
        tempResult <- resC
        tempResult[,adjusted:="-"]
        tempResult[,Apsingle:="-"]
        tempResult[,Ap:=NULL]
        tempResult[,Ap:="-"]
      }
      tempResult[,crude:=RAWmisc::FormatEstCIFromEstSE(Cbeta,Cse)]
      tempResult[Cp<0.05,crude:=paste0(crude,"*")]
      tempResult[,Cpsingle:=RAWmisc::Format(Cpsingle,3)]
      tempResult[,Cp:=RAWmisc::Format(Cp,3)]
      
      tempResult <- tempResult[,c("var","crude","Cpsingle","Cp","adjusted","Apsingle","Ap")]
      if(datasource=="A"){
        setnames(tempResult,c("var","A_crude","A_Cpsingle","A_Cp","A_adjusted","A_Apsingle","A_Ap"))
        Aresult <- tempResult
      } else {
        setnames(tempResult,c("var","L_crude","L_Cpsingle","L_Cp","L_adjusted","L_Apsingle","L_Ap"))
        tosave <- merge(Aresult,tempResult,by="var",all=T)  
        openxlsx::write.xlsx(tosave,file=file.path(folder,sprintf("regressions_%s.xlsx",outcome)))
      }
    }
    index <- index + 1
  }
}









