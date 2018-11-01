org::InitialiseOpinionatedUnix("/code_minor/2017/lana_live_birth_rate/")

library(data.table)
library(ggplot2)
library(mice)

lanaOutcomes <- data.table(openxlsx::read.xlsx(file.path(org::PROJ$RAW,"Lifestyle_IVF_cases170912.xlsx"))) 
#lanaOutcomes <- lanaOutcomes[,c("")]
#lana <- data.table(openxlsx::read.xlsx(file.path(org::PROJ$RAW,"pek4-IVFcases170816buppstart.xlsx")))
lana <- data.table(openxlsx::read.xlsx(file.path(org::PROJ$RAW,"Lifestyle_IVF_cases170912.xlsx"))) 
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

lana[,smoker:=SMOKE_DAILY_NOW]
lana[SMOKE_DAILY_NOW==2,smoker:=1]
lana[SMOKE_DAILY_NOW>5,smoker:=0]
lana[,alcohol:=round(alcohol)]

lana[,SMOKE_EVER:=smoker]

lana[SOC_EDUCATION==4, education:=1]

#################

lana[,PAL_total:=PAL_TOTAL]
lana[,AFC_TOTAL:=AFC_ULJ]
lana[,reason_infert_Q:=reason_infert_Q_uppstart]

lana[,BMI_Category:=cut(BMI,breaks=c(0,18.5,25,100))]

#outcomes
lana[,pos_preg_all:=pos_preg]
lana[,Livebirth_all:=livbirth_all]

lana[,FSH_TOTALDOS:=FSH_totaldose]

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

masterdata <- data.table(openxlsx::read.xlsx(file.path(org::PROJ$RAW,"Uppstart-predicition_alcohol.xlsx")))
smoking <- data.table(openxlsx::read.xlsx(file.path(org::PROJ$RAW,"Kopia avuppstart170911_smoke.xlsx")))
smoking[!is.na(SMOKE_EVER),smoker:=0]
smoking[SMOKE_DAILY_NOW==1,smoker:=1]
smoking[SMOKE_STOP_SINCE %in% c(1,2),smoker:=1]
smoking[smoker>1,smoker:=NA]
smoking <- smoking[,c("STUDYPERSONID","smoker","SMOKE_EVER")]
nrow(masterdata)
masterdata <- merge(masterdata,smoking,by="STUDYPERSONID",all.x=T)
nrow(masterdata)
names(masterdata)
masterdata <- masterdata[SEX==2]
for(i in 1:ncol(masterdata)) masterdata[[i]] <- as.numeric(masterdata[[i]])

median(masterdata$caffeine_daily,na.rm=T)
masterdata[,BMI_Category:=cut(BMI,breaks=c(0,18.5,25,100))]

masterdata[,ULJ_HOGER_AFC:=as.numeric(stringr::str_replace_all(ULJ_HOGER_AFC," ",""))]
masterdata[,ULJ_VANSTER_AFC:=as.numeric(stringr::str_replace_all(ULJ_VANSTER_AFC," ",""))]
#masterdata[is.na(ULJ_HOGER_AFC),ULJ_HOGER_AFC:=0]
#masterdata[is.na(ULJ_VANSTER_AFC),ULJ_VANSTER_AFC:=0]

masterdata[,AFC_TOTAL:=ULJ_HOGER_AFC+ULJ_VANSTER_AFC]

masterdata[final_trying_time_years>100,final_trying_time_years:=NA]
masterdata[,alcohol:=round(alcohol)]

# ADDITIONAL CLEANING
masterdata[Livebirth_all==2,Livebirth_all:=1]
masterdata[livebirth_ET==2,livebirth_ET:=1]

###### JOINT CLEANING
lana[ET==1 & pos_preg==0, miscarriage:=NA]
masterdata[ET==1 & pos_preg==0, miscarriage:=NA]

### SMOKING NOW
lana[,smoke_now_daily:=SMOKE_DAILY_NOW_UPPSTART]
masterdata[,smoke_now_daily:=SMOKE_DAILY_NOW]

# AFC_TOTAL_UNDER_7
lana[,AFC_TOTAL_UNDER_7:=AFC_TOTAL<7]
masterdata[,AFC_TOTAL_UNDER_7:=AFC_TOTAL<7]

# AMH_UNDER_05
lana[,AMH_UNDER_05:=AMH<0.5]
masterdata[,AMH_UNDER_05:=AMH<0.5]

# AMH_UNDER_1
lana[,AMH_UNDER_1:=AMH<1]
masterdata[,AMH_UNDER_1:=AMH<1]

#"HEALTH_HYPERTH",
#"HEALTH_HYPOTH",
#"HEALTH_MUSCLE_PAIN",

masterdata[,INF_INTERCOURSE_3MNT_FRQ:=as.factor(INF_INTERCOURSE_3MNT_FRQ)]
lana[,INF_INTERCOURSE_3MNT_FRQ:=as.factor(INF_INTERCOURSE_3MNT_FRQ)]

masterdata[,reason_infert_Q:=as.factor(reason_infert_Q)]
lana[,reason_infert_Q:=as.factor(reason_infert_Q)]

masteranastasia <- copy(masterdata)
masterlana <- copy(lana)


####
# changing lana <-> masterdata
#x <- copy(lana)
#y <- copy(masterdata)

#masterdata <- copy(x)
#lana <- copy(y)


#### CHECKING NUMBERS
nrow(masterlana) #242
sum(masterlana$ASP_EGG>0,na.rm=T) #235
sum(masterlana$n_embryo_used>0,na.rm=T) #222
sum(masterlana$pos_preg>0,na.rm=T) #95
sum(masterlana$miscarriage_IVF>0,na.rm=T) #17
sum(masterlana$livbirth_all>0,na.rm=T) #72

nrow(masteranastasia) #432
sum(masteranastasia$ASP_EGG>0,na.rm=T) #256
sum(masteranastasia$n_embryo_used>0,na.rm=T) #214
sum(masteranastasia$pos_preg>0,na.rm=T) #146
sum(masteranastasia$miscarriage_IVF>0,na.rm=T) #32
sum(masteranastasia$livebirth_all>0,na.rm=T) #102



####

outcomes <- c("ASP_EGG",
              "n_embryo_created",
              "n_embryo_used",
              "n_mature_egg")

variablesFirst <- c(
  "X_AGE",
  "BMI",
  "SMOKE_EVER",
  "alcohol",
  "caffeine_daily",
  "PAL_total",
  "depression_ever"
)

variablesSecond <- variablesFirst

variablesTable1 <- c(
  "X_AGE",
  "education",
  "phs_length_m",
  "BMI",
  "BMI_Category",
  "SMOKE_EVER",
  "smoke_now_daily",
  "alcohol",
  "caffeine_daily",
  "PAL_total",
  "INF_INTERCOURSE_3MNT_FRQ",
  "depression_ever",
  
  "reason_infert_Q",
  "final_trying_time_years",
  "AFC_TOTAL",
  "AMH",
  "FSH_TOTALDOS",
  
  "ASP_EGG",
  "n_embryo_created",
  "n_embryo_used",
  "n_mature_egg",
  
  "AFC_TOTAL_UNDER_7",
  "AMH_UNDER_05",
  "AMH_UNDER_1"
)


folder <- org::PROJ$SHARED_TODAY
dir.create(folder)

masterdata <- copy(masteranastasia)
lana <- copy(masterlana)


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

descriptives <- variablesTable1
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
  "AFC_TOTAL",
  "AMH",
  variablesFirst
),with=F]
l[["egg_asp"]] <- lana[!is.na(egg_asp),c(
  "egg_asp",
  "AFC_TOTAL",
  "AMH",
  variablesFirst
),with=F]
##
d[["ASP_EGG"]] <- masterdata[!is.na(ASP_EGG),c(
  "ASP_EGG",
  "AFC_TOTAL",
  "AMH",
  variablesFirst
  ),with=F]
l[["ASP_EGG"]] <- lana[!is.na(ASP_EGG),c(
  "ASP_EGG",
  "AFC_TOTAL",
  "AMH",
  variablesFirst
),with=F]
##
d[["n_embryo_created"]] <- masterdata[!is.na(n_embryo_created),c(
  "n_embryo_created",
  "ASP_EGG",
  "AFC_TOTAL",
  "AMH",
  variablesFirst
),with=F]
l[["n_embryo_created"]] <- lana[!is.na(n_embryo_created),c(
  "n_embryo_created",
  "ASP_EGG",
  "AFC_TOTAL",
  "AMH",
  variablesFirst
),with=F]
##
d[["n_embryo_used"]] <- masterdata[!is.na(n_embryo_used),c(
  "n_embryo_used",
  "ASP_EGG",
  "AFC_TOTAL",
  "AMH",
  variablesFirst
),with=F]
l[["n_embryo_used"]] <- lana[!is.na(n_embryo_used),c(
  "n_embryo_used",
  "ASP_EGG",
  "AFC_TOTAL",
  "AMH",
  variablesFirst
),with=F]
##
d[["n_mature_egg"]] <- masterdata[!is.na(n_mature_egg),c(
  "n_mature_egg",
  "ASP_EGG",
  "AFC_TOTAL",
  "AMH",
  variablesFirst
),with=F]
l[["n_mature_egg"]] <- lana[!is.na(n_mature_egg),c(
  "n_mature_egg",
  "ASP_EGG",
  "AFC_TOTAL",
  "AMH",
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
  data[[i]] <- mice::mice(d[[i]], m=20, method="pmm", seed=4)
  if(!is.null(l[[i]])) ldata[[i]] <- mice::mice(l[[i]], m=20, method="pmm", seed=4)
  
  openxlsx::write.xlsx(d[[i]][,1:2],file=file.path(org::PROJ$SHARED_TODAY,sprintf("sample_sizes_anastasia_%s.xlsx",i)))
  openxlsx::write.xlsx(l[[i]][,1:2],file=file.path(org::PROJ$SHARED_TODAY,sprintf("sample_sizes_lana_%s.xlsx",i)))
}

plotData <- rbind(data[["n_mature_egg"]]$data,complete(data[["n_mature_egg"]],1))
plotData$id <- rep(1:nrow(data[["n_mature_egg"]]$data),2)
plotData$imputed <- "Imputed"
plotData$imputed[1:nrow(data[["n_mature_egg"]]$data)] <- "Real data"
plotData <- data.table(plotData)

ids <- plotData[imputed=="Real data" & !is.na(AMH)]$id
plotData[imputed=="Imputed" & id %in% ids,AMH:=NA]

q <- ggplot(plotData,aes(x=AMH,y=AFC_TOTAL,color=imputed))
q <- q + geom_point(data=plotData[imputed=="Real data"])
q <- q + geom_point(data=plotData[imputed=="Imputed"])
q <- q + scale_color_brewer(palette="Set1")
q <- q + scale_x_continuous(lim=c(0,25))
q <- q + theme_gray(16)
RAWmisc::saveA4(q,file.path(org::PROJ$SHARED_TODAY,"afc_imputation.png"))




models1 <- vector("list",length=100)


#### ASP_EGG
modelsIndex <- 1
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
  
retval0 <- vector("list",length=1000)
retval1 <- vector("list",length=1000)
predictiveAbility <- vector("list",length=1000)
index <- 1
for(denominator in c("no_denominator","denominator_asp_egg")) for(i in 1:modelsIndex){
  outcome <- models1[[i]]$outcome
  family <- models1[[i]]$family
  modelType <- models1[[i]]$modelType
  riskFactors <- c()
  protectiveFactors <- c()
  
  if(denominator=="denominator_asp_egg" & outcome=="ASP_EGG") next
  
  for(datasource in c("L","A")){
    if(datasource=="L"){
      useData <- ldata[[outcome]]
    } else {
      useData <- data[[outcome]]
    }
    
    exposures <- names(useData$data)[-1]
    OFFSET <- ""
    if(sum(exposures=="ASP_EGG")>0){
      exposures <- exposures[-which(exposures=="ASP_EGG")]
      if(denominator=="denominator_asp_egg") OFFSET <- "offset(log(ASP_EGG))+"
    }
    exposures <- exposures[-which(exposures=="AFC_TOTAL")]
    exposures <- exposures[-which(exposures=="AMH")]
    
    includedExposures <- c()
    
    explan <- vector("list",length=length(exposures))
    
    formulaFull <- sprintf("%s~%s",outcome,paste0(exposures,collapse="+"))
    
    fitFull <- tryCatch({
      if(denominator=="denominator_asp_egg"){
        with(useData, glm(as.formula(formulaFull), family=family, subset=(ASP_EGG>0)))
      } else {
        with(useData, glm(as.formula(formulaFull), family=family))
      }
    }, warning=function(err){
        NULL
    })
    if(!is.null(fitFull)){
      x <- data.frame(summary(pool(fitFull)))
      x$var <- row.names(x)
      resA <- x[,c("estimate","std.error","p.value","var")]
      names(resA) <- c("Abeta","Ase","Ap","var")
      resA <- data.table(resA)
      resA <- resA[var!="(Intercept)" & abs(Abeta)<3]
    } else {
      resA <- NULL
    }
    
    resC <- vector("list",length(exposures))
    for(j in 1:length(exposures)){
      
      fit1 <- tryCatch({
        if(denominator=="denominator_asp_egg"){
          with(useData, glm(as.formula(sprintf("%s~%s %s",outcome,OFFSET, exposures[j])), family=family, subset=(ASP_EGG>0)))
        } else {
          with(useData, glm(as.formula(sprintf("%s~%s %s",outcome,OFFSET, exposures[j])), family=family))
        }
      }, warning=function(err){
        NULL
      })
      
      if(!is.null(fit1)){
        x <- data.frame(summary(pool(fit1)))
        x$var <- row.names(x)
        if(abs(x$est[2])<3){
          resC[[j]] <- x[,c("estimate","std.error","p.value","var")]
          names(resC[[j]]) <- c("Cbeta","Cse","Cp","var")
          
          fit0x <- tryCatch({
            if(denominator=="denominator_asp_egg"){
              with(useData, glm(as.formula(sprintf("%s~%s %s + AFC_TOTAL",outcome,OFFSET, exposures[j])), family=family, subset=(ASP_EGG>0)))
            } else {
              with(useData, glm(as.formula(sprintf("%s~%s %s + AFC_TOTAL",outcome,OFFSET, exposures[j])), family=family))
            }
          }, warning=function(err){
            NULL
          })
          fit1x <- tryCatch({
            if(denominator=="denominator_asp_egg"){
              with(useData, glm(as.formula(sprintf("%s~%s %s*AFC_TOTAL",outcome,OFFSET, exposures[j])), family=family, subset=(ASP_EGG>0)))
            } else {
              with(useData, glm(as.formula(sprintf("%s~%s %s*AFC_TOTAL",outcome,OFFSET, exposures[j])), family=family))
            }
          }, warning=function(err){
            NULL
          })
          
          Intp <- 1
          try({
            if(!is.null(fit1x) & !is.null(fit0x)){
              pc <- mice::pool.compare(fit1x, fit0x, method="Wald")
              Intp <- as.numeric(pc$pvalue)
            }
          }, TRUE)
          resC[[j]]$CIntp <- RAWmisc::Format(Intp,3)
          
        }
      }
    }
    resC <- rbindlist(resC)
    resC <- resC[var!="(Intercept)"]

    if(!is.null(resA)){
      tempResult <- merge(resA, resC, by="var", all = T)
      tempResult[,adjusted:=RAWmisc::FormatEstCIFromEstSE(Abeta,Ase)]
      tempResult[Ap<0.05,adjusted:=paste0(adjusted,"*")]
      tempResult[,Ap:=RAWmisc::Format(Ap,3)]
    } else if(is.null(resA)){
      tempResult <- resC
      tempResult[,adjusted:="-"]
      tempResult[,Ap:="-"]
    }
    tempResult[,crude:=RAWmisc::FormatEstCIFromEstSE(Cbeta,Cse)]
    tempResult[Cp<0.05,crude:=paste0(crude,"*")]
    tempResult[,Cp:=RAWmisc::Format(Cp,3)]
    
    if(datasource=="L"){
      riskFactors <- tempResult[(Ap<"0.050" & Abeta<0) | (Cp<"0.050" & Cbeta<0)]$var
      protectiveFactors <- tempResult[(Ap<"0.050" & Abeta>0) | (Cp<"0.050" & Cbeta>0)]$var
    }
    
    tempResult <- tempResult[,c("var","crude","Cp","CIntp","adjusted","Ap")]
    if(datasource=="L"){
      setnames(tempResult,c("var","L_crude","L_Cp","L_CIntp","L_adjusted","L_Ap"))
      Lresult <- tempResult
    } else {
      setnames(tempResult,c("var","A_crude","A_Cp","A_CIntp","A_adjusted","A_Ap"))
      tosave <- merge(Lresult,tempResult,by="var",all=T)  
      openxlsx::write.xlsx(tosave,file=file.path(folder,sprintf("%s_regressions_%s.xlsx",denominator,outcome)))
      
      tosave[,denominator:=denominator]
      tosave[,outcome:=outcome]
      retval0[[index]] <- tosave
    }
  }
  
  if(length(riskFactors)>0 | length(protectiveFactors)>0){
    newData <- complete(data[[outcome]], action='long', include=TRUE)
    newData$risk_factors <- 0
    
    if(length(riskFactors)>0){
      for(j in 1:length(riskFactors)){
        if(length(unique(newData[[riskFactors[j]]]))>3){
          cutoff <- median(newData[[riskFactors[j]]],na.rm=T)
        } else cutoff <- 0
        indexPoint <- which(newData[[riskFactors[j]]]>cutoff)
        indexPoint <- indexPoint[!is.na(indexPoint)]
        newData$risk_factors[indexPoint] <- newData$risk_factors[indexPoint] + 1
      }
    }
    if(length(protectiveFactors)>0){
      for(j in 1:length(protectiveFactors)){
        if(length(unique(newData[[protectiveFactors[j]]]))>3){
          cutoff <- median(newData[[protectiveFactors[j]]],na.rm=T)
        } else cutoff <- 1
        indexPoint <- which(newData[[protectiveFactors[j]]]<cutoff)
        indexPoint <- indexPoint[!is.na(indexPoint)]
        newData$risk_factors[indexPoint] <- newData$risk_factors[indexPoint] + 1
      }
    }
    
    cutoff <- median(newData$AFC_TOTAL,na.rm=T)
    indexPoint <- which(newData$AFC_TOTAL>cutoff)
    indexPoint <- indexPoint[!is.na(indexPoint)]
    newData$high_afc <- 0
    newData$high_afc[indexPoint] <- 1
    
    newData <- as.mids(newData)
    fit0 <- tryCatch({
      if(denominator=="denominator_asp_egg"){
        with(newData, glm(as.formula(sprintf("%s~%s 1",outcome, OFFSET)), family=family, subset=(ASP_EGG>0)))
      } else {
        with(newData, glm(as.formula(sprintf("%s~%s 1",outcome, OFFSET)), family=family))
      }
    }, warning=function(err){
      NULL
    })
    
    #formulaRisk <- sprintf("%s~as.factor(risk_factors)",outcome)
    formulaRisk <- sprintf("%s~%s as.factor(risk_factors)",outcome, OFFSET)
    fit1 <- tryCatch({
      if(denominator=="denominator_asp_egg"){
        with(newData, glm(as.formula(formulaRisk), family=family, subset=(ASP_EGG>0)))
      } else {
        with(newData, glm(as.formula(formulaRisk), family=family))
      }
    }, warning=function(err){
      NULL
    })
    
    x <- data.frame(summary(pool(fit1)))
    x$var <- row.names(x)
    retval <- x[,c("estimate","std.error","p.value","var")]
    names(retval) <- c("beta","se","psingle","var")
    retval <- data.table(retval)
    retval <- retval[var!="(Intercept)"]
    
    Cp <- 1
    try({
      if(!is.null(fit1) & !is.null(fit0)){
        pc <- mice::pool.compare(fit1, fit0, method="Wald")
        Cp <- as.numeric(pc$pvalue)
      }
    }, TRUE)
    retval$Cp <- Cp
    
    retval[,crude:=RAWmisc::FormatEstCIFromEstSE(beta,se)]
    retval[,psingle:=RAWmisc::Format(psingle,3)]
    retval[,Cp:=RAWmisc::Format(Cp,3)]
    retval <- retval[,c("var","crude","psingle","Cp")]
    
    formulaInteractedAFCRisk <- sprintf("%s~%s AFC_TOTAL*as.factor(risk_factors)",outcome, OFFSET)
    fit1 <- tryCatch({
      if(denominator=="denominator_asp_egg"){
        with(newData, glm(as.formula(formulaInteractedAFCRisk), family=family, subset=(ASP_EGG>0)))
      } else {
        with(newData, glm(as.formula(formulaInteractedAFCRisk), family=family))
      }
    }, warning=function(err){
      NULL
    })
    
    formulaWithAFCRisk <- sprintf("%s~%s AFC_TOTAL + as.factor(risk_factors)",outcome, OFFSET)
    fit0 <- tryCatch({
      if(denominator=="denominator_asp_egg"){
        with(newData, glm(as.formula(formulaWithAFCRisk), family=family, subset=(ASP_EGG>0)))
      } else {
        with(newData, glm(as.formula(formulaWithAFCRisk), family=family))
      }
    }, warning=function(err){
      NULL
    })
    
    IntP <- 1
    try({
      if(!is.null(fit1) & !is.null(fit0)){
        pc <- mice::pool.compare(fit1, fit0, method="Wald")
        IntP <- as.numeric(pc$pvalue)
      }
    }, TRUE)
    
    retval[,IntP := RAWmisc::Format(IntP,3)]
    retval[,riskFactors:=paste0(c(riskFactors,protectiveFactors),collapse=", ")]
    retval[,denominator:=denominator]
    retval[,outcome:=outcome]
    openxlsx::write.xlsx(retval,file=file.path(folder,sprintf("%s_cum_riskfactor_%s.xlsx",denominator,outcome)))
    
    retval[,N:=length(c(riskFactors,protectiveFactors))]
    retval <- rbind(retval[1],retval)
    retval[1,var:="0"]
    retval[1,crude:="1 (ref)"]
    retval1[[index]] <- retval
    index <- index + 1
  }
}

retval1 <- rbindlist(retval1)
retval1[,outcome:=factor(outcome,levels=c("ASP_EGG","n_mature_egg","n_embryo_created","n_embryo_used"))]
setorder(retval1,-denominator,outcome,var)

retval1[,var:=sprintf("%s risk factors",stringr::str_replace_all(var,"as.factor\\(risk_factors\\)",""))]
retval1[,decision:="Non-significant cumulative risk factor"]
retval1[N==1,decision:="Only 1 risk factor"]
retval1[N>1 & Cp<"0.100",decision:="Borderline significant cumulative risk factor"]
retval1[N>1 & Cp<"0.050",decision:="Significant cumulative risk factor"]
#retval1[N>1 & IntP<"0.100",decision:="Borderline significant interaction with AFC"]
retval1[N>1 & IntP<"0.050",decision:="Significant interaction with AFC"]

retval1[crude!="1 (ref)",Cp:=""]
retval1[crude!="1 (ref)",IntP:=""]
retval1[crude!="1 (ref)",riskFactors:=""]
retval1[crude!="1 (ref)",decision:=""]
retval1[crude!="1 (ref)",outcome:=""]

retval1 <- retval1[,c("denominator","outcome","var","crude","psingle","Cp","IntP","riskFactors","decision")]
retval1
openxlsx::write.xlsx(retval1,file.path(org::PROJ$SHARED_TODAY,"FINAL_CUMULATIVE.xlsx"))

retval0 <- rbindlist(retval0)
retval0[,outcome:=factor(outcome,levels=c("ASP_EGG","n_mature_egg","n_embryo_created","n_embryo_used"))]
setorder(retval0,-denominator,outcome,var)
retval0 <- retval0[,c("denominator","outcome","var","L_crude","L_Cp","L_CIntp","L_adjusted","L_Ap","A_crude","A_Cp","A_CIntp","A_adjusted","A_Ap")]
setnames(retval0,c(
  "denominator",
  "outcome",
  "var",
  "LANA_crude_irr",
  "LANA_crude_pval",
  "LANA_crude_inter_pval",
  "LANA_adjusted_irr",
  "LANA_adjusted_pval",
  "ANAS_crude_irr",
  "ANAS_crude_pval",
  "ANAS_crude_inter_pval",
  "ANAS_adjusted_irr",
  "ANAS_adjusted_pval"
))
openxlsx::write.xlsx(retval0,file.path(org::PROJ$SHARED_TODAY,"FINAL_REGRESSIONS.xlsx"))

ExtractInteractedEffectEstimates <- function (beta, va, nameBase, nameInteractions, interactionValue = 1) 
{
  lincom <- matrix(0, ncol = ncol(beta), nrow = (1 + length(nameInteractions)))
  lincom[, which(names(beta) == nameBase)] <- 1
  for (i in 1:length(nameInteractions)) {
    lincom[i + 1, which(names(beta) == nameInteractions[i])] <- interactionValue
  }
  beta <- c(lincom %*% t(beta))
  se <- diag(sqrt(lincom%*%va%*%t(lincom)))
  p <- 2 * (1 - pnorm(abs(beta/se)))
  return(list(beta = beta, se = se, p = p))
}



est <- vector("list",40)
index <- 1
for(i in 1:19){
  for(riskFactors in c(1:2)){
    Q <- U <- c()
    for(j in 1:5){
      f <- fit1$analyses[[j]]
      
      beta=t(coef(f))
      names(beta) <- names(coef(f))
      
      val <- ExtractInteractedEffectEstimates(beta=beta,
                                     va=vcov(f),
                                     nameBase=sprintf("as.factor(risk_factors)%s",riskFactors),
                                     nameInteractions=sprintf("AFC_TOTAL:as.factor(risk_factors)%s",riskFactors),
                                     interactionValue=i)
      Q <- c(Q,val$beta[2])
      U <- c(U,val$se[2]^2)
    }
    r <- mice::pool.scalar(Q,U)
    b <- exp(r$qbar)
    l95 <- exp(r$qbar-1.96*sqrt(r$t))
    u95 <- exp(r$qbar+1.96*sqrt(r$t))
    est[[index]] <- data.frame(b,l95,u95,afc_total=as.numeric(i),riskFactors=riskFactors)
    index <- index+1
  }
}

est <- rbindlist(est)
est[,facet:=sprintf("Effect of %s risk factor(s) at different levels of AFC",riskFactors)]

q <- ggplot(est,aes(x=afc_total,y=b,ymin=l95,ymax=u95))
q <- q + geom_hline(yintercept=1,col="red")
q <- q + geom_pointrange()
q <- q + facet_wrap(~facet,ncol=1)
q <- q + geom_label(aes(label=RAWmisc::Format(b,2),y=u95+0.04))
q <- q + scale_y_continuous("Incidence rate ratio")
q <- q + scale_x_continuous("AFC")
q <- q + theme_gray(16)
q

RAWmisc::saveA4(q,file.path(org::PROJ$SHARED_TODAY,sprintf("FINAL_INTERACTION_%s_%s.png",outcome,denominator)))


modelsIndex <- modelsIndex + 1
models1[[modelsIndex]] <- data.frame(
  "outcome"="n_mature_egg",
  "method"="regression",
  "family"="quasipoisson",
  stringsAsFactors = FALSE)

strataD <- copy(d[["n_mature_egg"]][!is.na(ASP_EGG) & ASP_EGG>0])
strataD[,riskBMI:=BMI>median(BMI,na.rm=T)]
strataD[,riskSMOKE:=SMOKE_EVER]
strataD[,risk:=riskBMI+riskSMOKE]
xtabs(~strataD$risk)

strataD[,highAFC_TOTAL:=AFC_TOTAL>quantile(AFC_TOTAL,probs=0.25,na.rm=T)]

f <- glm(n_mature_egg ~ as.factor(risk) + offset(log(ASP_EGG)), data=strataD[highAFC_TOTAL==T], family="quasipoisson")
res <- as.data.frame(coef(summary(f)))
res$var <- row.names(res)
setDT(res)
res[,l95:=exp(Estimate-1.96*`Std. Error`)]
res[,u95:=exp(Estimate+1.96*`Std. Error`)]
res[,IRR:=exp(Estimate)]
res <- res[var!="(Intercept)",c("var","IRR","l95","u95","Pr(>|t|)")]
print(res)
openxlsx::write.xlsx(res,file.path(org::PROJ$SHARED_TODAY,"SENSITIVITY_OVER25thPERCENTILE_AFC.xlsx"))


f <- glm(n_mature_egg ~ as.factor(risk) + offset(log(ASP_EGG)), data=strataD[highAFC_TOTAL==F], family="quasipoisson")
res <- as.data.frame(coef(summary(f)))
res$var <- row.names(res)
setDT(res)
res[,l95:=exp(Estimate-1.96*`Std. Error`)]
res[,u95:=exp(Estimate+1.96*`Std. Error`)]
res[,IRR:=exp(Estimate)]
res <- res[var!="(Intercept)",c("var","IRR","l95","u95","Pr(>|t|)")]
print(res)
openxlsx::write.xlsx(res,file.path(org::PROJ$SHARED_TODAY,"SENSITIVITY_UNDER25thPERCENTILE_AFC.xlsx"))

