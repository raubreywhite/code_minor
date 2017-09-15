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

lana[,caffeine_daily:=caffeine_daily*2.2]

lana[,phs_length_m:=phs_length_m/100]

lana[,SMOKE_DAILY_NOW:=SMOKE_DAILY_NOW_UPPSTART]

#################

lana[,PAL_total:=PAL_TOTAL]
lana[,AFC_TOTAL:=AFC_ULJ]
lana[,reason_infert_Q:=reason_infert_Q_uppstart]

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

masterdata[,ULJ_HOGER_AFC:=as.numeric(stringr::str_replace_all(ULJ_HOGER_AFC," ",""))]
masterdata[,ULJ_VANSTER_AFC:=as.numeric(stringr::str_replace_all(ULJ_VANSTER_AFC," ",""))]
masterdata[is.na(ULJ_HOGER_AFC),ULJ_HOGER_AFC:=0]
masterdata[is.na(ULJ_VANSTER_AFC),ULJ_VANSTER_AFC:=0]

masterdata[,AFC_TOTAL:=ULJ_HOGER_AFC+ULJ_VANSTER_AFC]

masterdata[final_trying_time_years>100,final_trying_time_years:=NA]

# ADDITIONAL CLEANING
masterdata[Livebirth_all==2,Livebirth_all:=NA]
masterdata[livebirth_ET==2,livebirth_ET:=NA]

#"HEALTH_HYPERTH",
#"HEALTH_HYPOTH",
#"HEALTH_MUSCLE_PAIN",


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


########
# explanatory analyses
explan <- vector("list",length=length(variablesFirst))
for(i in 1:length(variablesFirst)){
  fit <- glm(as.formula(sprintf("ASP_EGG~%s",variablesFirst[i])),data=masterdata,family="quasipoisson")
  temp <- as.data.frame(coef(summary(fit)))
  temp$var <- row.names(temp)
  explan[[i]] <- temp
}
explan <- rbindlist(explan)
setnames(explan,c("beta","se","t","p","var"))
explan <- explan[var!="(Intercept)"]
explan[,useInModel:=""]
explan[p<0.1,useInModel:="Yes"]
explan[,p:=RAWmisc::Format(p,3)]
explan
openxlsx::write.xlsx(explan,file=file.path(RAWmisc::PROJ$SHARED_TODAY,"univariable_asp_egg.xlsx"))

explan <- vector("list",length=length(variablesSecond))
for(i in 1:length(variablesSecond)){
  fit <- glm(as.formula(sprintf("livebirth_ET~%s",variablesSecond[i])),data=masterdata,family=binomial)
  temp <- as.data.frame(coef(summary(fit)))
  temp$var <- row.names(temp)
  explan[[i]] <- temp
}
explan <- rbindlist(explan)
setnames(explan,c("beta","se","t","p","var"))
explan <- explan[var!="(Intercept)"]
explan[p<0.1,useInModel:="Yes"]
explan[,p:=RAWmisc::Format(p,3)]
explan
openxlsx::write.xlsx(explan,file=file.path(RAWmisc::PROJ$SHARED_TODAY,"univariable_livebirth_ET.xlsx"))

#########

modelExposures <- list()
modelExposures[["null"]] <- "1"

modelExposures[["first_reduced_linear"]] <- "
X_AGE+
factor(INF_INTERCOURSE_3MNT_FRQ)+
SMOKE_DAILY_NOW+
BMI+
AFC_TOTAL"

modelExposures[["first_reduced_spline"]] <- "
splines::ns(X_AGE,2)+
factor(INF_INTERCOURSE_3MNT_FRQ)+
SMOKE_DAILY_NOW+
splines::ns(BMI,2)+
splines::ns(AFC_TOTAL,2)"

modelExposures[["first_full_linear"]] <- "
X_AGE+
factor(INF_INTERCOURSE_3MNT_FRQ)+
education+
SMOKE_DAILY_NOW+
alcohol+
gravid+
BMI+
depression_ever+
factor(reason_infert_Q)+
PAL_total+
final_trying_time_years+
caffeine_daily+
AMH+
AFC_TOTAL+
phs_length_m"

modelExposures[["first_full_spline"]] <- "
splines::ns(X_AGE,2)+
factor(INF_INTERCOURSE_3MNT_FRQ)+
education+
SMOKE_DAILY_NOW+
alcohol+
gravid+
splines::ns(BMI,2)+
depression_ever+
factor(reason_infert_Q)+
splines::ns(PAL_total,2)+
splines::ns(final_trying_time_years,2)+
splines::ns(caffeine_daily,2)+
splines::ns(AMH,2)+
splines::ns(AFC_TOTAL,2)+
splines::ns(phs_length_m,2)"

modelExposures[["second_reduced_linear"]] <- "
depression_ever+
phs_length_m+
n_embryo_created+
n_embryo_used"

modelExposures[["second_reduced_spline"]] <- "
depression_ever+
splines::ns(phs_length_m,2)+
splines::ns(n_embryo_created,2)+
splines::ns(n_embryo_used,2)"

modelExposures[["second_full_linear"]] <- "
  X_AGE+
factor(INF_INTERCOURSE_3MNT_FRQ)+
education+
SMOKE_DAILY_NOW+
alcohol+
gravid+
BMI+
depression_ever+
factor(reason_infert_Q)+
PAL_total+
final_trying_time_years+
caffeine_daily+
AMH+
AFC_TOTAL+
phs_length_m+
ASP_EGG+
n_embryo_created+
n_embryo_used"

modelExposures[["second_full_spline"]] <- "
splines::ns(X_AGE,2)+
factor(INF_INTERCOURSE_3MNT_FRQ)+
education+
SMOKE_DAILY_NOW+
alcohol+
gravid+
splines::ns(BMI,2)+
depression_ever+
factor(reason_infert_Q)+
splines::ns(PAL_total,2)+
splines::ns(final_trying_time_years,2)+
splines::ns(caffeine_daily,2)+
splines::ns(AMH,2)+
splines::ns(AFC_TOTAL,2)+
splines::ns(phs_length_m,2)+
splines::ns(ASP_EGG)+
splines::ns(n_embryo_created)+
splines::ns(n_embryo_used)"

categoricalExposures <- list()
categoricalExposures[["first_full_linear"]] <- c(
  "INF_INTERCOURSE_3MNT_FRQ",
  "reason_infert_Q"
)
categoricalExposures[["second_full_linear"]] <- c(
  "INF_INTERCOURSE_3MNT_FRQ",
  "reason_infert_Q"
)

continuousExposures <- list()
continuousExposures[["first_full_linear"]] <- c(
  "INF_INTERCOURSE_3MNT_FRQ",
  "X_AGE",
  "education",
  "SMOKE_DAILY_NOW",
  "alcohol",
  "gravid",
  "BMI",
  "depression_ever",
  "PAL_total",
  "final_trying_time_years",
  "caffeine_daily",
  "AMH",
  "AFC_TOTAL",
  "phs_length_m"
)
continuousExposures[["second_full_linear"]] <- c(
  "INF_INTERCOURSE_3MNT_FRQ",
  "X_AGE",
  "education",
  "SMOKE_DAILY_NOW",
  "alcohol",
  "gravid",
  "BMI",
  "depression_ever",
  "PAL_total",
  "final_trying_time_years",
  "caffeine_daily",
  "AMH",
  "AFC_TOTAL",
  "phs_length_m",
  "ASP_EGG",
  "n_embryo_created",
  "n_embryo_used"
)

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



table(d[["Livebirth_all"]]$Livebirth_all,useNA="always")
table(d[["livebirth_ET"]]$livebirth_ET,useNA="always")

table(masterdata$Livebirth_all,useNA="always")

### INSPECT DATA

xd <- apply(d[["Livebirth_all"]],2,mean,na.rm=T)
xl <- apply(l[["Livebirth_all"]],2,mean,na.rm=T)

percs <- round((xl-xd)/xd*100)

xd[abs(percs)>20]
xl[abs(percs)>20]


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
  "modelType"="null"
  ,stringsAsFactors = FALSE)

modelsIndex <- modelsIndex + 1
models1[[modelsIndex]] <- data.frame(
  "outcome"="egg_asp",
  "method"="regression",
  "family"="binomial",
  "modelType"="first_reduced_linear",
  stringsAsFactors = FALSE)

modelsIndex <- modelsIndex + 1
models1[[modelsIndex]] <- data.frame(
  "outcome"="egg_asp",
  "method"="regression",
  "family"="binomial",
  "modelType"="first_reduced_spline",
  stringsAsFactors = FALSE)

modelsIndex <- modelsIndex + 1
models1[[modelsIndex]] <- data.frame(
  "outcome"="egg_asp",
  "method"="regression",
  "family"="binomial",
  "modelType"="first_full_linear",
  stringsAsFactors = FALSE)

modelsIndex <- modelsIndex + 1
models1[[modelsIndex]] <- data.frame(
  "outcome"="egg_asp",
  "method"="regression",
  "family"="binomial",
  "modelType"="first_full_spline",
  stringsAsFactors = FALSE)

#### ASP_EGG
modelsIndex <- modelsIndex + 1
models1[[modelsIndex]] <- data.frame(
  "outcome"="ASP_EGG",
  "method"="regression",
  "family"="quasipoisson",
  "modelType"="null",
  stringsAsFactors = FALSE)

modelsIndex <- modelsIndex + 1
models1[[modelsIndex]] <- data.frame(
  "outcome"="ASP_EGG",
  "method"="regression",
  "family"="quasipoisson",
  "modelType"="first_reduced_linear",
  stringsAsFactors = FALSE)

modelsIndex <- modelsIndex + 1
models1[[modelsIndex]] <- data.frame(
  "outcome"="ASP_EGG",
  "method"="regression",
  "family"="quasipoisson",
  "modelType"="first_reduced_spline",
  stringsAsFactors = FALSE)

modelsIndex <- modelsIndex + 1
models1[[modelsIndex]] <- data.frame(
  "outcome"="ASP_EGG",
  "method"="regression",
  "family"="quasipoisson",
  "modelType"="first_full_linear",
  stringsAsFactors = FALSE)

modelsIndex <- modelsIndex + 1
models1[[modelsIndex]] <- data.frame(
  "outcome"="ASP_EGG",
  "method"="regression",
  "family"="quasipoisson",
  "modelType"="first_full_spline",
  stringsAsFactors = FALSE)

#### n_embryo_created
modelsIndex <- modelsIndex + 1
models1[[modelsIndex]] <- data.frame(
  "outcome"="n_embryo_created",
  "method"="regression",
  "family"="quasipoisson",
  "modelType"="null",
  stringsAsFactors = FALSE)

modelsIndex <- modelsIndex + 1
models1[[modelsIndex]] <- data.frame(
  "outcome"="n_embryo_created",
  "method"="regression",
  "family"="quasipoisson",
  "modelType"="first_reduced_linear",
  stringsAsFactors = FALSE)

modelsIndex <- modelsIndex + 1
models1[[modelsIndex]] <- data.frame(
  "outcome"="n_embryo_created",
  "method"="regression",
  "family"="quasipoisson",
  "modelType"="first_reduced_spline",
  stringsAsFactors = FALSE)

modelsIndex <- modelsIndex + 1
models1[[modelsIndex]] <- data.frame(
  "outcome"="n_embryo_created",
  "method"="regression",
  "family"="quasipoisson",
  "modelType"="first_full_linear",
  stringsAsFactors = FALSE)

modelsIndex <- modelsIndex + 1
models1[[modelsIndex]] <- data.frame(
  "outcome"="n_embryo_created",
  "method"="regression",
  "family"="quasipoisson",
  "modelType"="first_full_spline",
  stringsAsFactors = FALSE)

#### n_embryo_used
modelsIndex <- modelsIndex + 1
models1[[modelsIndex]] <- data.frame(
  "outcome"="n_embryo_used",
  "method"="regression",
  "family"="quasipoisson",
  "modelType"="null",
  stringsAsFactors = FALSE)

modelsIndex <- modelsIndex + 1
models1[[modelsIndex]] <- data.frame(
  "outcome"="n_embryo_used",
  "method"="regression",
  "family"="quasipoisson",
  "modelType"="first_reduced_linear",
  stringsAsFactors = FALSE)

modelsIndex <- modelsIndex + 1
models1[[modelsIndex]] <- data.frame(
  "outcome"="n_embryo_used",
  "method"="regression",
  "family"="quasipoisson",
  "modelType"="first_reduced_spline",
  stringsAsFactors = FALSE)

modelsIndex <- modelsIndex + 1
models1[[modelsIndex]] <- data.frame(
  "outcome"="n_embryo_used",
  "method"="regression",
  "family"="quasipoisson",
  "modelType"="first_full_linear",
  stringsAsFactors = FALSE)

modelsIndex <- modelsIndex + 1
models1[[modelsIndex]] <- data.frame(
  "outcome"="n_embryo_used",
  "method"="regression",
  "family"="quasipoisson",
  "modelType"="first_full_spline",
  stringsAsFactors = FALSE)

#### n_mature_egg
modelsIndex <- modelsIndex + 1
models1[[modelsIndex]] <- data.frame(
  "outcome"="n_mature_egg",
  "method"="regression",
  "family"="quasipoisson",
  "modelType"="null",
  stringsAsFactors = FALSE)

modelsIndex <- modelsIndex + 1
models1[[modelsIndex]] <- data.frame(
  "outcome"="n_mature_egg",
  "method"="regression",
  "family"="quasipoisson",
  "modelType"="first_reduced_linear",
  stringsAsFactors = FALSE)

modelsIndex <- modelsIndex + 1
models1[[modelsIndex]] <- data.frame(
  "outcome"="n_mature_egg",
  "method"="regression",
  "family"="quasipoisson",
  "modelType"="first_reduced_spline",
  stringsAsFactors = FALSE)

modelsIndex <- modelsIndex + 1
models1[[modelsIndex]] <- data.frame(
  "outcome"="n_mature_egg",
  "method"="regression",
  "family"="quasipoisson",
  "modelType"="first_full_linear",
  stringsAsFactors = FALSE)

modelsIndex <- modelsIndex + 1
models1[[modelsIndex]] <- data.frame(
  "outcome"="n_mature_egg",
  "method"="regression",
  "family"="quasipoisson",
  "modelType"="first_full_spline",
  stringsAsFactors = FALSE)

#### ET
modelsIndex <- modelsIndex + 1
models1[[modelsIndex]] <- data.frame(
  "outcome"="ET",
  "method"="regression",
  "family"="binomial",
  "modelType"="null",
  stringsAsFactors = FALSE)

modelsIndex <- modelsIndex + 1
models1[[modelsIndex]] <- data.frame(
  "outcome"="ET",
  "method"="regression",
  "family"="binomial",
  "modelType"="first_reduced_linear",
  stringsAsFactors = FALSE)

modelsIndex <- modelsIndex + 1
models1[[modelsIndex]] <- data.frame(
  "outcome"="ET",
  "method"="regression",
  "family"="binomial",
  "modelType"="first_reduced_spline",
  stringsAsFactors = FALSE)

modelsIndex <- modelsIndex + 1
models1[[modelsIndex]] <- data.frame(
  "outcome"="ET",
  "method"="regression",
  "family"="binomial",
  "modelType"="first_full_linear",
  stringsAsFactors = FALSE)

modelsIndex <- modelsIndex + 1
models1[[modelsIndex]] <- data.frame(
  "outcome"="ET",
  "method"="regression",
  "family"="binomial",
  "modelType"="first_full_spline",
  stringsAsFactors = FALSE)

#### pos_preg_all
modelsIndex <- modelsIndex + 1
models1[[modelsIndex]] <- data.frame(
  "outcome"="pos_preg_all",
  "method"="regression",
  "family"="binomial",
  "modelType"="null",
  stringsAsFactors = FALSE)

modelsIndex <- modelsIndex + 1
models1[[modelsIndex]] <- data.frame(
  "outcome"="pos_preg_all",
  "method"="regression",
  "family"="binomial",
  "modelType"="first_reduced_linear",
  stringsAsFactors = FALSE)

modelsIndex <- modelsIndex + 1
models1[[modelsIndex]] <- data.frame(
  "outcome"="pos_preg_all",
  "method"="regression",
  "family"="binomial",
  "modelType"="first_reduced_spline",
  stringsAsFactors = FALSE)

modelsIndex <- modelsIndex + 1
models1[[modelsIndex]] <- data.frame(
  "outcome"="pos_preg_all",
  "method"="regression",
  "family"="binomial",
  "modelType"="first_full_linear",
  stringsAsFactors = FALSE)

modelsIndex <- modelsIndex + 1
models1[[modelsIndex]] <- data.frame(
  "outcome"="pos_preg_all",
  "method"="regression",
  "family"="binomial",
  "modelType"="first_full_spline",
  stringsAsFactors = FALSE)

#### pos_preg_ET
modelsIndex <- modelsIndex + 1
models1[[modelsIndex]] <- data.frame(
  "outcome"="pos_preg_ET",
  "method"="regression",
  "family"="binomial",
  "modelType"="null",
  stringsAsFactors = FALSE)

modelsIndex <- modelsIndex + 1
models1[[modelsIndex]] <- data.frame(
  "outcome"="pos_preg_ET",
  "method"="regression",
  "family"="binomial",
  "modelType"="second_reduced_linear",
  stringsAsFactors = FALSE)

modelsIndex <- modelsIndex + 1
models1[[modelsIndex]] <- data.frame(
  "outcome"="pos_preg_ET",
  "method"="regression",
  "family"="binomial",
  "modelType"="second_reduced_spline",
  stringsAsFactors = FALSE)

modelsIndex <- modelsIndex + 1
models1[[modelsIndex]] <- data.frame(
  "outcome"="pos_preg_ET",
  "method"="regression",
  "family"="binomial",
  "modelType"="second_full_linear",
  stringsAsFactors = FALSE)

modelsIndex <- modelsIndex + 1
models1[[modelsIndex]] <- data.frame(
  "outcome"="pos_preg_ET",
  "method"="regression",
  "family"="binomial",
  "modelType"="second_full_spline",
  stringsAsFactors = FALSE)

#### miscarriage
modelsIndex <- modelsIndex + 1
models1[[modelsIndex]] <- data.frame(
  "outcome"="miscarriage",
  "method"="regression",
  "family"="binomial",
  "modelType"="null",
  stringsAsFactors = FALSE)

modelsIndex <- modelsIndex + 1
models1[[modelsIndex]] <- data.frame(
  "outcome"="miscarriage",
  "method"="regression",
  "family"="binomial",
  "modelType"="first_reduced_linear",
  stringsAsFactors = FALSE)

modelsIndex <- modelsIndex + 1
models1[[modelsIndex]] <- data.frame(
  "outcome"="miscarriage",
  "method"="regression",
  "family"="binomial",
  "modelType"="first_reduced_spline",
  stringsAsFactors = FALSE)

modelsIndex <- modelsIndex + 1
models1[[modelsIndex]] <- data.frame(
  "outcome"="miscarriage",
  "method"="regression",
  "family"="binomial",
  "modelType"="first_full_linear",
  stringsAsFactors = FALSE)

modelsIndex <- modelsIndex + 1
models1[[modelsIndex]] <- data.frame(
  "outcome"="miscarriage",
  "method"="regression",
  "family"="binomial",
  "modelType"="first_full_spline",
  stringsAsFactors = FALSE)

#### Livebirth_all
modelsIndex <- modelsIndex + 1
models1[[modelsIndex]] <- data.frame(
  "outcome"="Livebirth_all",
  "method"="regression",
  "family"="binomial",
  "modelType"="null",
  stringsAsFactors = FALSE)

modelsIndex <- modelsIndex + 1
models1[[modelsIndex]] <- data.frame(
  "outcome"="Livebirth_all",
  "method"="regression",
  "family"="binomial",
  "modelType"="first_reduced_linear",
  stringsAsFactors = FALSE)

modelsIndex <- modelsIndex + 1
models1[[modelsIndex]] <- data.frame(
  "outcome"="Livebirth_all",
  "method"="regression",
  "family"="binomial",
  "modelType"="first_reduced_spline",
  stringsAsFactors = FALSE)

modelsIndex <- modelsIndex + 1
models1[[modelsIndex]] <- data.frame(
  "outcome"="Livebirth_all",
  "method"="regression",
  "family"="binomial",
  "modelType"="first_full_linear",
  stringsAsFactors = FALSE)

modelsIndex <- modelsIndex + 1
models1[[modelsIndex]] <- data.frame(
  "outcome"="Livebirth_all",
  "method"="regression",
  "family"="binomial",
  "modelType"="first_full_spline",
  stringsAsFactors = FALSE)

#### livebirth_ET
modelsIndex <- modelsIndex + 1
models1[[modelsIndex]] <- data.frame(
  "outcome"="livebirth_ET",
  "method"="regression",
  "family"="binomial",
  "modelType"="null",
  stringsAsFactors = FALSE)

modelsIndex <- modelsIndex + 1
models1[[modelsIndex]] <- data.frame(
  "outcome"="livebirth_ET",
  "method"="regression",
  "family"="binomial",
  "modelType"="second_reduced_linear",
  stringsAsFactors = FALSE)

modelsIndex <- modelsIndex + 1
models1[[modelsIndex]] <- data.frame(
  "outcome"="livebirth_ET",
  "method"="regression",
  "family"="binomial",
  "modelType"="second_reduced_spline",
  stringsAsFactors = FALSE)

modelsIndex <- modelsIndex + 1
models1[[modelsIndex]] <- data.frame(
  "outcome"="livebirth_ET",
  "method"="regression",
  "family"="binomial",
  "modelType"="second_full_linear",
  stringsAsFactors = FALSE)

modelsIndex <- modelsIndex + 1
models1[[modelsIndex]] <- data.frame(
  "outcome"="livebirth_ET",
  "method"="regression",
  "family"="binomial",
  "modelType"="second_full_spline",
  stringsAsFactors = FALSE)


retval1 <- vector("list",length=1000)
predictiveAbility <- vector("list",length=1000)
pb <- RAWmisc::ProgressBarCreate(min=0,max=modelsIndex)
for(i in 1:modelsIndex){
  RAWmisc::ProgressBarSet(pb, i)
  
  outcome <- models1[[i]]$outcome
  family <- models1[[i]]$family
  modelType <- models1[[i]]$modelType
  formula1 <- sprintf("%s ~ %s",models1[[i]]$outcome,modelExposures[[models1[[i]]$modelType]])
  formula0 <- sprintf("%s ~ 1",models1[[i]]$outcome)
  useData <- data[[outcome]]
  valData <- ldata[[outcome]]
    
  fit1 <- with(useData, glm(as.formula(formula1), family=family))
  x <- with(valData, glm(as.formula(formula1), family=family))
  retval1[[i]] <- data.table(summary(pool(fit1)))
  retval1[[i]][,outcome:=outcome]
  retval1[[i]][,modelType:=modelType]
  
  if(FALSE){
    f <- gee::gee(as.formula(formula1),id=1:nrow(complete(useData,1)),data=complete(useData,1))
    p <- predict(f)
    q <- predict(fit1$analyses[[1]])
  }
  
  fit0 <- with(useData, glm2::glm2(as.formula(formula0), family=family))
  tryCatch({
    pc <- mice::pool.compare(fit1, fit0, method="Wald")
    retval1[[i]][,modelPvalue:=pc$pvalue]
  }, error=function(err){
    retval1[[i]][,modelPvalue:=1.0]
  })
  
  valv2 <- r2 <- rep(NA,5)
  val <- pred <- list("vector",5)
  for(j in 1:5){
    pred[[j]] <- data.table(complete(useData,j))
    pred[[j]][,p:=predict(fit1$analyses[[j]],complete(useData,j),type="response")]
    pred[[j]][,id:=1:.N]
    
    val[[j]] <- data.table(complete(valData,j))
    val[[j]][,p:=predict(fit1$analyses[[j]],complete(valData,j),type="response")]
    val[[j]][,id:=1:.N]
    
    if(family=="quasipoisson"){
      f <- MASS::glm.nb(as.formula(formula1), data=complete(useData,j))
      r2[j] <- pscl::pR2(f)["r2ML"]
    }
  }
  pred <- rbindlist(pred)
  pred <- pred[, lapply(.SD, mean, na.rm=TRUE), by=id ]
  setnames(pred,models1[[i]]$outcome,"outcome")
 
  pred[,errorModel:=outcome-p]
  pred[,errorNull:=outcome-mean(outcome)]
  
  pred <- pred[,.(
    rmseModel=sqrt(mean(errorModel^2)),
    rmseNull=sqrt(mean(errorNull^2))
  )]
  
  val <- rbindlist(val)
  val <- val[, lapply(.SD, mean, na.rm=TRUE), by=id ]
  setnames(val,models1[[i]]$outcome,"outcome")
  
  val[,errorModel:=outcome-p]
  val[,errorNull:=outcome-mean(outcome)]
  
  val <- val[,.(
    rmseModel=sqrt(mean(errorModel^2)),
    rmseNull=sqrt(mean(errorNull^2))
  )]
  
  retval1[[i]][,rmseModel:=pred[["rmseModel"]]]
  retval1[[i]][,rmseNull:=pred[["rmseNull"]]]
  retval1[[i]][,r2:=RAWmisc::Format(mean(r2),2)]
  retval1[[i]][,lana_rmseModel:=val[["rmseModel"]]]
  #retval1[[i]][,lana_rmseNull:=val[["rmseNull"]]]
}


retval1 <- rbindlist(retval1)
retval1[,rmsePercDecrease:=round((rmseModel-rmseNull)/rmseNull*100,1)]
retval1[modelType=="null",temp:=lana_rmseModel]
retval1[,temp:=mean(temp,na.rm=T),by=.(outcome)]
retval1[,lana_rmsePercDecrease:=round((lana_rmseModel-temp)/temp*100,1)]
retval1[,temp:=NULL]

for(i in c("rmseModel","rmseNull","lana_rmseModel","lana_rmseNull")) retval1[[i]] <- RAWmisc::Format(retval1[[i]],digits=2)
for(i in c("rmsePercDecrease","lana_rmsePercDecrease")) retval1[[i]] <- RAWmisc::Format(retval1[[i]],digits=1)
x <- unique(retval1[,c("outcome","modelType","modelPvalue","rmseModel","rmseNull","rmsePercDecrease","lana_rmseModel","lana_rmsePercDecrease","r2")])
x[,significantAfterBonferroni:=""]
x[.N*modelPvalue<0.05,significantAfterBonferroni:="Yes"]
x[,modelPvalue:=RAWmisc::Format(modelPvalue,3)]
x[significantAfterBonferroni=="Yes"]

openxlsx::write.xlsx(x,file=file.path(RAWmisc::PROJ$SHARED_TODAY,"regression_models.xlsx"))
























######## LASSO


models2 <- vector("list",length=100)

#### egg_asp
modelsIndex <- 1

models2[[modelsIndex]] <- data.frame(
  "outcome"="egg_asp",
  "method"="lasso",
  "family"="binomial",
  "modelType"="first_full_linear",
  stringsAsFactors = FALSE)

#### ASP_EGG

modelsIndex <- modelsIndex + 1
models2[[modelsIndex]] <- data.frame(
  "outcome"="ASP_EGG",
  "method"="lasso",
  "family"="poisson",
  "modelType"="first_full_linear",
  stringsAsFactors = FALSE)

#### n_embryo_created

modelsIndex <- modelsIndex + 1
models2[[modelsIndex]] <- data.frame(
  "outcome"="n_embryo_created",
  "method"="lasso",
  "family"="poisson",
  "modelType"="first_full_linear",
  stringsAsFactors = FALSE)

#### n_embryo_used

modelsIndex <- modelsIndex + 1
models2[[modelsIndex]] <- data.frame(
  "outcome"="n_embryo_used",
  "method"="lasso",
  "family"="poisson",
  "modelType"="first_full_linear",
  stringsAsFactors = FALSE)

#### n_mature_egg

modelsIndex <- modelsIndex + 1
models2[[modelsIndex]] <- data.frame(
  "outcome"="n_mature_egg",
  "method"="lasso",
  "family"="poisson",
  "modelType"="first_full_linear",
  stringsAsFactors = FALSE)

#### ET

modelsIndex <- modelsIndex + 1
models2[[modelsIndex]] <- data.frame(
  "outcome"="ET",
  "method"="lasso",
  "family"="binomial",
  "modelType"="first_full_linear",
  stringsAsFactors = FALSE)

#### pos_preg_all

modelsIndex <- modelsIndex + 1
models2[[modelsIndex]] <- data.frame(
  "outcome"="pos_preg_all",
  "method"="lasso",
  "family"="binomial",
  "modelType"="first_full_linear",
  stringsAsFactors = FALSE)

#### pos_preg_ET

modelsIndex <- modelsIndex + 1
models2[[modelsIndex]] <- data.frame(
  "outcome"="pos_preg_ET",
  "method"="lasso",
  "family"="binomial",
  "modelType"="second_full_linear",
  stringsAsFactors = FALSE)

#### miscarriage

modelsIndex <- modelsIndex + 1
models2[[modelsIndex]] <- data.frame(
  "outcome"="miscarriage",
  "method"="lasso",
  "family"="binomial",
  "modelType"="first_full_linear",
  stringsAsFactors = FALSE)

#### Livebirth_all

modelsIndex <- modelsIndex + 1
models2[[modelsIndex]] <- data.frame(
  "outcome"="Livebirth_all",
  "method"="lasso",
  "family"="binomial",
  "modelType"="first_full_linear",
  stringsAsFactors = FALSE)

#### livebirth_ET

modelsIndex <- modelsIndex + 1
models2[[modelsIndex]] <- data.frame(
  "outcome"="livebirth_ET",
  "method"="lasso",
  "family"="binomial",
  "modelType"="second_full_linear",
  stringsAsFactors = FALSE)

retval2 <- vector("list",length=1000)
pb <- RAWmisc::ProgressBarCreate(min=0,max=modelsIndex)
for(i in 1:modelsIndex){
  RAWmisc::ProgressBarSet(pb, i)
  
  outcome <- models2[[i]]$outcome
  family <- models2[[i]]$family
  modelType <- models2[[i]]$modelType
  formula1 <- sprintf("%s ~ %s",models2[[i]]$outcome,paste(c(categoricalExposures[[modelType]],continuousExposures[[modelType]]),collapse="+"))
  useData <- data[[outcome]]
  
  res <- coefs <- fit <- vector("list",length=5)
  for(j in 1:5){
    fitData <- complete(useData,j)
    for(k in categoricalExposures[[modelType]]) fitData[[k]] <- factor(fitData[[k]])
    set.seed(4)
    fit[[j]] <- glmnetUtils::cv.glmnet(
      as.formula(formula1),
      data=fitData,
      family=family,
      alpha=1,
      nfolds=nrow(fitData))
  
    coefs[[j]] <- as.matrix(coef(fit[[j]]))
    coefs[[j]] <- data.frame(var=row.names(coefs[[j]]),coefs[[j]])
  }
  
  coefs <- rbindlist(coefs)
  coefs <- coefs[,.(est=mean(X1)),by=var]
  
  pred <- list("vector",5)
  for(j in 1:5){
    fitData <- complete(useData,j)
    pData <- rep(1,nrow(fitData))
    for(k in categoricalExposures[[modelType]]){
      pData <- cbind(pData,
                     model.matrix(as.formula(sprintf("%s ~ -1 + factor(%s)",outcome,k)),
                                  fitData))
    }
    for(k in continuousExposures[[modelType]]){
      pData <- cbind(pData,fitData[[k]])
    }
    p <- pData %*% matrix(coefs[[2]],ncol=1)
    if(family=="binomial"){
      p <- boot::inv.logit(p)
    } else if(family=="poisson"){
      p <- exp(p)
    }
    pred[[j]] <- data.table(complete(useData,j))
    pred[[j]][,p:=p]
    pred[[j]][,id:=1:.N]
  }

  pred <- rbindlist(pred)
  pred <- pred[, lapply(.SD, mean, na.rm=TRUE), by=id ]
  setnames(pred,models2[[i]]$outcome,"outcome")
  
  q <- ggplot(pred,aes(y=outcome-p,x=p))
  q <- q + geom_point()
  q <- q + geom_hline(yintercept=0)
  #q
  
  pred[,errorModel:=outcome-p]
  pred[,errorNull:=outcome-mean(outcome)]
  
  pred <- pred[,.(
    rmseModel=sqrt(mean(errorModel^2)),
    rmseNull=sqrt(mean(errorNull^2))
  )]
  
  retval2[[i]] <- data.table(outcome=outcome,modelType=modelType,outcome=outcome)
  
  retval2[[i]][,rmseModel:=pred[["rmseModel"]]]
  retval2[[i]][,rmseNull:=pred[["rmseNull"]]]
}


retval2 <- rbindlist(retval2)
retval2[,rmsePercDecrease:=round((rmseModel-rmseNull)/rmseModel*100,1)]

cat("REGRESSION")
unique(retval1[,c("outcome","modelType","modelPvalue","rmseModel","rmseNull","rmsePercDecrease","r2")])

cat("LASSO")
retval2




