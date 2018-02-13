# Participants All women in the file fulfill the criteria for inclusion to the 
# BASIC study (18 years of age or older, Swedish speaking, without confidential 
# data and with non-pathological pregnancy diagnoses at the routine ultrasound) 
# The file includes only women that is non-smoking, not having twins and are not
# on cortisone medication at time of blood sampling and who were at a visit at 
# approx. 8 weeks postpartum (n=178).
# 
# The groups for the main analysis (variable “main_analysis_groups”): (A or --) 
# women who screened negative for depressive symptoms during all the surveys 
# filled in (EPDS<13 in gestational week 17 and 32 and EPDS<12 at 6 and 8 weeks 
# postpartum) and were negative for ongoing depression episode at the MINI 
# interview (n=107). (BC or x+) women who screened positive for depression 
# symptoms postpartum (EPDS>12 at 6 and 8 weeks postpartum) and/or were positive
# for ongoing depression at the MINI interview  This group also included women 
# who were on antidepressants at time of blood sampling regardless of screening 
# results (8)) (n=62).
# 
# The group for the sensitivity analysis (variable 
# “sensitivity_analysis_groups”): (A or --) women who screened negative for 
# depressive symptoms during all the surveys filled in (EPDS<13 in gestational 
# week 17 and 32 and EPDS<12 at 6 and 8 weeks postpartum) and were negative for 
# ongoing depression episode at the MINI interview (n=107). (B or -+) women who 
# screened negative for depressive symptoms during the pregnancy (EPDS<13 in 
# gestational week 17 and 32) but positive after the delivery (EPDS>12 at 6 or 8
# weeks postpartum) or had an ongoing depression according to the MINI interview
# (n=22). This group also include women without antidepressants in pregnancy but
# with antidepressants postpartum. (C or ++) women who developed symptoms of 
# depression during pregnancy (EPDS>13 in gestational week 17 or 32) and 
# remained with symptoms of depression after the delivery (EPDS>12 at 6 or 8 
# weeks postpartum) or had an ongoing depression according to the MINI interview
# (n=40). This group also include women on antidepressants in pregnancy and 
# postpartum.
# 
# The file also includes women with depression in pregnancy who became healthy 
# postpartum (group D or +-). These women we will probably exclude since there 
# are only 9 of them (exclude in variable “include_pp”). In the main analysis, 
# we could change the A group to AC by including these women (AC or x-, (n=116))
# but I think I would prefer to have them excluded. What’s your opinion?
# 
# Markers The markers are still without the LOD/Sqrt(2) value (kept blank if 
# missing) and the markers that have more than 50% missing are placed in the 
# bottom (21 markers). For the markers we include (71 markers) we should insert 
# the LOD/Sqrt(2) value. I have not yet organized the markers in the order I 
# want to present them. I hope that it is possible to re-arrange them in the 
# tables you are making?
# 
# Covariates I have included in the file variables for a lot of covariates: Age 
# at time of delivery, Pre- pregnancy BMI, Education (grouped into high school 
# level or higher), Parity, infant gender, Premature, Delivery mode (two 
# variables), IVF-treatment, marital status, pregnancy complications, 
# breastfeeding, History of depression, History of inflammatory or autoimmune 
# diseases, Days from partus, medication at time of blood sampling (8 variables)
# and other MINI diagnoses (12 variables). I thought we could see if any of 
# these are effecting the results, and those who are not we could probably keep 
# only the once normally used in the literature? Is that ok to do so? 
# Unfortunately I am having trouble understanding the coding of MINI variable 
# GAD and some women in the file are coded 9999. For these women (9212, 9749, 
# 10408, 10554, 10795, 10829) I will get back to you after I have understood how
# I should translate their answers…
# 
# And we also a question mark regarding breastfeeding. In the variable for 
# breastfeeding there at the moment “exclusively breastfeeding”, “breastfeeding 
# and formula” and “formula only”. If we need to make this variable dichotomous 
# I don’t know where “breastfeeding and formula” should go. It feels like even 
# the small amount of breastfeeding in these women will affect the inflammation 
# markers (being more similar to exclusively breastfeeding) but the possibility 
# to give formula and get some help and perhaps some sleep would more affect the
# depression status (being more similar to formula only).. Perhaps a question 
# for Alkistis and Emma ?
# 
# Also, let me know if you are missing any covariate that possibly should be 
# included (Alkistis and Emma).
# 
# Tables
# 
# What we want to have is:

# 1. Demographic data / Descriptive univariate analyses  – This I will fix
# 
# 2. Inflammation markers vs. confounders. (If we think it would be
# 
# 3.
#
# a) Inflammation markers vs outcome (A -- vs. BC x+) Mann-Whitney, 
# Mann-Whitney-Bonferroni, Logistic regression, Logistic regression-Bonferroni, 
# Adjusted logistic regression, LASSO, Elastic Net? Is it possible to get all of
# these analyses in one big table?
# 
# b) logistic regressions for A -- vs B-+ and 
# A-- vs C++  and B-+ vs C++  for the significant Table 3a analyses
# 
# What do you think of, that for the table in 3a, if we could combine the 
# picture with red and green and the table from the last article by making a 
# table with p-values and for those who are significantly upregulated in 
# controls we write the p-value in green and for those who are significantly 
# upregulated in cases we write the p-value in red and for the un-significant 
# once we write the p-value in grey? Would it be possible?
# 
# Inflammation summary variable
# 
# And lastly, how should we do with the inflammation summary variable? Where to 
# be included?



# A (--) = sensitivity_analysis_groups=1 (n=107)
# B (-+) = sensitivity_analysis_groups=2 (n=22)
# C (++) = sensitivity_analysis_groups=3 (n=40)
# D (+-) = sensitivity_analysis_groups=4 (n=9) [EXCLUDE]

# 
# My comments: Main analysis: A vs BC sounds like a nice clean comparison to me.
# Markers: It is easy to rearrange the markers in the final tables Covariates:
# "See if any of these are effecting the results" => Yes, I can run model 1:
# outcome = exposure, and model 2: outcome = exposure + confounder1, and then
# compare the effect estimates between the two models. If the change in effect
# estimate is >10%, then we have some evidence that it may be a confounder. I
# can do this for all of the IFs and the summary IFs, and then we can try to
# make an informed decision on which confounders to include. Breastfeeding: It
# is possible to convert this into a fractional continuous variable. E.g. "No
# BF"=0, "Full BF"=1, "BF + Formula"=0.5 (so they get half as much breastmilk as
# a kid who fully BFs). Tables/graphs for 3A: This should be possible. We might
# want to consider dropping the mann-whitney analyses though, as in the last
# paper the reviewers were confused by their inclusion (they thought we were
# overdoing it). It is easy to make an argument for crude logistic regression ->
# adjusted logistic regression -> LASSO (+EN) [argument=they are just more
# advanced versions of each other], but it is harder to make that argument with
# mann-whitney [non-parametric] and it makes the table even bigger and more
# confusing than it needs to be. Graphs for 3B: I think the reviewers were
# really confused by the red/green graph in the last paper. I think we should
# probably try to make this one a table if possible. IF summary variable: I
# think it should be included as "just another IF" in Table 3 for the
# crude/adjusted logistic regressions, but not for the LASSO/EN. What do you
# think?


RAWmisc::AllowFileManipulationFromInitialiseProject()
RAWmisc::InitialiseProject(
  HOME = "/git/code_minor/2017/emma_depression_pp_if/",
  RAW = "/analyses/data_raw/code_minor/2017/emma_depression_pp_if/",
  CLEAN = "/analyses/data_clean/code_minor/2017/emma_depression_pp_if",
  BAKED = "/analyses/results_baked/code_minor/2017/emma_depression_pp_if/",
  FINAL = "/analyses/results_final/code_minor/2017/emma_depression_pp_if/",
  SHARED = "/dropbox/results_shared/code_minor/2017/emma_depression_pp_if/"
)
dir.create(RAWmisc::PROJ$SHARED_TODAY)
library(data.table)
library(ggplot2)
library(mice)

d <- haven::read_sav(file.path(RAWmisc::PROJ$RAW,"PParticle 170915EB_2 .sav"))
nrow(d)
mean(d$im_log2_134_SLAMF1_pp,na.rm=T)

xtabs(~d$sensitivity_analysis_groups)
xtabs(~d$main_analysis_groups)

d$outcomeMain <- d$main_analysis_groups-1
d$outcomeMain[d$sensitivity_analysis_groups==4] <- NA
xtabs(~d$outcomeMain)
d$outcomeAvsB <- as.numeric(d$outcomeMain)
d$outcomeAvsB[d$sensitivity_analysis_groups==3] <- NA
xtabs(~d$outcomeAvsB)
############MAYBE FIX THIS???
for(i in 1:10) warning("MAYBE FIX THIS")
for(i in 1:10) warning("MAYBE FIX THIS")
#d$outcomeMain <- d$outcomeAvsB

# Fixing mini confounders
d$mini <- 0
for(i in c(
  "mini_mani_emma",
  "mini_panikattacker_emma",
  "mini_panisyndrom_emma",
  "mini_agorafobi_emma",
  "mini_socialfobi_emma",
  "mini_tvångssyndrom_emma",
  "mini_alkohol_eller_missbruk_emma",
  "mini_ptsd_emma",
  "mini_atstorning_emma",
  "mini_gad_emma",
  "mini_primemd_angest_emma",
  "mini_psykos_emma")){
  
  d$mini[d[[i]] %in% c(1,2)] <- 1
  d$mini[d$mini==0 & d[[i]]>1000] <- NA
  d$mini[d$mini==0 & is.na(d[[i]])] <- NA
}
d <- data.table(d)


# Fixing LODS
# im_103_BDNF_pp
IFs <- names(d)
IFs <- IFs[stringr::str_detect(IFs,"^im_log2_")]
underLOD <- names(d)
underLOD <- underLOD[stringr::str_detect(underLOD,"^underLOD_")]

percUnderLOD <- vector("list",length=length(underLOD))
for(i in 1:length(percUnderLOD)){
  percUnderLOD[[i]] <- data.frame(perc=mean(d[[underLOD[i]]],na.rm=T),var=underLOD[i])
}
percUnderLOD <- rbindlist(percUnderLOD)
percUnderLOD[,IFs:=IFs]

IFs <- percUnderLOD[perc<0.5]$IFs
IFandZ <- c(IFs,"Infl_RWall_meanZPP")

# Determining confounders
confoundersRemoved <- c(
  "parity_emma",
  "deliverymode_emma"
)

confoundersPossible <- c(
  "age_at_partus_emma",
  "prepregnancy_bmi_emma",                           
  "education",                             
  "employment",
  "parity_dichotomous",
  "infant_gender_emma",
  "premature_emma",
  "deliverymode_dichotomous",
  "ivf_emma",
  "ppv6_marital_status",
  "pregnancy_complications_emma",
  "breastfeeding_6vpp",
  "history_of_depression",
  "infl_reum_disease_emma",
  "days_from_partus_emma",
  "antidepressives_emma",
  "antibiotics_emma",
  "astma_allergy_emma",
  "hormones_emma",
  "nsaid_emma",
  "paracetamol_emma",
  "levaxin_emma",
  "bloodpressure_med_emma",
  "smoking_pp_emma",
  "mini")

for(i in confoundersPossible){
  print("****")
  print(i)
  print(d[[i]])
}

d$breastfeeding_6vpp <- (d$breastfeeding_6vpp-1)/2

results <- vector("list",10000)
resultsIndex <- 1
for(i in IFs){
  formulaBase <- sprintf("%s ~ %s", "outcomeMain",i)
  fitBase <- glm(as.formula(formulaBase),data=d)
  for(j in confoundersPossible){
    formulaC1 <- sprintf("%s ~ %s + %s", "outcomeMain",i,j)
    fitC1 <- glm(as.formula(formulaC1),data=d)
    retval <- data.frame(confounder=j,coefBase=coef(fitBase)[i],coefAdjusted=coef(fitC1)[i],pvalBase=coef(summary(fitBase))[2,4])
    results[[resultsIndex]] <- retval
    resultsIndex <- resultsIndex + 1
  }
}
results <- rbindlist(results)
results[,change:=(coefAdjusted-coefBase)/coefBase]
results[,changeMoreThan10:=0]
results[abs(change)>0.1,changeMoreThan10:=1]
confounders <- results[pvalBase<0.05,.(averageChange=mean(abs(change)),changeMoreThan10perc=mean(changeMoreThan10)),by=confounder]
setorder(confounders,-changeMoreThan10perc)
confounders

openxlsx::write.xlsx(confounders, file.path(RAWmisc::PROJ$SHARED_TODAY,"confounders.xlsx"))

#confoundersDecided <- c("breastfeeding_6vpp","antidepressives_emma","mini","history_of_depression")
confoundersDecided <- c("breastfeeding_6vpp",
                        "mini",
                        "age_at_partus_emma",
                        "history_of_depression",
                        "employment",
                        "bloodpressure_med_emma",
                        "nsaid_emma",
                        "levaxin_emma")

# Table 3 A
# main_analysis_groups 1 (A --) vs 2 (BC x+)
# Table 3 B
# sensitivity_analysis_groups 1 (A --) vs 2 (B -+), 1 (A --) vs 3 (C ++), 2 (B -+) vs 3 (C ++)
dataUnimp <- d[!is.na(outcomeMain),c("outcomeMain",confoundersDecided,IFandZ),with=F]
dataUnimp[,x:=rnorm(.N)]
dataUnimp[1,x:=NA]
dataImp <- mice(dataUnimp, method="pmm", seed=4, m=10)

res <- vector("list",1000)
resIndex <- 1
for(i in 1:length(IFandZ)) for(m in c("crude","adjusted")) {
  outcome <- "outcomeMain"
  if(m=="crude"){
    formula <- sprintf("%s ~ %s",outcome,IFandZ[i])  
  } else if(m=="adjusted"){
    formula <- sprintf("%s ~ %s + %s",outcome,IFandZ[i],paste(confoundersDecided,collapse="+"))
  }
  
  fit <- with(dataImp, glm2::glm2(as.formula(formula)))
  retval <- data.frame(summary(pool(fit)))
  retval$var <- row.names(retval)
  retval$varNum <- 1:nrow(retval)
  retval$model <- m
  
  retval$outcomeCases <- sum(dataUnimp[[outcome]]==1)
  retval$outcomeControls <- sum(dataUnimp[[outcome]]==0)
  retval$outcomeMissing <- sum(is.na(dataUnimp[[outcome]]))
  
  m <- dataImp$m
  td <- vector("list",length=m)
  for (j in 1:m) {
    td[[j]] <- data.table(complete(dataImp, j))
    td[[j]][,id:=1:.N]
    td[[j]][,imp:=j]
  }
  td <- rbindlist(td)
  
  td <- td[,
    .(
      Q=unlist(lapply(.SD,mean)),
      U=unlist(lapply(.SD,var)),
      N=.N
      ),
  by=.(outcomeMain,imp),
  .SDcols=IFandZ[i]]
  td[,SD:=U]
  td[,U:=as.numeric(U/N)]
  td[,N:=NULL]
  
  r <- pool.scalar(td[outcomeMain==1]$Q, td[outcomeMain==1]$U, method = "rubin")   # Rubin 1987
  retval$meanExposureCases <- r$qbar
  r <- pool.scalar(td[outcomeMain==1]$SD, rep(0,length=sum(td$outcomeMain==1)), method = "rubin")   # Rubin 1987
  retval$meanExposureCasesSD <- r$qbar
  
  r <- pool.scalar(td[outcomeMain==0]$Q, td[outcomeMain==0]$U, method = "rubin")   # Rubin 1987
  retval$meanExposureControls <- r$qbar
  r <- pool.scalar(td[outcomeMain==0]$SD, rep(0,length=sum(td$outcomeMain==0)), method = "rubin")   # Rubin 1987
  retval$meanExposureControlsSD <- r$qbar
  
  res[[resIndex]] <- retval
  resIndex <- resIndex + 1
}

res <- rbindlist(res)
setnames(res,"Pr...t..","p")
res <- res[varNum==2]



res[varNum==2 & model=="crude" & p<0.05]

sum(res[varNum==2 & model=="crude"]$p<(0.05/71))
sum(res[varNum==2 & model=="adjusted"]$p<(0.05/71))

res[varNum==2 & model=="crude"]

set.seed(4)
lasso <- vector("list",dataImp$m)
for(i in 1:dataImp$m){
  fitData <- complete(dataImp,i)
  fitData <- fitData[-ncol(fitData)] # remove x
  fitData <- fitData[-ncol(fitData)] # remove meanZPP
  fit <- glmnet::cv.glmnet(as.matrix(fitData)[,-1],fitData[,1], nfolds=nrow(dataUnimp))
  lasso[[i]] <- data.frame(as.matrix(coef(fit)))
  lasso[[i]]$var <- row.names(lasso[[i]])
}

lasso <- rbindlist(lasso)
lasso <- lasso[,.(est_lasso=mean(X1)),by=var]

lasso[est_lasso!=0]
sum(lasso$est_lasso!=0)

res[,se:=NULL]
res[,t:=NULL]
res[,df:=NULL]
res[,fmi:=NULL]
res[,lambda:=NULL]
res[,varNum:=NULL]
res <- dcast.data.table(res,var~model,value.var = c("est","p","lo.95","hi.95","nmis","outcomeCases","outcomeControls","outcomeMissing","meanExposureCases","meanExposureCasesSD","meanExposureControls","meanExposureControlsSD"))
res[var=="im_log2_101_IL8_pp"]

finalRes <- merge(res,lasso,by="var",all.x=T)
finalRes[,crude:=sprintf("%s (%s, %s)",
                         RAWmisc::Format(exp(est_crude),2),
                         RAWmisc::Format(exp(lo.95_crude),2),
                         RAWmisc::Format(exp(hi.95_crude),2))]
finalRes[,adjusted:=sprintf("%s (%s, %s)",
                         RAWmisc::Format(exp(est_adjusted),2),
                         RAWmisc::Format(exp(lo.95_adjusted),2),
                         RAWmisc::Format(exp(hi.95_adjusted),2))]
finalRes[,p_crude_bonf:=stats::p.adjust(p_crude,method="bonf")]
finalRes[,p_adjusted_bonf:=stats::p.adjust(p_adjusted,method="bonf")]

finalRes[,p_crude_fdr:=stats::p.adjust(p_crude,method="BH")]
finalRes[,p_adjusted_fdr:=stats::p.adjust(p_adjusted,method="BH")]

cols <- c("p_crude","p_crude_bonf","p_adjusted","p_adjusted_bonf")
finalRes[ , (cols) := lapply(.SD, RAWmisc::Format, 3), .SDcols = cols]
finalRes[, lasso := RAWmisc::Format(exp(est_lasso),3)]
finalRes[est_lasso != 0, lasso := paste0(lasso,"*")]

finalRes[,exposureCases:=sprintf("%s (%s)",
                                 RAWmisc::Format(meanExposureCases_crude,2),
                                 RAWmisc::Format(meanExposureCasesSD_crude,2))]

finalRes[,exposureControls:=sprintf("%s (%s)",
                                 RAWmisc::Format(meanExposureControls_crude,2),
                                 RAWmisc::Format(meanExposureControlsSD_crude,2))]

finalRes <- finalRes[,c(
  "var",
  "nmis_crude",
  "outcomeMissing_crude",
  "outcomeCases_crude",
  "outcomeControls_crude",
  "exposureCases",
  "exposureControls",
  "crude",
  "p_crude",
  "p_crude_bonf",
  "adjusted",
  "p_adjusted",
  "p_adjusted_bonf",
  "lasso"
),with=F]

finalRes[stringr::str_detect(lasso,"\\*")]

openxlsx::write.xlsx(finalRes, file.path(RAWmisc::PROJ$SHARED_TODAY,"A_vs_BC.xlsx"))



significantIFs <- finalRes[stringr::str_detect(lasso,"\\*")]$var

# sensitivity_analysis_groups 1 (A --) vs 2 (B -+), 1 (A --) vs 3 (C ++), 2 (B -+) vs 3 (C ++)
xtabs(~d$sensitivity_analysis_groups)
dataUnimp <- d[!is.na(outcomeMain),c("sensitivity_analysis_groups",confoundersDecided,significantIFs),with=F]

dataBaselineAvsB <- dataUnimp[sensitivity_analysis_groups %in% c(1,2)]
dataBaselineAvsC <- dataUnimp[sensitivity_analysis_groups %in% c(1,3)]
dataBaselineBvsC <- dataUnimp[sensitivity_analysis_groups %in% c(2,3)]

dataBaselineAvsB[,sensitivity_analysis_groups:=sensitivity_analysis_groups-1]
dataBaselineAvsC[,sensitivity_analysis_groups:=round((sensitivity_analysis_groups-1)/2)]
dataBaselineBvsC[,sensitivity_analysis_groups:=sensitivity_analysis_groups-2]

xtabs(~dataBaselineAvsB$sensitivity_analysis_groups)
xtabs(~dataBaselineAvsC$sensitivity_analysis_groups)
xtabs(~dataBaselineBvsC$sensitivity_analysis_groups)

res <- vector("list",1000)
resIndex <- 1
for(i in 1:length(significantIFs)) for(comparison in c("AvsB","AvsC","BvsC")) for(m in c("crude","adjusted")) {
  outcome <- "sensitivity_analysis_groups"
  if(comparison=="AvsB"){
    data <- dataBaselineAvsB
  } else if(comparison=="AvsC"){
    data <- dataBaselineAvsC
  } else if(comparison=="BvsC"){
    data <- dataBaselineBvsC
  } 
  
  if(m=="crude"){
    formula <- sprintf("%s ~ %s",outcome,significantIFs[i])  
  } else if(m=="adjusted"){
    formula <- sprintf("%s ~ %s + %s",outcome,significantIFs[i],paste(confoundersDecided,collapse="+"))
  }
  
  fit <- glm2::glm2(as.formula(formula),data=data)
  retval <- data.frame(coef(summary(fit)))
  retval$var <- row.names(retval)
  retval$varNum <- 1:nrow(retval)
  retval$comparison <- comparison
  retval$model <- m
  
  retval$outcomeCases <- sum(data[[outcome]]==1)
  retval$outcomeControls <- sum(data[[outcome]]==0)
  retval$outcomeMissing <- sum(is.na(data[[outcome]]))
  
  retval$meanExposureCases <- mean(data[sensitivity_analysis_groups==1][[significantIFs[i]]],na.rm=T)
  retval$meanExposureCasesSD <- sd(data[sensitivity_analysis_groups==1][[significantIFs[i]]],na.rm=T)
  
  retval$meanExposureControls <- mean(data[sensitivity_analysis_groups==0][[significantIFs[i]]],na.rm=T)
  retval$meanExposureControlsSD <- sd(data[sensitivity_analysis_groups==0][[significantIFs[i]]],na.rm=T)
  
  res[[resIndex]] <- retval
  resIndex <- resIndex + 1
}

res <- rbindlist(res)
setnames(res,"Pr...t..","p")
setnames(res,"Std..Error","se")
res <- res[varNum==2]

res[,est:=sprintf("%s (%s, %s)",
                  RAWmisc::Format(exp(Estimate),2),
                  RAWmisc::Format(exp(Estimate-1.96*se),2),
                  RAWmisc::Format(exp(Estimate+1.96*se),2)
                  )]

res[,exposureCases:=sprintf("%s (%s)",
                                 RAWmisc::Format(meanExposureCases,2),
                                 RAWmisc::Format(meanExposureCasesSD,2))]

res[,exposureControls:=sprintf("%s (%s)",
                                    RAWmisc::Format(meanExposureControls,2),
                                    RAWmisc::Format(meanExposureControlsSD,2))]

IFvals <- unique(res[model=="crude",c("var","exposureCases","exposureControls","comparison")])
IFvals[comparison %in% c("AvsB","AvsC"),A:=exposureControls]
IFvals[comparison %in% c("AvsB"),B:=exposureCases]
IFvals[comparison %in% c("BvsC"),B:=exposureControls]
IFvals[comparison %in% c("BvsC","AvsC"),C:=exposureCases]

IFvals <- unique(IFvals[,c("var","A","B","C")])
IFvals <- unique(na.omit(melt.data.table(IFvals,id="var")))
IFvals <- dcast.data.table(IFvals, variable ~ var)

openxlsx::write.xlsx(IFvals, file.path(RAWmisc::PROJ$SHARED_TODAY,"sensitivity_IF_values.xlsx"))

res <- res[,c("var","outcomeMissing","outcomeCases","outcomeControls","est","p","comparison","model")]
res[,p:=RAWmisc::Format(p,3)]
res <- melt.data.table(res,id=c("var","model","comparison"))
res[,model:=factor(model,levels=c("crude","adjusted"))]
res <- dcast.data.table(res,comparison+model+variable~var)

openxlsx::write.xlsx(res, file.path(RAWmisc::PROJ$SHARED_TODAY,"sensitivity_logistic_regression_values.xlsx"))


toPlot <- dataUnimp[,c("sensitivity_analysis_groups",significantIFs),with=F]
toPlot[,sensitivity_analysis_groups:=factor(sensitivity_analysis_groups,levels=c(1,2,3))]
levels(toPlot$sensitivity_analysis_groups) <- c("A (--)","B (-+)","C (++)")
toPlot[,id:=1:.N]

toPlot <- melt(toPlot,id=c("id","sensitivity_analysis_groups"))

q <- ggplot(toPlot,aes(x=sensitivity_analysis_groups,y=value))
q <- q + geom_boxplot()
q <- q + facet_wrap(~variable,scales="free")
q <- q + scale_x_discrete("")
q <- q + scale_y_continuous("log2 NPX values")
q <- q + theme_gray(16)
RAWmisc::saveA4(q,filename=file.path(RAWmisc::PROJ$SHARED_TODAY,"sensitivity_boxplots.png"))



