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
  HOME = "/git/code_minor/emma_depression_pp_if/",
  RAW = "/analyses/data_raw/code_minor/emma_depression_pp_if/",
  CLEAN = "/analyses/data_clean/code_minor/emma_depression_pp_if",
  BAKED = "/analyses/results_baked/code_minor/emma_depression_pp_if/",
  FINAL = "/analyses/results_final/code_minor/emma_depression_pp_if/",
  SHARED = "/dropbox/results_shared/emma_depression_pp_if/"
)

library(data.table)

d <- haven::read_sav(file.path(RAWmisc::PROJ$RAW,"PParticle 170713EB.sav"))
nrow(d)

xtabs(~d$sensitivity_analysis_groups)
xtabs(~d$main_analysis_groups)

d$outcomeMain <- d$main_analysis_groups-1

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


# Determining confounders
IFs <- names(d)[49:119]
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
confounders <- results[pvalBase<0.05,.(change=mean(change)),by=confounder]
setorder(confounders,change)

confoundersDecided <- c("breastfeeding_6vpp","antidepressives_emma","mini")

# Table 3 A
# main_analysis_groups 1 (A --) vs 2 (BC x+)
# Table 3 B
# sensitivity_analysis_groups 1 (A --) vs 2 (B -+), 1 (A --) vs 3 (C ++), 2 (B -+) vs 3 (C ++)



