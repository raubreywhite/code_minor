RAWmisc::AllowFileManipulationFromInitialiseProject()
RAWmisc::InitialiseProject(
  HOME = "/git/code_minor/2017/fotis_ssri_rct_seasons/",
  RAW = "/analyses/data_raw/code_minor/2017/fotis_ssri_rct_seasons/",
  CLEAN = "/analyses/data_clean/code_minor/2017/fotis_ssri_rct_seasons",
  BAKED = "/analyses/results_baked/code_minor/2017/fotis_ssri_rct_seasons/",
  FINAL = "/analyses/results_final/code_minor/2017/fotis_ssri_rct_seasons/",
  SHARED = "/dropbox/results_shared/code_minor/2017/fotis_ssri_rct_seasons/"
)

dir.create(RAWmisc::PROJ$SHARED_TODAY)

library(data.table)
library(lme4)

FormatEstCIFromEstSE.int <- function(beta,se,digits=2,exp=TRUE){
  l95 <- beta-1.96*se
  u95 <- beta+1.96*se
  if(exp){
    beta <- exp(beta)
    l95 <- exp(l95)
    u95 <- exp(u95)
  }
  retval <- sprintf("%s (%s, %s)",
                    RAWmisc::Format(beta, digits),
                    RAWmisc::Format(l95, digits),
                    RAWmisc::Format(u95, digits)
  )
}
FormatEstCIFromEstSE <- Vectorize(FormatEstCIFromEstSE.int)

ExtractEffectEstimates <- function(beta, va, nameBase, nameInteractions){
  lincom <- matrix(0,ncol=ncol(beta),nrow=(1+length(nameInteractions)))
  lincom[,which(names(beta)==nameBase)] <- 1
  for(i in 1:length(nameInteractions)){
    lincom[i+1,which(names(beta)==nameInteractions[i])] <- 1
  }
  beta <- c(lincom%*%t(beta))
  se <- rep(0,length(beta))
  
  for(i in 1:length(beta)){
    vars <- diag(va)[which(lincom[i,]==TRUE)]
    id <- which(lincom[i,]==TRUE)
    covar <- va[id[1],id[2]]
    if(i==1){
      se[i] <- sqrt(vars)
    } else {
      se[i] <- sqrt(sum(vars)+2*covar)
    }
  }
  p <- 2*(1-pnorm(abs(beta/se)))
  return(list("beta"=beta,"se"=se,"p"=p))
}

d <- data.table(haven::read_dta(file.path(RAWmisc::PROJ$RAW,"FINAL DATA.dta")))
d[,isWinter:=0]
d[start_season==0,isWinter:=1]
nrow(d)
names(d)

xtabs(~d$id)
xtabs(~d$study)
xtabs(~d$medicine)
xtabs(~d$dropout)

xtabs(~d$ham_2)
xtabs(~d$remission_2)
xtabs(~d$response_2)

xtabs(~d$study+d$medicine)

d[,isActiveTreatment:=1]
d[medicine=="Placebo",isActiveTreatment:=0]

fitx <- lm(response_2~isActiveTreatment,data=d)
coef(fitx)

outcomes <- c("ham_2","ham_4","ham_6","ham_8",
              "remission_2","remission_4","remission_6","remission_8",
              "response_2","response_4","response_6","response_8")
results <- vector("list",length=100)
resultsIndex <- 0
pb <- RAWmisc::ProgressBarCreate(max=length(outcomes))
for(i in 1:length(outcomes)){
  RAWmisc::ProgressBarSet(pb,i)
  form0 <- sprintf("%s ~ isActiveTreatment + (1|study)", outcomes[i])
  form1 <- sprintf("%s ~ isActiveTreatment + factor(start_season) + (1|study)", outcomes[i])
  form2 <- sprintf("%s ~ isActiveTreatment*factor(start_season) + (1|study)", outcomes[i])
  
  if(stringr::str_extract(outcomes[i],"^[a-z][a-z][a-z]") %in% c("ham")){
    fit0 <- lme4::`glmer.nb`(as.formula(form0), data=d)
    fit1 <- lme4::`glmer.nb`(as.formula(form1), data=d)
    fit2 <- lme4::`glmer.nb`(as.formula(form2), data=d)
  } else {
    fit0 <- lme4::glmer(as.formula(form0), data=d, family=binomial)
    fit1 <- lme4::glmer(as.formula(form1), data=d, family=binomial)
    fit2 <- lme4::glmer(as.formula(form2), data=d, family=binomial)
  }
  
  resultsIndex <- resultsIndex + 1
  results[[resultsIndex]] <- as.data.frame(coef(summary(fit0)))
  results[[resultsIndex]]$var <- row.names(results[[resultsIndex]])
  setnames(results[[resultsIndex]],c("beta","se","z","p","var"))
  results[[resultsIndex]]$outcome <- outcomes[i]
  results[[resultsIndex]]$model <- 0
  results[[resultsIndex]]$pval0vs1 <- anova(fit1,fit0)$`Pr(>Chisq)`[2]
  results[[resultsIndex]]$pval1vs2 <- anova(fit2,fit1)$`Pr(>Chisq)`[2]
  
  resultsIndex <- resultsIndex + 1
  results[[resultsIndex]] <- as.data.frame(coef(summary(fit1)))
  results[[resultsIndex]]$var <- row.names(results[[resultsIndex]])
  setnames(results[[resultsIndex]],c("beta","se","z","p","var"))
  results[[resultsIndex]]$outcome <- outcomes[i]
  results[[resultsIndex]]$model <- 1
  results[[resultsIndex]]$pval0vs1 <- anova(fit1,fit0)$`Pr(>Chisq)`[2]
  results[[resultsIndex]]$pval1vs2 <- anova(fit2,fit1)$`Pr(>Chisq)`[2]
  
  resultsIndex <- resultsIndex + 1
  results[[resultsIndex]] <- as.data.frame(coef(summary(fit2)))
  results[[resultsIndex]]$var <- row.names(results[[resultsIndex]])
  setnames(results[[resultsIndex]],c("beta","se","z","p","var"))
  results[[resultsIndex]]$outcome <- outcomes[i]
  results[[resultsIndex]]$model <- 2
  results[[resultsIndex]]$pval0vs1 <- anova(fit1,fit0)$`Pr(>Chisq)`[2]
  results[[resultsIndex]]$pval1vs2 <- anova(fit2,fit1)$`Pr(>Chisq)`[2]
  
  strat <- ExtractEffectEstimates(
    beta=coef(fit2)$study[1,],
    va=vcov(fit2),
    nameBase="isActiveTreatment",
    nameInteractions=paste0("isActiveTreatment:factor(start_season)",1:3)
  )
  seasons <- c("Winter","Spring","Summer","Autumn")
  for(i in 1:4){
    results[[resultsIndex]]$var[i] <- sprintf("isActiveTreatment(%s)",seasons[i])
    results[[resultsIndex]]$beta[i] <- strat$beta[i]
    results[[resultsIndex]]$se[i] <- strat$se[i]
    results[[resultsIndex]]$p[i] <- strat$p[i]
  }
  results[[resultsIndex]] <- results[[resultsIndex]][1:4,]
}

results <- rbindlist(results)

results[,est:=FormatEstCIFromEstSE(beta,se)]

final <- results[stringr::str_detect(var,"^isActiveTreatment"),c("outcome","model","var","est","p","pval0vs1","pval1vs2")]
final[p<0.05,est:=paste0(est,"*")]
final[,p:=RAWmisc::Format(p,3)]
final[,pval0vs1:=RAWmisc::Format(pval0vs1,3)]
final[,pval1vs2:=RAWmisc::Format(pval1vs2,3)]

openxlsx::write.xlsx(final, file=file.path(RAWmisc::PROJ$SHARED_TODAY,"mixed_effects_models_season.xlsx"))


#### WINTER

results <- vector("list",length=100)
resultsIndex <- 0
pb <- RAWmisc::ProgressBarCreate(max=length(outcomes))
for(i in 1:length(outcomes)){
  RAWmisc::ProgressBarSet(pb,i)
  form0 <- sprintf("%s ~ isActiveTreatment + (1|study)", outcomes[i])
  form1 <- sprintf("%s ~ isActiveTreatment + isWinter + (1|study)", outcomes[i])
  form2 <- sprintf("%s ~ isActiveTreatment*isWinter + (1|study)", outcomes[i])
  
  if(stringr::str_extract(outcomes[i],"^[a-z][a-z][a-z]") %in% c("ham")){
    fit0 <- lme4::`glmer.nb`(as.formula(form0), data=d)
    fit1 <- lme4::`glmer.nb`(as.formula(form1), data=d)
    fit2 <- lme4::`glmer.nb`(as.formula(form2), data=d)
  } else {
    fit0 <- lme4::glmer(as.formula(form0), data=d, family=binomial)
    fit1 <- lme4::glmer(as.formula(form1), data=d, family=binomial)
    fit2 <- lme4::glmer(as.formula(form2), data=d, family=binomial)
  }
  
  resultsIndex <- resultsIndex + 1
  results[[resultsIndex]] <- as.data.frame(coef(summary(fit0)))
  results[[resultsIndex]]$var <- row.names(results[[resultsIndex]])
  setnames(results[[resultsIndex]],c("beta","se","z","p","var"))
  results[[resultsIndex]]$outcome <- outcomes[i]
  results[[resultsIndex]]$model <- 0
  results[[resultsIndex]]$pval0vs1 <- anova(fit1,fit0)$`Pr(>Chisq)`[2]
  results[[resultsIndex]]$pval1vs2 <- anova(fit2,fit1)$`Pr(>Chisq)`[2]
  
  resultsIndex <- resultsIndex + 1
  results[[resultsIndex]] <- as.data.frame(coef(summary(fit1)))
  results[[resultsIndex]]$var <- row.names(results[[resultsIndex]])
  setnames(results[[resultsIndex]],c("beta","se","z","p","var"))
  results[[resultsIndex]]$outcome <- outcomes[i]
  results[[resultsIndex]]$model <- 1
  results[[resultsIndex]]$pval0vs1 <- anova(fit1,fit0)$`Pr(>Chisq)`[2]
  results[[resultsIndex]]$pval1vs2 <- anova(fit2,fit1)$`Pr(>Chisq)`[2]
  
  resultsIndex <- resultsIndex + 1
  results[[resultsIndex]] <- as.data.frame(coef(summary(fit2)))
  results[[resultsIndex]]$var <- row.names(results[[resultsIndex]])
  setnames(results[[resultsIndex]],c("beta","se","z","p","var"))
  results[[resultsIndex]]$outcome <- outcomes[i]
  results[[resultsIndex]]$model <- 2
  results[[resultsIndex]]$pval0vs1 <- anova(fit1,fit0)$`Pr(>Chisq)`[2]
  results[[resultsIndex]]$pval1vs2 <- anova(fit2,fit1)$`Pr(>Chisq)`[2]
  
  strat <- ExtractEffectEstimates(
    beta=coef(fit2)$study[1,],
    va=vcov(fit2),
    nameBase="isActiveTreatment",
    nameInteractions="isActiveTreatment:isWinter"
  )
  seasons <- c("Not winter","Winter")
  for(i in 1:2){
    results[[resultsIndex]]$var[i] <- sprintf("isActiveTreatment(%s)",seasons[i])
    results[[resultsIndex]]$beta[i] <- strat$beta[i]
    results[[resultsIndex]]$se[i] <- strat$se[i]
    results[[resultsIndex]]$p[i] <- strat$p[i]
  }
  results[[resultsIndex]] <- results[[resultsIndex]][1:2,]
}

results <- rbindlist(results)

results[,est:=FormatEstCIFromEstSE(beta,se)]

final <- results[stringr::str_detect(var,"^isActiveTreatment"),c("outcome","model","var","est","p","pval0vs1","pval1vs2")]
final[p<0.05,est:=paste0(est,"*")]
final[,p:=RAWmisc::Format(p,3)]
final[,pval0vs1:=RAWmisc::Format(pval0vs1,3)]
final[,pval1vs2:=RAWmisc::Format(pval1vs2,3)]

openxlsx::write.xlsx(final, file=file.path(RAWmisc::PROJ$SHARED_TODAY,"mixed_effects_models_winter_vs_not.xlsx"))

