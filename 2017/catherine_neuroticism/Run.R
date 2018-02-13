RAWmisc::AllowFileManipulationFromInitialiseProject()
RAWmisc::InitialiseProject(
  HOME = "/git/code_minor/2017/catherine_neuroticism/",
  RAW = "/analyses/data_raw/code_minor/2017/catherine_neuroticism/",
  CLEAN = "/analyses/data_clean/code_minor/2017/catherine_neuroticism",
  BAKED = "/analyses/results_baked/code_minor/2017/catherine_neuroticism/",
  FINAL = "/analyses/results_final/code_minor/2017/catherine_neuroticism/",
  SHARED = "/dropbox/results_shared/code_minor/2017/catherine_neuroticism/")

suppressWarnings(suppressMessages(library(data.table)))
suppressWarnings(suppressMessages(library(ggplot2)))
suppressWarnings(suppressMessages(library(rms)))

d <- data.table(haven::read_spss(file.path(RAWmisc::PROJ$RAW,"Patricia SSP_MFR CA_180121.sav")))
d <- d[Filter_1para_singleton==1]
nrow(d)

OUTCOMES <- c("Gestational_diabetes_CA",
  "Gestational_diabetes_LGA_CA",
  "Gestational_HT_PE_CA",
  "Planned_CS_analysis_CA",
  "Induction_analysis_CA",
  "Emergency_CS_analysis_CA",
  "VE_analysis_CA",
  "Uterine_inertia_analysis_CA",
  "Severe_lacerations_analysis_CA",
  "Placental_retention_analysis_CA",
  "Postpartum_hemorrhage_CA",
  "Premature_w37_analysis_CA",
  "SGA_analysis_CA",
  "LGA_analysis_CA",
  "Apgar5_sub7_CA",
  "Asphyxia_distress_CA",
  "Composite_worstcase_wide_CA",
  "Composite_worstcase_strict_CA")

POTENTIAL_CONFOUNDERS_BINARY <- c(
  "Education_cat_CA",
  "ROKGRAV_CA",
  "Previous_psychiatric_morbidity_CA"
)

POTENTIAL_CONFOUNDERS_ORDINAL <- c(
  "MALDER_cat_CA",
  "MBMI_cat_CA_new",
  "MLANGD_cat_CA"
)

CONFOUNDERS <- c(
  POTENTIAL_CONFOUNDERS_BINARY,
  POTENTIAL_CONFOUNDERS_ORDINAL
)

d <- d[,c(
  "Neuroticism",
  OUTCOMES,
  CONFOUNDERS
),with=F]

for(i in POTENTIAL_CONFOUNDERS_ORDINAL){
  d[[i]] <- as.factor(d[[i]])
}

library(rms)
library(ggplot2)

# CATHRINE'S BASIC OPTION

ddist <- datadist(d)
ddist$limits$Neuroticism[2] <- 292 ##### SETTING REFERENCE VALUE FOR NEUROTICISM
options(datadist='ddist')

fit <- lrm(Gestational_diabetes_CA ~ rcs(Neuroticism,knots=4),
           x=TRUE, y=TRUE, data=d)
print(fit)

##### RUNNING THE MODEL
fit <- lrm(Gestational_diabetes_CA ~ rcs(Neuroticism,knots=4),
           x=TRUE, y=TRUE, data=d)
print(fit)
#### GETTING OUT THE ESTIMATED LOG-ODDS RATIOS AND 95% CIS FOR DIFFERENT POINTS
p <- Predict(fit, Neuroticism=sort(c(292,261,324, 200, 250, 300, 350, 400, 450, 500)), ref.zero=T)
p <- data.frame(p)
# converting from log-odds ratio to odds ratio
p$yhat <- exp(p$yhat)
p$lower <- exp(p$lower)
p$upper <- exp(p$upper)
print(p)

nrow(d)

# exposures
p50 <- BASELINE <- 292
p25 <- 261
p75 <- 324
POINTS <- seq(200,450,50)

#### TOTAL ANXIETY
linearResults <- linearResultsAdjusted <- vector("list", length = length(OUTCOMES))
results <- vector("list",length(OUTCOMES))
resultsAdjusted <- vector("list",length(OUTCOMES))
for(i in 1:length(OUTCOMES)){
  # making the variable "outcome" equal to the appropriate OUTCOME
  # (e.g. copying "Planned_CS_analysis_CA" into "outcome")
  d$outcome <- d[[OUTCOMES[i]]]
  
  ddist <- datadist(d)
  ddist$limits$Neuroticism[2] <- BASELINE ##### SETTING REFERENCE VALUE FOR CHOLESTEROL
  options(datadist='ddist')
  
  ##### RUNNING THE MODEL (CRUDE) 
  fitx <- lrm(outcome ~ Neuroticism,
              x=TRUE, y=TRUE, data=d)
  fit0 <- lrm(outcome ~ 1,
              x=TRUE, y=TRUE, data=d[!is.na(outcome)])
  fit1 <- lrm(outcome ~ rcs(Neuroticism,c(250,300,350)),
              x=TRUE, y=TRUE, data=d)
  p <- Predict(fit1, Neuroticism=sort(c(BASELINE,POINTS,p25,p75)), ref.zero=T)
  # saving log OR + 95% CI
  results[[i]] <- data.frame(p)
  linearResults[[i]] <- as.data.frame(summary(fitx))
  linearResults[[i]]$var <- row.names(linearResults[[i]])
  linearResults[[i]]$aic_spline_minus_linear <- AIC(fit1) - AIC(fitx)
  linearResults[[i]]$outcome <- OUTCOMES[i]
  linearResults[[i]]$numerator <- sum(d$outcome,na.rm=T)
  linearResults[[i]]$denominator <- sum(!is.na(d$outcome))
  
  # saving what the outcome was
  results[[i]]$outcome <- OUTCOMES[i]
  
  # saving likelihood ratio test to test for neuroticism's effect
  results[[i]]$pval <- lrtest(fit0,fit1)$stats[["P"]]
  
  # saving number of cases in outcome
  results[[i]]$numerator <- sum(d$outcome,na.rm=T)
  
  # saving number of outcome (denominator)
  results[[i]]$denominator <- sum(!is.na(d$outcome))
  
  # saving location on y-axis as to where the label should be displayed
  results[[i]]$labelloc <- 0
  results[[i]][seq(1,10,2),]$labelloc <- results[[i]][seq(1,10,2),]$upper+0.2
  results[[i]][seq(2,10,2),]$labelloc <- results[[i]][seq(2,10,2),]$lower-0.2
  
  ##### RUNNING THE MODEL (ADJUSTED)
  fitx <- lrm(as.formula(sprintf("outcome ~ Neuroticism + %s",paste0(CONFOUNDERS,collapse="+"))),
              x=TRUE, y=TRUE, data=d[!is.na(outcome)])
  fit0 <- lrm(as.formula(sprintf("outcome ~ %s",paste0(CONFOUNDERS,collapse="+"))),
              x=TRUE, y=TRUE, data=d[!is.na(outcome)])
  fit1 <- lrm(as.formula(sprintf("outcome ~ rcs(Neuroticism,c(250,300,350)) + %s",paste0(CONFOUNDERS,collapse="+"))),
              x=TRUE, y=TRUE, data=d)
  p <- Predict(fit1, Neuroticism=sort(c(BASELINE,POINTS,p25,p75)), ref.zero=T)
  # saving log OR + 95% CI
  resultsAdjusted[[i]] <- data.frame(p)[,c("Neuroticism","yhat","lower","upper")]
  linearResultsAdjusted[[i]] <- as.data.frame(summary(fitx))
  linearResultsAdjusted[[i]]$var <- row.names(linearResultsAdjusted[[i]])
  linearResultsAdjusted[[i]]$aic_spline_minus_linear <- AIC(fit1) - AIC(fitx)
  linearResultsAdjusted[[i]]$outcome <- OUTCOMES[i]
  linearResultsAdjusted[[i]]$numerator <- sum(d$outcome,na.rm=T)
  linearResultsAdjusted[[i]]$denominator <- sum(!is.na(d$outcome))
  # saving what the outcome was
  resultsAdjusted[[i]]$outcome <- OUTCOMES[i]
  
  # saving likelihood ratio test to test for neuroticism's effect
  resultsAdjusted[[i]]$pval <- lrtest(fit0,fit1)$stats[["P"]]
  
  # saving number of cases in outcome
  resultsAdjusted[[i]]$numerator <- sum(d$outcome,na.rm=T)
  
  # saving number of outcome (denominator)
  resultsAdjusted[[i]]$denominator <- sum(!is.na(d$outcome))
  
  # saving location on y-axis as to where the label should be displayed
  resultsAdjusted[[i]]$labelloc <- 0
  resultsAdjusted[[i]][seq(1,10,2),]$labelloc <- resultsAdjusted[[i]][seq(1,10,2),]$upper+0.2
  resultsAdjusted[[i]][seq(2,10,2),]$labelloc <- resultsAdjusted[[i]][seq(2,10,2),]$lower-0.2
}

results <- rbindlist(results)
resultsAdjusted <- rbindlist(resultsAdjusted)
linearResults <- rbindlist(linearResults)
linearResultsAdjusted <- rbindlist(linearResultsAdjusted)

linearResults[,est:=RAWmisc::FormatEstCIFromEstSE(Effect,`S.E.`)]
linearResults[,pval:=2*(1-pnorm(abs(Effect/`S.E.`)))]
linearResults[,sig:=""]
linearResults[pval<0.05,sig:="*"]
linearResults[,pval:=sprintf("%s%s",RAWmisc::Format(pval,digits=3),sig)]
linearResults[,sig:=NULL]
linearResults[,aic_spline_minus_linear:=RAWmisc::Format(aic_spline_minus_linear)]
linearResults <- linearResults[var=="Neuroticism"]

linearResultsAdjusted[,est:=RAWmisc::FormatEstCIFromEstSE(Effect,`S.E.`)]
linearResultsAdjusted[,pval:=2*(1-pnorm(abs(Effect/`S.E.`)))]
linearResultsAdjusted[,sig:=""]
linearResultsAdjusted[pval<0.05,sig:="*"]
linearResultsAdjusted[,pval:=sprintf("%s%s",RAWmisc::Format(pval,digits=3),sig)]
linearResultsAdjusted[,sig:=NULL]
linearResultsAdjusted[,aic_spline_minus_linear:=RAWmisc::Format(aic_spline_minus_linear)]
linearResultsAdjusted <- linearResultsAdjusted[var=="Neuroticism"]

linearResults <- linearResults[,c("outcome","numerator","denominator","est","pval","aic_spline_minus_linear")]
linearResultsAdjusted <- linearResultsAdjusted[,c("outcome","est","pval","aic_spline_minus_linear")]
setnames(linearResults,c("outcome","cases","controls","cOR","cPVAL","crude_aic_spline_minus_linear"))
setnames(linearResultsAdjusted,c("outcome","aOR","aPVAL","adj_aic_spline_minus_linear"))
res <- merge(linearResults,linearResultsAdjusted,by="outcome")
res[,odds_ratios_for_63_units_of_neuroticism:=IQR(d$Neuroticism,na.rm=T)]
openxlsx::write.xlsx(res, file.path(RAWmisc::PROJ$SHARED_TODAY,"logistic_regression_linear_association.xlsx"))
res

#plotData$Neuroticism[plotData$Neuroticism!=BASELINE] <- plotData$Neuroticism[plotData$Neuroticism!=BASELINE] - 0.25

labels <- c("1/128","1/64","1/32","1/16","1/8","1/4","1/2","1","2","4","8","16","32","64","128","256","512")
breaks <- log(c(1/128,1/64,1/32,1/16,1/8,1/4,1/2,1,2,4,8,16,32,64,128,256,512))

dir.create(file.path(RAWmisc::PROJ$SHARED_TODAY,"univariable_splines"))
for(i in 1:length(OUTCOMES)){
  d$outcome <- d[[OUTCOMES[i]]]
  plotData1 <- results[outcome==OUTCOMES[i]]
  plotData2 <- resultsAdjusted[outcome==OUTCOMES[i]]
  plotData1[,type:="Crude"]
  plotData2[,type:="Adjusted"]
  plotData <- rbind(plotData1,plotData2)
  plotData[,type:=factor(type,levels=c("Crude","Adjusted"))]
  
  q <- ggplot(data=plotData, mapping=aes(x=Neuroticism))   # or plot()
  q <- q + geom_hline(yintercept=0, colour="red")
  q <- q + geom_vline(xintercept=p50, colour="red")
  q <- q + geom_vline(xintercept=p25, colour="red",lty=2)
  q <- q + geom_vline(xintercept=p75, colour="red",lty=2)
  q <- q + geom_ribbon(aes(ymin=lower,ymax=upper),alpha=0.2)
  q <- q + geom_errorbar(aes(ymin=lower,ymax=upper),lwd=1,width=0)
  q <- q + geom_point(aes(y=yhat))
  q <- q + geom_line(aes(y=yhat),lwd=1)
  q <- q + geom_label(aes(y=labelloc,label=RAWmisc::Format(exp(yhat),1)))
  q <- q + geom_rug(data=d[outcome==0],mapping=aes(y=outcome),sides="b")
  q <- q + geom_rug(data=d[outcome==1],mapping=aes(y=outcome),sides="b",colour="red")
  #q <- q + geom_rug(data=d,mapping=aes_string(y=OUTCOMES[i]),sides="b")
  q <- q + scale_x_continuous("Neuroticism",breaks=seq(0,1000,50))
  q <- q + scale_y_continuous("Odds ratio",breaks=breaks,labels=labels)
  q <- q + coord_cartesian(ylim = c(log(1/128),log(256)))
  q <- q + labs(title=OUTCOMES[i])
  q <- q + labs(caption=sprintf("\n%s cases/%s patients. Crude pvalue %s. Adjusted pvalue %s.",
                                plotData$numerator[1],
                                plotData$denominator[1],
                                RAWmisc::Format(plotData1$pval[1]),
                                RAWmisc::Format(plotData2$pval[1])))
  q <- q + facet_wrap(~type)
  q <- q + theme_gray(16)
  RAWmisc::saveA4(q, file.path(RAWmisc::PROJ$SHARED_TODAY,"univariable_splines",sprintf("odds_ratio_%s.png",OUTCOMES[i])))
  
  plotData[,yhat:=RAWmisc::Format(exp(yhat))]
  plotData[,lower:=RAWmisc::Format(exp(lower))]
  plotData[,upper:=RAWmisc::Format(exp(upper))]
  plotData[,labelloc:=NULL]
  
  openxlsx::write.xlsx(plotData, file.path(RAWmisc::PROJ$SHARED_TODAY,"univariable_splines",sprintf("values_%s.xlsx",OUTCOMES[i])))
}


#### CONFOUNDER BOXPLOTS
dir.create(file.path(RAWmisc::PROJ$SHARED_TODAY,"univariable_confounders"))

for(i in 1:length(CONFOUNDERS)){
  d$confounder <- d[[CONFOUNDERS[i]]]
  plotData <- na.omit(d[,c("confounder","Neuroticism")])
  
  pval <- kruskal.test(x=plotData$Neuroticism,g=plotData$confounder)$p.value
  
  q <- ggplot(data=plotData, mapping=aes(x=as.factor(confounder),group=confounder,y=Neuroticism))   # or plot()
  q <- q + geom_boxplot()
  q <- q + scale_x_discrete(CONFOUNDERS[i])
  q <- q + scale_y_continuous("Neuroticism")
  q <- q + labs(title=CONFOUNDERS[i])
  q <- q + labs(caption=sprintf("Kruskal-Wallis Rank Sum Test Pvalue %s",
                                RAWmisc::Format(pval)))
  q <- q + theme_gray(16)
  RAWmisc::saveA4(q, file.path(RAWmisc::PROJ$SHARED_TODAY,"univariable_confounders",sprintf("confounders_%s.png",CONFOUNDERS[i])))
}
