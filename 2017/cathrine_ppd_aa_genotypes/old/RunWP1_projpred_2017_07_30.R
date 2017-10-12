# If packages aren't installed -- install them
if(!require(devtools)) install.packages("devtools")
if(!require(pbkrtest)) install.packages("pbkrtest")
if(!require(car)) install.packages("car")
if(!require(MASS)) install.packages("MASS")
if(!require(parallel)) install.packages("parallel")
if(!require(data.table)) install.packages("data.table")
if(!require(rstanarm)) install.packages("rstanarm")
if(!require(data.table)) install.packages("data.table")
if(!require(projpred)) devtools::install_github('stan-dev/projpred')
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(bayesplot)) install.packages("bayesplot")
if(!require(haven)) install.packages("haven")
if(!require(RAWmisc)) devtools::install_github('raubreywhite/RAWmisc')

# Load important packages
suppressWarnings(suppressMessages(library(data.table)))
suppressWarnings(suppressMessages(library(rstanarm)))
suppressWarnings(suppressMessages(library(projpred)))
suppressWarnings(suppressMessages(library(ggplot2)))
suppressWarnings(suppressMessages(library(bayesplot)))
options(mc.cores = parallel::detectCores())

SCALE <- TRUE

############################################################
##### SETTING UP VARIABLES THAT WILL BE USED LATER ON IN CLEANING/ANALYSIS

# These are all the SNPs we are interested in
SNPs <- c(
  "rs2740210_CA",
  "rs4813625_CA",
  "rs4813627_CA",
  "rs7632287_CA",
  "rs1042778_CA",
  "rs2254298_CA",
  "rs2268490_CA",
  "rs2268491_CA",
  "rs237885_CA",
  "rs235887_CA",
  "rs237902_CA",
  "rs4686302_CA",
  "rs53576_CA"
)

# These are the SNPs that need to be recategorized to 0 vs 1
RECATEGORIZED_SNPs <- c(
  "rs2254298_CA",
  "rs2268490_CA", 
  "rs2268491_CA",
  "rs4686302_CA"
)

# These are teh SNPs that do not need to be recategorized
UNRECATEGORIZED_SNPs <- SNPs[!SNPs %in% RECATEGORIZED_SNPs]

# These are covariates
COVAR <- c(
  "v32_Alder_CA",
  "v17_fodelseort_R",
  "v17_utbildning_R",
  "v17_arbetardu_R"
)


############################################################
##### CLEANING THE DATA FOR THE NORMAL ANALYSES (i.e. ANALYSES 1 AND 2)
## READ IN DATA
## CATHRINE YOU WILL NEED TO CHANGE THIS
d <- haven::read_spss("/analyses/data_raw/cathrine_ppd_aa_genotypes/Pek3 PPD ASQ OXT LITE 1631.sav")
#d <- haven::read_spss("~/BASICkrypt/Pek3 PPD ASQ OXT LITE 1631.sav")

## APPLY FACTOR LABELS TO SNPs THAT DONT NEED TO BE RECATEGORIZED
for(s in UNRECATEGORIZED_SNPs){
  d[s] <- haven::as_factor(d[s])
  
  print("*****")
  print(s)
  print(levels(d[[s]]))
  l <- levels(d[[s]])
  if("NaN" %in% levels(d[[s]])){
    l <- l[-4]
    d[[s]] <- car::Recode(d[[s]],"'NaN'=NA")
    d[[s]] <- factor(d[[s]],levels=l)
  }
  print(levels(d[[s]]))
}

## RECATEGORIZE SNPs THAT NEED TO BE
for(s in RECATEGORIZED_SNPs){
  print("***")
  print(s)
  l <- attributes(haven::print_labels(d[[s]]))$labels
  print(xtabs(~d[[s]]))
  baseline <- as.numeric(names(sort(table(d[[s]]),decreasing=TRUE))[1])
  baselineLabel <- names(l[baseline+1])
  other <- as.numeric(names(sort(table(d[[s]]),decreasing=TRUE))[2:3])
  otherLabel <- paste0(names(l[-(baseline+1)]),collapse="_")
  print("before")
  print(xtabs(~d[[s]]))
  print("after")
  d[[s]] <- car::Recode(d[[s]],"baseline=0;other=1", as.factor.result=FALSE)
  print(xtabs(~d[[s]]))
  d[[s]] <- factor(d[[s]],levels=c(0,1))
  levels(d[[s]]) <- c(baselineLabel,otherLabel)
  print("after factor")
  print(xtabs(~d[[s]]))
}


## APPLY FILTERS TO DATASET TO REMOVE PEOPLE
d <- data.table(d)
d <- d[filter_pek3_170705==1]
nrow(d)
d <- d[Any_genotype==1]
nrow(d)

prop.table(table(d$rs2740210_CA))
table(d$rs2740210_CA)
prop.table(table(d$rs4813625_CA))
prop.table(table(d$rs4813627_CA))
prop.table(table(d$rs7632287_CA)) #2 only 0.07
prop.table(table(d$rs1042778_CA))
prop.table(table(d$rs2254298_CA)) #0 only 0.01
prop.table(table(d$rs2268490_CA)) #2 only 0.02
prop.table(table(d$rs2268491_CA)) #0 only 0.01
prop.table(table(d$rs237885_CA))
prop.table(table(d$rs235887_CA))
prop.table(table(d$rs237902_CA))
prop.table(table(d$rs4686302_CA)) #0 only 0.022
prop.table(table(d$rs53576_CA))

########### RIGHT NOW THE DATASET IS READY FOR ANALYSES 1 AND 2

###### CREATING CORRELATION MATRIX
wideData <- vector("list", length=length(SNPs))
for(i in 1:length(SNPs)){
  temp <- dcast.data.table(
    data=d[,c("CustomDataR", SNPs[i]),with=F],
    as.formula(sprintf("CustomDataR~%s",SNPs[i])), value.var=SNPs[i])
  temp[,`NA`:=NULL]
  temp <- data.frame(temp)
  peopleMissingSNP <- apply(temp[,-1],1,function(x){sum(!is.na(x))})==0
  for(j in 2:ncol(temp)){
    temp[,j] <- as.numeric(temp[,j])
    temp[,j][!is.na(temp[,j])] <- 1
    temp[,j][is.na(temp[,j])] <- 0
    if(sum(peopleMissingSNP)>0) temp[peopleMissingSNP,j] <- NA
  }
  names(temp)[2:ncol(temp)] <- paste0(SNPs[i],"___",names(temp)[2:ncol(temp)])
  wideData[[i]] <- temp[2:ncol(temp)]
}
wideData <- do.call(cbind,wideData)
correlationMatrix <- cor(wideData,use="pairwise.complete.obs")
openxlsx::write.xlsx(correlationMatrix, file="/dropbox/clients/cathrine/ppd_aa_genotypes/results_richard/correlationMatrix.xlsx", rowNames=TRUE, colNames=TRUE)


############################################################
##### RUNNING ANALYSES 1 AND 2
## ANALYSIS 1 (Crude negative-binomial regression, with one exposure per regression)

# we start off by listing all of the SNP names
print(SNPs)

# we now fit a crude negative-binomial regression
# we need to round the outcome to whole integers, as 
# negative-binomial regressions can only deal with
# whole integers
fit <- MASS::glm.nb(round(ppv6_EPDS_9R) ~ rs2740210_CA, data=d)
# we then look at the results of the regression
# we can see the LOG-incident rate ratios of the dummy variables
# comparing CA to AA (baseline), and CC to AA (baseline)
# 
# To get IRRs (incident rate ratios) you take exp(estimate)
# So for rs2740210_CACA (CA vs baseline of AA), IRR = exp(0.07219) = 1.07486
# To get 95% confidence intervals, you take:
# L95 = exp(estimate - 1.96* std.error) = exp(0.07219 - 1.96*0.08849) = 0.9037067
# U95 = exp(estimate + 1.96* std.error) = exp(0.07219 + 1.96*0.08849) = 1.278427
# P-value is listed as Pr(>|z|) = 0.351
summary(fit)
# However, it is important to note that these are just pairwise comparisons.
# Before you look at pairwise comparisons, it should be tested that the
# entire variable is significant. As an example, if your exposure is
# 3 categories of BMI (underweight, normal, overweight), then you should first
# test if "BMI" is significant, before you start looking at underweight vs normal,
# and normal vs overweight. This is because the pairwise analyses are conditional
# upon your baseline choice (e.g. underweight vs overweight is probably significant, 
# while underweight vs normal might not be). Therefore it makes sense to test:
# "does the outcome vary significantly according to different categories" and then
# if the answer is "yes", *then* start looking at the pairwise comparisons
#
# We do this using a "joint significance test" (an F-test), which determines if
# both of the estimates for CA and CC are simultaneously equal to zero
# It is done as follows, and the p-value for this is 0.04688.
# 
# This is interesting, because this says "the outcome differs according to rs2740210_CA",
# however, your previous analyses did not show any significant results. That is because
# CA is slightly more harmful than AA (not sig), CC is slightly more protective than AA (not sig),
# but CA is probably significantly more harmful than CC.
#
# This p-value given from the F-test is the one that should be corrected for
# multiple testing using the Bonferroni correction
car::lht(fit,c("rs2740210_CACA=0","rs2740210_CACC=0"))

# Out of curiosity, we can now test if CA = CC
# We see that we get a p-value of 0.01365 (significant)
# Showing that CA != CC
car::lht(fit,c("rs2740210_CACA=rs2740210_CACC"))

## We now start again with a new SNP
fit <- MASS::glm.nb(round(ppv6_EPDS_9R) ~ rs4813625_CA, data=d)
summary(fit)
car::lht(fit,c("rs4813625_CACG=0","rs4813625_CACC=0"))

fit <- MASS::glm.nb(round(ppv6_EPDS_9R) ~ rs4813627_CA, data=d)
summary(fit)
car::lht(fit,c("rs4813627_CAGA=0","rs4813627_CAGG=0"))

fit <- MASS::glm.nb(round(ppv6_EPDS_9R) ~ rs7632287_CA, data=d)
summary(fit)
car::lht(fit,c("rs7632287_CAAG=0","rs7632287_CAAA=0"))

fit <- MASS::glm.nb(round(ppv6_EPDS_9R) ~ rs1042778_CA, data=d)
summary(fit)
car::lht(fit,c("rs1042778_CAGT=0","rs1042778_CATT=0"))


fit <- MASS::glm.nb(round(ppv6_EPDS_9R) ~ rs2254298_CA, data=d)
summary(fit)

fit <- MASS::glm.nb(round(ppv6_EPDS_9R) ~ rs2268490_CA, data=d)
summary(fit)

fit <- MASS::glm.nb(round(ppv6_EPDS_9R) ~ rs2268491_CA, data=d)
summary(fit)

fit <- MASS::glm.nb(round(ppv6_EPDS_9R) ~ rs237885_CA, data=d)
summary(fit)
car::lht(fit,c("rs237885_CAGT=0","rs237885_CAGG=0"))

fit <- MASS::glm.nb(round(ppv6_EPDS_9R) ~ rs235887_CA, data=d)
summary(fit)
car::lht(fit,c("rs235887_CAAG=0","rs235887_CAAA=0"))

fit <- MASS::glm.nb(round(ppv6_EPDS_9R) ~ rs237902_CA, data=d)
summary(fit)
car::lht(fit,c("rs237902_CAAG=0","rs237902_CAAA=0"))

fit <- MASS::glm.nb(round(ppv6_EPDS_9R) ~ rs4686302_CA, data=d)
summary(fit)

fit <- MASS::glm.nb(round(ppv6_EPDS_9R) ~ rs53576_CA, data=d)
summary(fit)
car::lht(fit,c("rs53576_CAGA=0","rs53576_CAGG=0"))



# CA: SAME FOR OUTCOME PPM6

fit <- MASS::glm.nb(round(ppm6_EPDS_9R) ~ rs2740210_CA, data=d)
summary(fit)
car::lht(fit,c("rs2740210_CACA=0","rs2740210_CACC=0"))
car::lht(fit,c("rs2740210_CACA=rs2740210_CACC"))

fit <- MASS::glm.nb(round(ppm6_EPDS_9R) ~ rs4813625_CA, data=d)
summary(fit)
car::lht(fit,c("rs4813625_CACG=0","rs4813625_CACC=0"))

fit <- MASS::glm.nb(round(ppm6_EPDS_9R) ~ rs4813627_CA, data=d)
summary(fit)
car::lht(fit,c("rs4813627_CAGA=0","rs4813627_CAGG=0"))

fit <- MASS::glm.nb(round(ppm6_EPDS_9R) ~ rs7632287_CA, data=d)
summary(fit)
car::lht(fit,c("rs7632287_CAAG=0","rs7632287_CAAA=0"))

fit <- MASS::glm.nb(round(ppm6_EPDS_9R) ~ rs1042778_CA, data=d)
summary(fit)
car::lht(fit,c("rs1042778_CAGT=0","rs1042778_CATT=0"))


fit <- MASS::glm.nb(round(ppm6_EPDS_9R) ~ rs2254298_CA, data=d)
summary(fit)

fit <- MASS::glm.nb(round(ppm6_EPDS_9R) ~ rs2268490_CA, data=d)
summary(fit)

fit <- MASS::glm.nb(round(ppm6_EPDS_9R) ~ rs2268491_CA, data=d)
summary(fit)

fit <- MASS::glm.nb(round(ppm6_EPDS_9R) ~ rs237885_CA, data=d)
summary(fit)
car::lht(fit,c("rs237885_CAGT=0","rs237885_CAGG=0"))

fit <- MASS::glm.nb(round(ppm6_EPDS_9R) ~ rs235887_CA, data=d)
summary(fit)
car::lht(fit,c("rs235887_CAAG=0","rs235887_CAAA=0"))

fit <- MASS::glm.nb(round(ppm6_EPDS_9R) ~ rs237902_CA, data=d)
summary(fit)
car::lht(fit,c("rs237902_CAAG=0","rs237902_CAAA=0"))

fit <- MASS::glm.nb(round(ppm6_EPDS_9R) ~ rs4686302_CA, data=d)
summary(fit)

fit <- MASS::glm.nb(round(ppm6_EPDS_9R) ~ rs53576_CA, data=d)
summary(fit)
car::lht(fit,c("rs53576_CAGA=0","rs53576_CAGG=0"))




## ANALYSIS 2 (Adjusted negative-binomial regression, with one exposure per regression)
print(SNPs)

fit <- MASS::glm.nb(round(ppv6_EPDS_9R) ~ rs2740210_CA + 
                      v32_Alder_CA +
                      v17_fodelseort_R +
                      v17_utbildning_R +
                      v17_arbetardu_R, data=d)
summary(fit)
car::lht(fit,c("rs2740210_CACA=0","rs2740210_CACC=0"))
car::lht(fit,c("rs2740210_CACA=rs2740210_CACC"))

fit <- MASS::glm.nb(round(ppv6_EPDS_9R) ~ rs4813625_CA + 
                      v32_Alder_CA +
                      v17_fodelseort_R +
                      v17_utbildning_R +
                      v17_arbetardu_R, data=d)
summary(fit)
car::lht(fit,c("rs4813625_CACG=0","rs4813625_CACC=0"))

fit <- MASS::glm.nb(round(ppv6_EPDS_9R) ~ rs4813627_CA + 
                      v32_Alder_CA +
                      v17_fodelseort_R +
                      v17_utbildning_R +
                      v17_arbetardu_R, data=d)
summary(fit)
car::lht(fit,c("rs4813627_CAGA=0","rs4813627_CAGG=0"))


fit <- MASS::glm.nb(round(ppv6_EPDS_9R) ~ rs7632287_CA + 
                      v32_Alder_CA +
                      v17_fodelseort_R +
                      v17_utbildning_R +
                      v17_arbetardu_R, data=d)
summary(fit)
car::lht(fit,c("rs7632287_CAAG=0","rs7632287_CAAA=0"))


fit <- MASS::glm.nb(round(ppv6_EPDS_9R) ~ rs1042778_CA + 
                      v32_Alder_CA +
                      v17_fodelseort_R +
                      v17_utbildning_R +
                      v17_arbetardu_R, data=d)
summary(fit)
car::lht(fit,c("rs1042778_CAGT=0","rs1042778_CATT=0"))


fit <- MASS::glm.nb(round(ppv6_EPDS_9R) ~ rs2254298_CA + 
                      v32_Alder_CA +
                      v17_fodelseort_R +
                      v17_utbildning_R +
                      v17_arbetardu_R, data=d)
summary(fit)

fit <- MASS::glm.nb(round(ppv6_EPDS_9R) ~ rs2268490_CA + 
                      v32_Alder_CA +
                      v17_fodelseort_R +
                      v17_utbildning_R +
                      v17_arbetardu_R, data=d)
summary(fit)

fit <- MASS::glm.nb(round(ppv6_EPDS_9R) ~ rs2268491_CA + 
                      v32_Alder_CA +
                      v17_fodelseort_R +
                      v17_utbildning_R +
                      v17_arbetardu_R, data=d)
summary(fit)

fit <- MASS::glm.nb(round(ppv6_EPDS_9R) ~ rs237885_CA + 
                      v32_Alder_CA +
                      v17_fodelseort_R +
                      v17_utbildning_R +
                      v17_arbetardu_R, data=d)
summary(fit)
car::lht(fit,c("rs237885_CAGT=0","rs237885_CAGG=0"))


fit <- MASS::glm.nb(round(ppv6_EPDS_9R) ~ rs235887_CA + 
                      v32_Alder_CA +
                      v17_fodelseort_R +
                      v17_utbildning_R +
                      v17_arbetardu_R, data=d)
summary(fit)
car::lht(fit,c("rs235887_CAAG=0","rs235887_CAAA=0"))


fit <- MASS::glm.nb(round(ppv6_EPDS_9R) ~ rs237902_CA + 
                      v32_Alder_CA +
                      v17_fodelseort_R +
                      v17_utbildning_R +
                      v17_arbetardu_R, data=d)
summary(fit)
car::lht(fit,c("rs237902_CAAG=0","rs237902_CAAA=0"))



fit <- MASS::glm.nb(round(ppv6_EPDS_9R) ~ rs4686302_CA + 
                      v32_Alder_CA +
                      v17_fodelseort_R +
                      v17_utbildning_R +
                      v17_arbetardu_R, data=d)
summary(fit)

fit <- MASS::glm.nb(round(ppv6_EPDS_9R) ~ rs53576_CA + 
                      v32_Alder_CA +
                      v17_fodelseort_R +
                      v17_utbildning_R +
                      v17_arbetardu_R, data=d)
summary(fit)
car::lht(fit,c("rs53576_CAGA=0","rs53576_CAGG=0"))

#CA: ANALYSIS 2, PPM6

print(SNPs)

fit <- MASS::glm.nb(round(ppm6_EPDS_9R) ~ rs2740210_CA + 
                      v32_Alder_CA +
                      v17_fodelseort_R +
                      v17_utbildning_R +
                      v17_arbetardu_R, data=d)
summary(fit)
car::lht(fit,c("rs2740210_CACA=0","rs2740210_CACC=0"))
car::lht(fit,c("rs2740210_CACA=rs2740210_CACC"))

fit <- MASS::glm.nb(round(ppm6_EPDS_9R) ~ rs4813625_CA + 
                      v32_Alder_CA +
                      v17_fodelseort_R +
                      v17_utbildning_R +
                      v17_arbetardu_R, data=d)
summary(fit)
car::lht(fit,c("rs4813625_CACG=0","rs4813625_CACC=0"))

fit <- MASS::glm.nb(round(ppm6_EPDS_9R) ~ rs4813627_CA + 
                      v32_Alder_CA +
                      v17_fodelseort_R +
                      v17_utbildning_R +
                      v17_arbetardu_R, data=d)
summary(fit)
car::lht(fit,c("rs4813627_CAGA=0","rs4813627_CAGG=0"))


fit <- MASS::glm.nb(round(ppm6_EPDS_9R) ~ rs7632287_CA + 
                      v32_Alder_CA +
                      v17_fodelseort_R +
                      v17_utbildning_R +
                      v17_arbetardu_R, data=d)
summary(fit)
car::lht(fit,c("rs7632287_CAAG=0","rs7632287_CAAA=0"))


fit <- MASS::glm.nb(round(ppm6_EPDS_9R) ~ rs1042778_CA + 
                      v32_Alder_CA +
                      v17_fodelseort_R +
                      v17_utbildning_R +
                      v17_arbetardu_R, data=d)
summary(fit)
car::lht(fit,c("rs1042778_CAGT=0","rs1042778_CATT=0"))


fit <- MASS::glm.nb(round(ppm6_EPDS_9R) ~ rs2254298_CA + 
                      v32_Alder_CA +
                      v17_fodelseort_R +
                      v17_utbildning_R +
                      v17_arbetardu_R, data=d)
summary(fit)

fit <- MASS::glm.nb(round(ppm6_EPDS_9R) ~ rs2268490_CA + 
                      v32_Alder_CA +
                      v17_fodelseort_R +
                      v17_utbildning_R +
                      v17_arbetardu_R, data=d)
summary(fit)

fit <- MASS::glm.nb(round(ppm6_EPDS_9R) ~ rs2268491_CA + 
                      v32_Alder_CA +
                      v17_fodelseort_R +
                      v17_utbildning_R +
                      v17_arbetardu_R, data=d)
summary(fit)

fit <- MASS::glm.nb(round(ppm6_EPDS_9R) ~ rs237885_CA + 
                      v32_Alder_CA +
                      v17_fodelseort_R +
                      v17_utbildning_R +
                      v17_arbetardu_R, data=d)
summary(fit)
car::lht(fit,c("rs237885_CAGT=0","rs237885_CAGG=0"))


fit <- MASS::glm.nb(round(ppm6_EPDS_9R) ~ rs235887_CA + 
                      v32_Alder_CA +
                      v17_fodelseort_R +
                      v17_utbildning_R +
                      v17_arbetardu_R, data=d)
summary(fit)
car::lht(fit,c("rs235887_CAAG=0","rs235887_CAAA=0"))


fit <- MASS::glm.nb(round(ppm6_EPDS_9R) ~ rs237902_CA + 
                      v32_Alder_CA +
                      v17_fodelseort_R +
                      v17_utbildning_R +
                      v17_arbetardu_R, data=d)
summary(fit)
car::lht(fit,c("rs237902_CAAG=0","rs237902_CAAA=0"))


fit <- MASS::glm.nb(round(ppm6_EPDS_9R) ~ rs4686302_CA + 
                      v32_Alder_CA +
                      v17_fodelseort_R +
                      v17_utbildning_R +
                      v17_arbetardu_R, data=d)
summary(fit)

fit <- MASS::glm.nb(round(ppm6_EPDS_9R) ~ rs53576_CA + 
                      v32_Alder_CA +
                      v17_fodelseort_R +
                      v17_utbildning_R +
                      v17_arbetardu_R, data=d)
summary(fit)
car::lht(fit,c("rs53576_CAGA=0","rs53576_CAGG=0"))

# and so on ...

################################################################
################################################################
################################################################
##################### 2017-07-24 END HERE ######################
################################################################
################################################################
################################################################

## LOOK AT HOW MUCH WE ARE MISSING OF COVARIATES
melt.data.table(d[,COVAR,with=F],measure.vars=COVAR)[,.(
  invalid=sum(value=="NaN"),
  valid=sum(value!="NaN")
),by=variable]

## LOOK AT HOW MUCH WE ARE MISSING OF SNPs
x <- d[,SNPs,with=F]
melt.data.table(x,measure.vars=names(x))[,.(
  invalid=sum(value=="NaN"),
  valid=sum(value!="NaN")
  ),by=variable]

## CREATE DUMMY VARIABLES FOR SNPs
for(i in 1:ncol(x)){
  #x[[i]][x[[i]]=="NaN"] <- names(sort(table(x[[i]]),decreasing=TRUE))[1] # replace missing with most popular
  x[[i]] <- factor(x[[i]], levels=levels(droplevels(x[[i]]))) # remove empty levels
  x[[i]] <- factor(x[[i]], levels=names(sort(table(x[[i]]),decreasing=TRUE))) # make largest first
}
x <- model.matrix(  ~ . -1, data=x )[,-1]

## REJOIN THE COVARIATES WITH THE SNPs
z <- d[,COVAR,with=F]
x <- cbind(z,x)
if(SCALE) x <- scale(x)

## REJOIN THE COVARIATES + SNPs WITH THE OUTCOME
data <- data.frame(y=round(d$ppv6_EPDS_9R), x=x)
dim(data)
data <- na.omit(data)
dim(data)

## RUN BAYESIAN LASSO
n <- nrow(x) # 100
D <- ncol(x) # 20
p0 <- 2 # prior guess for the number of relevant variables
tau0 <- p0/(D-p0) * 1/sqrt(n) # scale for tau (notice that stan_glm will automatically scale this by sigma)
prior_coeff <- hs(df=1, global_df=1, global_scale=tau0) # horseshoe prior
fit <- stan_glm(y ~ ., family=poisson(), data=data, prior=prior_coeff,
                seed=1, adapt_delta=0.999, chains=4, iter=1000) 

fit_cv <- cv_varsel(fit, method='L1', cv_method='LOO')

# HOW MANY VARIABLES SHOULD WE KEEP?
fit_cv$varsel$ssize
varsel_plot(fit_cv, statistics = c('mlpd', 'mse'), deltas=T)

# GETTING OUT EFFECT ESTIMATES
proj <- project(fit_cv, nv = fit_cv$varsel$ssize, ns = 1000, intercept=T)
coefs <- as.matrix(proj)
scaleFactors <- attr(x,"scaled:scale")
coefsRescaled <- coefs
for(i in 2:ncol(coefs)){
  sf <- scaleFactors[paste0("x.",names(scaleFactors))==colnames(coefs)[i]]
  if(SCALE) coefsRescaled[,i] <- coefsRescaled[,i]/sf
}

# 2.5, 50, 97.5 PERCENTAILES (i.e. 95% BAYESICAN CREDIBILITY INTERVALS AND EFFECT ESTIMATE)
apply(coefsRescaled,2,function(x) quantile(x, probs=c(0.025,0.5,0.975)))

# FREQUENTIST SIMPLE REGRESSION
f <- glm(y~., data=data, family=quasipoisson())
summary(f)
if(SCALE) print(coef(f)/c(1,scaleFactors))

f <- glm(y~x.v17_dep_anamnes, data=data, family=quasipoisson())
summary(f)





