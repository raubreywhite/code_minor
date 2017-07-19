RAWmisc::AllowFileManipulationFromInitialiseProject()

RAWmisc::InitialiseProject(
  HOME = "/git/code_minor/cathrine_ppd_aa_genotypes/",
  RAW = "/analyses/data_raw/cathrine_ppd_aa_genotypes/",
  CLEAN = "/analyses/data_clean/cathrine_ppd_aa_genotypes",
  BAKED = "/analyses/results_baked/cathrine_ppd_aa_genotypes/",
  FINAL = "/analyses/results_final/cathrine_ppd_aa_genotypes/",
  SHARED = "/dropbox/results_shared/cathrine_ppd_aa_genotypes/")

suppressWarnings(suppressMessages(library(data.table)))
suppressWarnings(suppressMessages(library(rstanarm)))
suppressWarnings(suppressMessages(library(projpred)))
suppressWarnings(suppressMessages(library(ggplot2)))
suppressWarnings(suppressMessages(library(bayesplot)))
options(mc.cores = parallel::detectCores())

SCALE <- TRUE

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
  "rs237902_CA",
  "rs4686302_CA",
  "rs53576_CA"
)

COVAR <- c(
  "ASQSF_total_anxiety",
  "ASQSF_total_avoidance",
  "v17_dep_anamnes",
  "v32_Alder_CA",
  "Nulliparous_CA",
  "v17_fodelseort_R",
  "v17_utbildning_R",
  "v17_arbetardu_R",
  "v17_vald_R",
  "v17_planeradgrav",
  "v17_somn_6_R"
)

## READ IN DATA
d <- haven::read_spss(file.path(RAWmisc::PROJ$RAW,"Pek3 PPD ASQ OXT LITE 1631.sav"))

## APPLY FACTOR LABELS TO SNPs
for(s in SNPs){
  d[s] <- haven::as_factor(d[s])
}

## APPLY FILTERS TO DATASET TO REMOVE PEOPLE
d <- data.table(d)
d <- d[filter_pek3_170705==1]
nrow(d)
d <- d[Any_genotype==1]
nrow(d)

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

x.v17_dep_anamnes  x.rs2740210_CACA 
0.3886320         0.1036533 







