RAWmisc::AllowFileManipulationFromInitialiseProject()

RAWmisc::InitialiseProject(
  HOME = "/git/code_minor/cathrine_ppd_aa_genotypes/",
  RAW = "/analyses/data_raw/cathrine_ppd_aa_genotypes/",
  CLEAN = "/analyses/data_clean/cathrine_ppd_aa_genotypes",
  BAKED = "/analyses/results_baked/cathrine_ppd_aa_genotypes/",
  FINAL = "/analyses/results_final/cathrine_ppd_aa_genotypes/",
  SHARED = "/dropbox/results_shared/cathrine_ppd_aa_genotypes/")

suppressWarnings(suppressMessages(library(data.table)))

d <- data.table(haven::read_spss(file.path(RAWmisc::PROJ$RAW,"Pek3 PPD ASQ OXT LITE 1631.sav")))
d <- d[filter_pek3_170705==1]
nrow(d)
d <- d[Any_genotype==1]
nrow(d)

hist(d$ASQSF_total_anxiety)
hist(d$ASQSF_total_avoidance)
hist(d$ppv6_EPDS_9R)
hist(d$ppm6_EPDS_9R)

summary(lm(ASQSF_total_anxiety~
             factor(rs53576_CA), data=d))

summary(lm(log(ASQSF_total_anxiety)~
             factor(rs53576_CA), data=d))

summary(glm(ASQSF_total_anxiety~
             factor(rs53576_CA), data=d, family=quasipoisson()))

fit <- lm(ASQSF_total_anxiety~
            factor(rs2740210_CA)+
            factor(rs4813625_CA)+
            factor(rs4813627_CA)+
            factor(rs7632287_CA)+
            factor(rs1042778_CA)+
            factor(rs2254298_CA)+
            factor(rs2268490_CA)+
            factor(rs2268491_CA)+
            factor(rs237885_CA)+
            factor(rs237902_CA)+
            factor(rs4686302_CA)+
            factor(rs53576_CA), data=d)
summary(fit)

#library(rstanarm)
post <- rstanarm::stan_lm(ASQSF_total_anxiety~
                            factor(rs53576_CA), data=d,
                          prior = rstanarm::R2(location = 0.2), 
                          chains = 4, cores = 4, seed = 4)
post


fit0 <- rstanarm::stan_glm(round(ppv6_EPDS_9R)~
                             factor(rs53576_CA), data=d,
                           family=poisson,
                           prior = rstanarm::normal(0,2.5), prior_intercept = rstanarm::normal(0,5),
                           chains = 2, cores = 4, seed = 4)

fit1 <- rstanarm::stan_glm(round(ppv6_EPDS_9R)~
                             factor(rs53576_CA), data=d,
                           family=poisson,
                           prior = rstanarm::hs(), prior_intercept = rstanarm::normal(0,5),
                           chains = 2, cores = 4, seed = 4)

summary(fit0)
summary(fit1)
