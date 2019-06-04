## TODO: test proportional hazards
# https://www.uio.no/studier/emner/matnat/math/STK4080/h16/undervisningsmateriell/lecture12_16.pdf

library(data.table)
library(ggplot2)

org::AllowFileManipulationFromInitialiseProject()
org::InitialiseProject(
  HOME = "/git/code_minor/2018/inger_pcos_fecundity/",
  RAW = "/data/org/data_raw/code_minor/2018/inger_pcos_fecundity/",
  SHARED = "/dropbox/analyses/results_shared/code_minor/2018/inger_pcos_fecundity/results/"
)


fs::dir_create(fs::path(org::PROJ$SHARED_TODAY,"censor_at_25","F2"))
fs::dir_create(fs::path(org::PROJ$SHARED_TODAY,"censor_at_25","F2","normal"))
fs::dir_create(fs::path(org::PROJ$SHARED_TODAY,"censor_at_25","F2","pcos_sensitivity=1"))

fs::dir_create(fs::path(org::PROJ$SHARED_TODAY,"not_censor_at_25","F2"))
fs::dir_create(fs::path(org::PROJ$SHARED_TODAY,"not_censor_at_25","F2","normal"))
fs::dir_create(fs::path(org::PROJ$SHARED_TODAY,"not_censor_at_25","F2","pcos_sensitivity=1"))

fs::dir_create(fs::path(org::PROJ$SHARED_TODAY,"not_censor_at_25","F3"))
fs::dir_create(fs::path(org::PROJ$SHARED_TODAY,"not_censor_at_25","F3","normal"))
fs::dir_create(fs::path(org::PROJ$SHARED_TODAY,"not_censor_at_25","F3","pcos_sensitivity=1"))

d <- CleanData(censor_at_25=FALSE)


d[,N:=.N,by=.(lopnr_fall)]
xtabs(~d$N)

# 11702 women who had a first pregnancy not registered 
# in the MFR or had not yet reached the age of 18 at the end of the study period
# Most likely immigrants who had their first child elsewhere
xtabs(~d$First_pregnancy_all, addNA=T)

##
sum(!is.na(d[First_pregnancy_all==0]$Time_to_first_pregnancy))
sum(is.na(d[First_pregnancy_all==0]$Time_to_first_pregnancy))
sum(is.na(d[First_pregnancy_all==1]$Time_to_first_pregnancy))

################################
################################
AnalysesFigure3Inger(d, tag="normal", censor_at_25=F)
AnalysesFigure3Inger(d[PCOS_sensitivity==1], tag="pcos_sensitivity=1", censor_at_25=F)

################################
################################
(a1 <- Sys.time())
AnalysesFigure2(d, tag="normal", isMixed=F, censor_at_25=F)
AnalysesFigure2(d[PCOS_sensitivity==1], tag="pcos_sensitivity=1", isMixed=F, censor_at_25=F)
(a2 <- Sys.time())
a2-a1

(b1 <- Sys.time())
AnalysesFigure2(d, tag="normal", isMixed=T, censor_at_25=F)
AnalysesFigure2(d[PCOS_sensitivity==1], tag="pcos_sensitivity=1", isMixed=T, censor_at_25=F)
(b2 <- Sys.time())
b2-b1




################################
################################
################################
################################

d <- CleanData(censor_at_25=TRUE)

(a1 <- Sys.time())
AnalysesFigure2(d, tag="normal", isMixed=F, censor_at_25=T)
AnalysesFigure2(d[PCOS_sensitivity==1], tag="pcos_sensitivity=1", isMixed=F, censor_at_25=T)
(a2 <- Sys.time())
a2-a1

(b1 <- Sys.time())
AnalysesFigure2(d, tag="normal", isMixed=T, censor_at_25=T)
AnalysesFigure2(d[PCOS_sensitivity==1], tag="pcos_sensitivity=1", isMixed=T, censor_at_25=T)
(b2 <- Sys.time())
b2-b1