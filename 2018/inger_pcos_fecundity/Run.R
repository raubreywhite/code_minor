## TODO: test proportional hazards
# https://www.uio.no/studier/emner/matnat/math/STK4080/h16/undervisningsmateriell/lecture12_16.pdf

org::AllowFileManipulationFromInitialiseProject()
org::InitialiseProject(
  HOME = "/git/code_minor/2018/inger_pcos_fecundity/",
  RAW = "/Volumes/crypt_data/org/data_raw/code_minor/2018/inger_pcos_fecundity/",
  SHARED = "/dropbox/analyses/results_shared/code_minor/2018/inger_pcos_fecundity/results/"
)

library(data.table)
library(ggplot2)

dir.create(file.path(org::PROJ$SHARED_TODAY,"F2"))
dir.create(file.path(org::PROJ$SHARED_TODAY,"F2","normal"))
dir.create(file.path(org::PROJ$SHARED_TODAY,"F2","sensitivity"))
dir.create(file.path(org::PROJ$SHARED_TODAY,"F3"))
dir.create(file.path(org::PROJ$SHARED_TODAY,"F3","normal"))
dir.create(file.path(org::PROJ$SHARED_TODAY,"F3","sensitivity"))

d <- CleanData()


d[,N:=.N,by=.(lopnr_fall)]
xtabs(~d$N)


# 11702 women who had a first pregnancy not registered 
# in the MFR or had not yet reached the age of 18 at the end of the study period
# Most likely immigrants who had their first child elsewhere
xtabs(~d$First_pregnancy_all, addNA=T)

# figure 2````
sum(!is.na(d[First_pregnancy_all==0]$Time_to_first_pregnancy))
sum(is.na(d[First_pregnancy_all==0]$Time_to_first_pregnancy))
sum(is.na(d[First_pregnancy_all==1]$Time_to_first_pregnancy))

AnalysesFigure3Inger(d, tag="normal")
AnalysesFigure3Inger(d[PCOS_sensitivity==1], tag="sensitivity")

(a1 <- Sys.time())
AnalysesFigure2(d, tag="normal", isMixed=F)
AnalysesFigure2(d[PCOS_sensitivity==1], tag="sensitivity", isMixed=F)
(a2 <- Sys.time())
a2-a1

(b1 <- Sys.time())
AnalysesFigure2(d, tag="normal", isMixed=T)
AnalysesFigure2(d[PCOS_sensitivity==1], tag="sensitivity", isMixed=T)
(b2 <- Sys.time())
b2-b1




