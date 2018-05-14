RAWmisc::InitialiseOpinionatedUnix(project="code_minor/2017/cathrine_neuroticism_health_care_consumption/")

# *Neuroticism health care consumption*
#   
# **FILTER**
# Include_new_CA
# *n=1208
# 
# 
# **EXPOSURE**
#   Neuroticism_CA
# 
# 
# **OUTCOMES**
#   *BINARY*
#   Base_prog_CA
# Hospital_admission_CA
# Prenatal_diagnostics_CA
# Sick_leave_CA
# Aurora_CA
# Annan_behandling
# 
# *SCALE*
#   Obgyn_physician_count_CA
# Midwife_ObGyn_phone_visits_CA
# Maternal_care_midwife_CA
# Ultrasounds_count_CA
# 
# 
# **CONFOUNDERS**
#   
#   *MAIN CONFOUNDER AND/OR EFFECT MODIFIER*
#   High_risk_somatic_disease_CA
# 
# *OTHER CONFOUNDERS*
#   Age_3_CA
# Parity_D_CA
# Country_CA
# Education_CA
# Working_status_CA
# Single_CA
# BMI_3_CA
# Psychiatric_disorder_medication_CA
# 


library(data.table)
library(ggplot2)
library(rms)

d <- data.table(haven::read_sav(file.path(
  RAWmisc::PROJ$RAW,"Neuroticism_health_consumption_180123.sav"
)))
setnames(d,"Graviditetsår","Graviditetsar")

dir.create(file.path(RAWmisc::PROJ$SHARED_TODAY,"tables"))
dir.create(file.path(RAWmisc::PROJ$SHARED_TODAY,"figures"))

nrow(d)
d <- d[Include_new_CA==1]
nrow(d)
d <- d[High_risk_somatic_disease_CA==0]
nrow(d)

d[,Base_prog_CA:=abs(Base_prog_CA-1)]



OUTCOMES_BINARY <- c(
  "Base_prog_CA",
  "Hospital_admission_CA",
  "Prenatal_diagnostics_CA",
  "Sick_leave_CA",
  "Aurora_CA",
  "Annan_behandling",
  "Obstetrician_fetal_movements_D",
  "Obstetrician_contractions_D"
)

OUTCOMES_CONTINUOUS <- c(
  "Ultrasounds_count_CA"
)

EXPOSURE <- "Neuroticism_CA"

EFFECT_MODIFIER <- "High_risk_somatic_disease_CA"

CONFOUNDERS_BINARY <- c(
  "Parity_D_CA",
  "Country_CA",
  "Education_CA",
  "Working_status_CA",
  "Single_CA",
  "Psychiatric_disorder_medication_CA"
)

CONFOUNDERS_CATEGORY <- c(
  "Age_3_CA",
  "BMI_3_CA"
)



for(i in c(
  CONFOUNDERS_CATEGORY
)){
  d[[i]] <- haven::as_factor(d[[i]])
}

p25 <- quantile(d[[EXPOSURE]],prob=0.25,na.rm=T)
d[["IQR_Neuroticism_CA"]] <- (d[["Neuroticism_CA"]]-p25)/IQR(d[["Neuroticism_CA"]])

stack_bin <- RAWmisc::CreateStackSkeleton(n=length(OUTCOMES_BINARY))
stack_bin$regressionType <- "logistic"
stack_bin$outcome <- OUTCOMES_BINARY
stack_bin$exposure <- "IQR_Neuroticism_CA"
stack_bin$confounders <- list(c(CONFOUNDERS_BINARY,CONFOUNDERS_CATEGORY))
stack_bin$data <- "d"

stack_con <- RAWmisc::CreateStackSkeleton(n=length(OUTCOMES_CONTINUOUS))
stack_con$regressionType <- "negbin"
stack_con$outcome <- OUTCOMES_CONTINUOUS
stack_con$exposure <- "IQR_Neuroticism_CA"
stack_con$confounders <- list(c(CONFOUNDERS_BINARY,CONFOUNDERS_CATEGORY))
stack_con$data <- "d"

stack <- rbind(stack_bin, stack_con)

retval <- list()

retval <- vector("list",length=nrow(stack))
for(i in 1:length(retval)){
  print(stack$outcome[i])
  print(xtabs(~d[[stack$outcome[i]]]))
  retval[[i]] <- RAWmisc::ProcessStack(stack=stack,i=i,formatResults=TRUE)
}
  
retval <- rbindlist(retval)
