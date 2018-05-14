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
  RAWmisc::PROJ$RAW,"Neuroticism_health_consumption_180328.sav"
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

OUTCOMES_RECODE_TO_BINARY <- c(
                               "Läkare_tid.blödning",
                               "Läkare_PE_HT",
                               "Läkare_minskade_fosterrörelser",
                               "Läkare_pga_tid.komplikation",
                               "Läkare_sen.blödning",
                               "Läkare_disk.förlossningssätt",
                               "Läkare_missbildning_tillväxthämning",
                               "Läkare_oro_sömnsvårighet",
                               "Läkare_prem.sammandragningar",
                               "Läkare_hyperemesis",
                               "Läkare_klåda_hepatos",
                               "Läkare_misst_vattenavgång",
                               "Läkare_buksmärta",
                               "Läkare_vändning_överburenhet",
                               "Läkare_annat")

for(i in OUTCOMES_RECODE_TO_BINARY){
  d[[i]] <- as.numeric(d[[i]]>0)
}

OUTCOMES_BINARY <- c(
  "Base_prog_CA",
  "Hospital_admission_CA",
  "Prenatal_diagnostics_CA",
  "Sick_leave_CA",
  "Aurora_CA",
  "Annan_behandling",
  "Obstetrician_fetal_movements_D",
  "Obstetrician_contractions_D",
  "Elective_CS_CA",
  "Induction_CA",
  OUTCOMES_RECODE_TO_BINARY
)

OUTCOMES_CONTINUOUS <- c(
  "Obgyn_physician_count_CA",
  "Midwife_ObGyn_phone_visits_CA",
  "Maternal_care_midwife_CA",
  "Ultrasounds_count_CA"
)
#"Läkare_duplex",
#"Läkare_DIAB",

EXPOSURE <- "Neuroticism_CA"

EFFECT_MODIFIER <- "High_risk_somatic_disease_CA"

CONFOUNDERS_BINARY <- c(
  "Parity_D_CA",
  "Country_CA",
  "Education_CA",
  "Working_status_CA",
  "Single_CA",
  "Psychiatric_disorder_medication_CA",
  "Graviditetsar"
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

Table1()

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

stack_linear_with_graviditetsar <- rbind(stack_bin, stack_con)
stack_linear_without_graviditetsar <- copy(stack_linear_with_graviditetsar)
for(i in 1:length(stack_linear_without_graviditetsar$confounders)){
  stack_linear_without_graviditetsar$confounders[[i]] <- stack_linear_without_graviditetsar$confounders[[i]][stack_linear_without_graviditetsar$confounders[[i]]!="Graviditetsar"]
}

stack_spline_with_graviditetsar <- copy(stack_linear_with_graviditetsar)
stack_spline_with_graviditetsar$exposure <- "splines::ns(IQR_Neuroticism_CA,df=4)"

stack_spline_without_graviditetsar <- copy(stack_linear_without_graviditetsar)
stack_spline_without_graviditetsar$exposure <- "splines::ns(IQR_Neuroticism_CA,df=4)"

retval <- list()
for(stack_name in c(
  "stack_linear_with_graviditetsar",
  "stack_linear_without_graviditetsar",
  "stack_spline_with_graviditetsar",
  "stack_spline_without_graviditetsar"
)){
  stack <- get(stack_name)
  
  temp <- vector("list",length=nrow(stack))
  for(i in 1:length(temp)){
    print(stack$outcome[i])
    print(xtabs(~d[[stack$outcome[i]]]))
    temp[[i]] <- RAWmisc::ProcessStack(stack=stack,i=i,formatResults=TRUE)
  }
  retval[[stack_name]] <- rbindlist(temp)
  retval[[stack_name]][,stack_name:=stack_name]
}

a <- retval[["stack_linear_without_graviditetsar"]][,c("regressionType","outcome","a_aic")]
setnames(a,"a_aic","a_linear_aic")
b <- retval[["stack_spline_without_graviditetsar"]][,c("outcome","a_aic")]
setnames(b,"a_aic","a_spline_aic")

spline_or_linear <- merge(a,b,by="outcome")
spline_or_linear[,aic_linear_minus_spline:=a_linear_aic-a_spline_aic]
spline_or_linear[,exposure:=ifelse(aic_linear_minus_spline>3,"0 to 1, splines::ns(IQR_Neuroticism_CA,df=4)","IQR_Neuroticism_CA")]

openxlsx::write.xlsx(spline_or_linear,file.path(RAWmisc::PROJ$SHARED_TODAY,"tables","table2_without_gravidetetsar_spline_or_linear.xlsx"))

retval <- rbindlist(retval)
retval <- merge(retval,spline_or_linear[,c("outcome","exposure")],by=c("outcome","exposure"))
setorder(retval,stack_name,outcome)

retval_with_graviditetsar <- retval[stringr::str_detect(stack_name,"_with_")]
retval_without_graviditetsar <- retval[stringr::str_detect(stack_name,"_without_")]
retval_with_graviditetsar[,stack_name:=NULL]
retval_without_graviditetsar[,stack_name:=NULL]

retval_with_graviditetsar <- RAWmisc::FormatResultsStack(retval_with_graviditetsar,bonf=F,useWald = FALSE, useLRT=TRUE)
retval_without_graviditetsar <- RAWmisc::FormatResultsStack(retval_without_graviditetsar,bonf=F,useWald = FALSE, useLRT=TRUE)

openxlsx::write.xlsx(retval_with_graviditetsar,file.path(RAWmisc::PROJ$SHARED_TODAY,"tables","table3_results_with_graviditetsar.xlsx"))
openxlsx::write.xlsx(retval_without_graviditetsar,file.path(RAWmisc::PROJ$SHARED_TODAY,"tables","table3_results_without_graviditetsar.xlsx"))

for(stack_name in c(
  "stack_linear_with_graviditetsar",
  "stack_linear_without_graviditetsar",
  "stack_spline_with_graviditetsar",
  "stack_spline_without_graviditetsar"
)){
  openxlsx::write.xlsx(get(stack_name),file.path(RAWmisc::PROJ$SHARED_TODAY,"tables",sprintf("details_table3_%s.xlsx",stack_name)))
}


stack_interaction <- stack_linear_without_graviditetsar[stack_linear_without_graviditetsar$outcome=="Prenatal_diagnostics_CA",]
stack_interaction$confounders[[1]] <- c(stack_interaction$confounders[[1]],stack_interaction$exposure[[1]])
stack_interaction$exposure[[1]] <- "IQR_Neuroticism_CA:Age_3_CA"

retval <- vector("list",length=nrow(stack_interaction))
for(i in 1:length(retval)){
  retval[[i]] <- RAWmisc::ProcessStack(stack=stack_interaction,i=i,formatResults=TRUE)
}
retval <- rbindlist(retval)

openxlsx::write.xlsx(stack_interaction,file.path(RAWmisc::PROJ$SHARED_TODAY,"tables","details_interaction.xlsx"))
openxlsx::write.xlsx(retval,file.path(RAWmisc::PROJ$SHARED_TODAY,"tables","interaction.xlsx"))





