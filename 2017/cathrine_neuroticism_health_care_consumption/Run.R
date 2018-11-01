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

d[,Emergency_Obgyn_physician_count_CA:=
    Läkare_tid.blödning+
    Läkare_sen.blödning+
    Läkare_hyperemesis+
    Läkare_PE_HT+
    Läkare_prem.sammandragningar+
    Läkare_minskade_fosterrörelser+
    Läkare_buksmärta+
    Läkare_misst_vattenavgång+
    Läkare_oro_sömnsvårighet
    ]

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
  "Induction_CA"
)

OUTCOMES_CONTINUOUS <- c(
  "Obgyn_physician_count_CA",
  "Midwife_ObGyn_phone_visits_CA",
  "Maternal_care_midwife_CA",
  "Ultrasounds_count_CA",
  "Emergency_Obgyn_physician_count_CA"
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
iqr <- IQR(d[["Neuroticism_CA"]])
d[["IQR_Neuroticism_CA"]] <- (d[["Neuroticism_CA"]]-p25)/iqr

probsOfInterest <- seq(0.05,0.95,0.05)
probsOfInterest <- probsOfInterest[probsOfInterest!=0.25]
graphExposureLocationsLinear <- quantile(d[[EXPOSURE]],prob=probsOfInterest,na.rm=T)
graphExposureLocationsSpline <- c(quantile(d[[EXPOSURE]],prob=probsOfInterest,na.rm=T))

graphExposureLocationsLinearLabels <- quantile(d[[EXPOSURE]],prob=c(0.1,0.5,0.75,0.9),na.rm=T)
graphExposureLocationsSplineLabels <- quantile(d[[EXPOSURE]],prob=c(0.1,0.5,0.75,0.9),na.rm=T)

graphExposureLocationsLinear <- (graphExposureLocationsLinear-p25)/iqr
graphExposureLocationsSpline <- (graphExposureLocationsSpline-p25)/iqr

graphExposureLocationsLinearLabels <- (graphExposureLocationsLinearLabels-p25)/iqr
graphExposureLocationsSplineLabels <- (graphExposureLocationsSplineLabels-p25)/iqr

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

stack_linear_with_graviditetsar$analysisID <- RAWmisc::UUID(nrow(stack_linear_with_graviditetsar))
stack_linear_without_graviditetsar$analysisID <- RAWmisc::UUID(nrow(stack_linear_without_graviditetsar))

for(i in 1:length(stack_linear_without_graviditetsar$confounders)){
  stack_linear_without_graviditetsar$confounders[[i]] <- stack_linear_without_graviditetsar$confounders[[i]][stack_linear_without_graviditetsar$confounders[[i]]!="Graviditetsar"]
}

stack_spline_with_graviditetsar <- copy(stack_linear_with_graviditetsar)
stack_spline_with_graviditetsar$exposure <- "splines::ns(IQR_Neuroticism_CA,df=4)"

stack_spline_without_graviditetsar <- copy(stack_linear_without_graviditetsar)
stack_spline_without_graviditetsar$exposure <- "splines::ns(IQR_Neuroticism_CA,df=4)"


# adding in necessary parts for graphs

stack_linear_without_graviditetsar$graphExposureScaleMultiply <- iqr
stack_linear_without_graviditetsar$graphExposureScaleAdd <- p25
stack_linear_without_graviditetsar$graphReference <- 0
stack_linear_without_graviditetsar$graphExposureLocationsLabels <- list(graphExposureLocationsLinearLabels)
stack_linear_without_graviditetsar$graphExposureLocations <- list(graphExposureLocationsLinear)
stack_linear_without_graviditetsar$graphTitleMain <- ""#stack_linear_without_graviditetsar$outcome
stack_linear_without_graviditetsar$graphTitleX <- "Neuroticism"
stack_linear_without_graviditetsar$graphFileName <- sprintf("%s/figures/linear_%s_without_graviditetsar.png",RAWmisc::PROJ$SHARED_TODAY,stack_linear_without_graviditetsar$outcome)

stack_spline_without_graviditetsar$graphExposureScaleMultiply <- iqr
stack_spline_without_graviditetsar$graphExposureScaleAdd <- p25
stack_spline_without_graviditetsar$graphReference <- 0
stack_spline_without_graviditetsar$graphExposureLocationsLabels <- list(graphExposureLocationsSplineLabels)
stack_spline_without_graviditetsar$graphExposureLocations <- list(graphExposureLocationsSpline)
stack_spline_without_graviditetsar$graphTitleMain <- ""#stack_spline_without_graviditetsar$outcome
stack_spline_without_graviditetsar$graphTitleX <- "Neuroticism"
stack_spline_without_graviditetsar$graphFileName <- sprintf("%s/figures/spline_%s_without_graviditetsar.png",RAWmisc::PROJ$SHARED_TODAY,stack_spline_without_graviditetsar$outcome)
# fin


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

# use table 2 to establish the desired linear/splines
retval <- rbindlist(retval)
retval <- merge(retval,spline_or_linear[,c("outcome","exposure")],by=c("outcome","exposure"))
setorder(retval,analysisID)

toMerge <- spline_or_linear[,c("outcome","exposure")]
toMerge[,exposure:=stringr::str_remove(exposure,"0 to 1, ")]
stack_with_graviditetsar <- rbind(stack_linear_with_graviditetsar,stack_spline_with_graviditetsar)
stack_with_graviditetsar <- merge(stack_with_graviditetsar,toMerge,by=c("outcome","exposure"))
stack_without_graviditetsar <- rbind(stack_linear_without_graviditetsar,stack_spline_without_graviditetsar)
stack_without_graviditetsar <- merge(stack_without_graviditetsar,toMerge,by=c("outcome","exposure"))

setorder(stack_with_graviditetsar,analysisID)
setorder(stack_without_graviditetsar,analysisID)

# cleaning up table3 results
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

### extracting full results for table 3s

retval <- list()
for(stack_name in c(
  "stack_with_graviditetsar",
  "stack_without_graviditetsar"
)){
  stack <- RAWmisc::ExpandStack(get(stack_name), newAnalysisID=F)
  
  temp <- vector("list",length=nrow(stack))
  for(i in 1:length(temp)){
    print(stack$outcome[i])
    print(xtabs(~d[[stack$outcome[i]]]))
    temp[[i]] <- RAWmisc::ProcessStack(stack=stack,i=i,formatResults=TRUE)
  }
  retval <- rbindlist(temp)
  retval[,stack_name:=stack_name]
  
  openxlsx::write.xlsx(retval,file.path(
    RAWmisc::PROJ$SHARED_TODAY,
    "tables",
    sprintf("table3_confounders_%s.xlsx",stringr::str_remove(stack_name,"stack_"))))
}




### interaction_age_prenatal_diagnostics_ca

stack_interaction <- stack_linear_without_graviditetsar[stack_linear_without_graviditetsar$outcome=="Prenatal_diagnostics_CA",]
stack_interaction$confounders[[1]] <- c(stack_interaction$confounders[[1]],stack_interaction$exposure[[1]])
stack_interaction$exposure[[1]] <- "IQR_Neuroticism_CA:Age_3_CA"
stack_interaction$graphFileName <- NA

retval <- vector("list",length=nrow(stack_interaction))
for(i in 1:length(retval)){
  retval[[i]] <- RAWmisc::ProcessStack(stack=stack_interaction,i=i,formatResults=TRUE)
}
retval <- rbindlist(retval)

openxlsx::write.xlsx(stack_interaction,file.path(RAWmisc::PROJ$SHARED_TODAY,"tables","details_interaction_age_prenatal_diagnostics_ca.xlsx"))
openxlsx::write.xlsx(retval,file.path(RAWmisc::PROJ$SHARED_TODAY,"tables","interaction_age_prenatal_diagnostics_ca.xlsx"))


### interactions with c("Age_3_CA","Education_CA","Country_CA")

for(varInteract in c("Age_3_CA","Education_CA","Country_CA")) for(x in c("with_graviditetsar","without_graviditetsar")){
  stack_interaction <- get(sprintf("stack_%s",x))
  stack_interaction$analysisID <- RAWmisc::UUID(nrow(stack_interaction))
  for(i in 1:nrow(stack_interaction)){
    stack_interaction$confounders[[i]] <- c(stack_interaction$confounders[[i]],stack_interaction$exposure[[i]])
    stack_interaction$exposure[[i]] <- sprintf("%s:%s",stack_interaction$exposure[[i]],varInteract)
  }
  stack_interaction$graphFileName <- NA
  if(varInteract=="Country_CA"){
    for(i in which(stringr::str_detect(stack_interaction$exposure,"splines"))){
      stack_interaction[i,]$nameInteractions <- c("Country_CA")
    }
    for(i in which(!stringr::str_detect(stack_interaction$exposure,"splines"))){
      stack_interaction[i,]$nameBase <- "IQR_Neuroticism_CA"
      stack_interaction[i,]$nameInteractions <- c("IQR_Neuroticism_CA:Country_CA")
    }
  }
  
  retval <- vector("list",length=nrow(stack_interaction))
  for(i in 1:length(retval)){
    retval[[i]] <- RAWmisc::ProcessStack(stack=stack_interaction,i=i,formatResults=TRUE)
  }
  retval <- rbindlist(retval)
  
  openxlsx::write.xlsx(stack_interaction,file.path(
    RAWmisc::PROJ$SHARED_TODAY,
    "tables",
    sprintf("details_table4_interaction_%s_%s.xlsx",x,varInteract)))
  
  openxlsx::write.xlsx(retval,file.path(
    RAWmisc::PROJ$SHARED_TODAY,
    "tables",
    sprintf("table4_interaction_%s_%s.xlsx",x,varInteract)))
}  

varInteract <- "Country_CA"
for(lev in na.omit(unique(d$Country_CA))) for(x in c("with_graviditetsar","without_graviditetsar")){
  newVar <- sprintf("d_%s_%s",varInteract,lev)
  assign(newVar, d[get(varInteract)==lev])
  
  stack_interaction <- get(sprintf("stack_%s",x))
  stack_interaction$analysisID <- RAWmisc::UUID(nrow(stack_interaction))
  for(i in 1:nrow(stack_interaction)){
    stack_interaction$confounders[[i]] <- stack_interaction$confounders[[i]][stack_interaction$confounders[[i]] != varInteract]
  }
  stack_interaction$graphFileName <- stringr::str_replace_all(stack_interaction$graphFileName,
                                                              "figures/",
                                                              sprintf("figures/%s_%s=%s_",x,varInteract,lev)
                                                              )
  stack_interaction$data <- newVar
  
  retval <- vector("list",length=nrow(stack_interaction))
  for(i in 1:length(retval)){
    retval[[i]] <- RAWmisc::ProcessStack(stack=stack_interaction,i=i,formatResults=TRUE)
  }
  retval <- rbindlist(retval)
  
  openxlsx::write.xlsx(stack_interaction,file.path(
    RAWmisc::PROJ$SHARED_TODAY,
    "tables",
    sprintf("details_table5_interaction_%s_%s=%s.xlsx",x,varInteract,lev)))
  
  openxlsx::write.xlsx(retval,file.path(
    RAWmisc::PROJ$SHARED_TODAY,
    "tables",
    sprintf("table5_interaction_%s_%s=%s.xlsx",x,varInteract,lev)))
}  


f1 <- MASS::glm.nb(Obgyn_physician_count_CA~IQR_Neuroticism_CA,data=d[Country_CA==0])
summary(f1)

f1 <- MASS::glm.nb(Obgyn_physician_count_CA~IQR_Neuroticism_CA,data=d[Country_CA==1])
summary(f1)





stack_interaction[stack_interaction$outcome=="Base_prog_CA",]
retval[outcome=="Base_prog_CA"]$a_p_lrt

f0 <- glm(Base_prog_CA~
            Parity_D_CA+
            Country_CA+Education_CA+
            Working_status_CA+
            Single_CA+
            Psychiatric_disorder_medication_CA+
            Age_3_CA+
            BMI_3_CA+IQR_Neuroticism_CA,data=d,family=binomial)

f1 <- glm(Base_prog_CA~IQR_Neuroticism_CA:Age_3_CA+
            Parity_D_CA+
            Country_CA+Education_CA+
            Working_status_CA+
            Single_CA+
            Psychiatric_disorder_medication_CA+
            Age_3_CA+
            BMI_3_CA+IQR_Neuroticism_CA,data=d,family=binomial)

lrtest(f1,f0)



f0 <- MASS::glm.nb(Midwife_ObGyn_phone_visits_CA~
            Parity_D_CA+
            Country_CA+Education_CA+
            Working_status_CA+
            Single_CA+
            Psychiatric_disorder_medication_CA+
            Age_3_CA+
            BMI_3_CA+splines::ns(IQR_Neuroticism_CA,df=4),data=dataAdj)

f1 <- MASS::glm.nb(Midwife_ObGyn_phone_visits_CA~splines::ns(IQR_Neuroticism_CA,df=4):Age_3_CA+
            Parity_D_CA+
            Country_CA+Education_CA+
            Working_status_CA+
            Single_CA+
            Psychiatric_disorder_medication_CA+
            Age_3_CA+
            BMI_3_CA+splines::ns(IQR_Neuroticism_CA,df=4),data=dataAdj)

lmtest::lrtest(f1,f0)
stack_interaction[stack_interaction$outcome=="Midwife_ObGyn_phone_visits_CA",]
retval[outcome=="Midwife_ObGyn_phone_visits_CA"]$a_p_lrt




