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
library(mice)

dir.create(file.path(RAWmisc::PROJ$SHARED_TODAY,"unimputed"))
dir.create(file.path(RAWmisc::PROJ$SHARED_TODAY,"imputed"))

d <- data.table(haven::read_spss(file.path(RAWmisc::PROJ$RAW,"Patricia SSP_MFR CA_180209.sav")))
d <- d[Filter_1para_singleton==1]
nrow(d)

OUTCOMES <- c(
  "Gestational_diabetes_CA",
  "Gestational_diabetes_ICD9_10_CA",
  "Gestational_HT_PE_CA",
  "Planned_CS_analysis_CA",
  "Induction_analysis_CA",
  "Emergency_CS_analysis_CA",
  "VE_analysis_CA",
  "Uterine_inertia_analysis_CA",
  "Severe_lacerations_analysis_CA_new",
  "Placental_retention_analysis_CA",
  "Postpartum_hemorrhage_CA",
  "Premature_w37_analysis_CA_new",
  "SGA_analysis_CA",
  "SGA_10_CA",
  "LGA_analysis_CA",
  "LGA_10_CA",
  "Apgar5_sub7_CA",
  "Composite_worstcase_strict_CA",
  "Icke_instrumentell_analysis_CA",
  "Spontaneous_vaginal_analysis_CA",
  "CS_analysis_CA",
  "Planned_CS_analysis_CA_all",
  "Induction_analysis_CA_all",
  "Emergency_CS_analysis_CA_all",
  "VE_analysis_CA_all",
  "Uterine_inertia_analysis_CA_all",
  "Severe_lacerations_CA",
  "Placental_retention_analysis_CA_all"
  )

POTENTIAL_CONFOUNDERS_BINARY_1 <- c(
  "Education_cat_CA",
  "ROKGRAV_CA",
  "Involuntary_childless_IVF_new_CA",
  "Year_ICDcat_CA"
)

POTENTIAL_CONFOUNDERS_ORDINAL_1 <- c(
  "MALDER_cat_CA",
  "MBMI_cat_CA_new",
  "MLANGD_cat_CA"
)

CONFOUNDERS_1 <- c(
  POTENTIAL_CONFOUNDERS_BINARY_1,
  POTENTIAL_CONFOUNDERS_ORDINAL_1
)

POTENTIAL_CONFOUNDERS_BINARY_2 <- c(
  "Psychiatric_morbidity_final_CA"
)

CONFOUNDERS_2 <- POTENTIAL_CONFOUNDERS_BINARY_2

IMPUTED_VARIABLES <- c(
  "ROKGRAV_CA",
  "MBMI_cat_CA_new",
  "MLANGD_cat_CA"
)

d <- d[,c(
  "Neuroticism",
  OUTCOMES,
  CONFOUNDERS_1,
  CONFOUNDERS_2
),with=F]

for(i in POTENTIAL_CONFOUNDERS_ORDINAL_1){
  d[[i]] <- as.factor(d[[i]])
}

masterData <- copy(d)

fileConn<-file(file.path(RAWmisc::PROJ$SHARED_TODAY,"details.txt"))
writeLines(c(
  sprintf("**OUTCOMES**\n%s",paste0(OUTCOMES,collapse="\n")),
  sprintf("\n\n**POTENTIAL_CONFOUNDERS_BINARY_1**\n%s",paste0(POTENTIAL_CONFOUNDERS_BINARY_1,collapse="\n")),
  sprintf("\n\n**POTENTIAL_CONFOUNDERS_ORDINAL_1**\n%s",paste0(POTENTIAL_CONFOUNDERS_ORDINAL_1,collapse="\n")),
  sprintf("\n\n**POTENTIAL_CONFOUNDERS_BINARY_2**\n%s",paste0(POTENTIAL_CONFOUNDERS_BINARY_2,collapse="\n")),
  sprintf("\n\n**IMPUTED_VARIABLES**\n%s",paste0(IMPUTED_VARIABLES,collapse="\n"))
  ), fileConn)
close(fileConn)


for(run in c("unimputed","imputed")){
  iqr <- IQR(masterData$Neuroticism, na.rm=T) 
  
  aicResults <-
    linearResultsCrude <- 
    linearResultsAdjusted1 <- 
    linearResultsAdjusted2 <-
    vector("list", length = length(OUTCOMES))
  
  for(i in 1:length(OUTCOMES)){
    if(run=="unimputed"){
      d <- masterData[!is.na(get(OUTCOMES[i])),c(
        "Neuroticism",
        OUTCOMES[i],
        CONFOUNDERS_1,
        CONFOUNDERS_2
      ),with=F]
      
      data_crude <- na.omit(masterData[,c(OUTCOMES[i]),with=F])
      data_adj1 <- na.omit(masterData[,c(OUTCOMES[i],CONFOUNDERS_1),with=F])
      data_adj2 <- na.omit(masterData[,c(OUTCOMES[i],CONFOUNDERS_1,CONFOUNDERS_2),with=F])
      
      cases_crude <- sum(data_crude[[OUTCOMES[i]]],na.rm=T)
      controls_crude <- sum(!data_crude[[OUTCOMES[i]]],na.rm=T)
      
      cases_adj1 <- sum(data_adj1[[OUTCOMES[i]]],na.rm=T)
      controls_adj1 <- sum(!data_adj1[[OUTCOMES[i]]],na.rm=T)
      
      cases_adj2 <- sum(data_adj2[[OUTCOMES[i]]],na.rm=T)
      controls_adj2 <- sum(!data_adj2[[OUTCOMES[i]]],na.rm=T)
    } else {
      d <- masterData[,c(
        "Neuroticism",
        OUTCOMES[i],
        CONFOUNDERS_1,
        CONFOUNDERS_2
      ),with=F]
      keep <- rep(TRUE,nrow(d))
      for(k in 1:ncol(d)){
        if(names(d)[k] %in% IMPUTED_VARIABLES) next
        keep[is.na(d[[k]])] <- FALSE
      }
      d <- d[keep]
      
      cases_crude <- cases_adj1 <- cases_adj2 <- sum(d[[OUTCOMES[i]]],na.rm=T)
      controls_crude <- controls_adj1 <- controls_adj2 <- sum(!d[[OUTCOMES[i]]],na.rm=T)
      d <- mice::mice(d, m=20, method="pmm", seed=4)
    }
   
    if(run=="unimputed"){ 
      ddist <- datadist(d)
      options(datadist='ddist')
      
      ##### RUNNING THE MODEL (CRUDE) 
      fit_linear_crude <- lrm(as.formula(sprintf("%s ~ Neuroticism",OUTCOMES[i])),
                  x=TRUE, y=TRUE, data=d)
      fit_spline_crude <- lrm(as.formula(sprintf("%s ~ rcs(Neuroticism,c(250,300,350))",OUTCOMES[i])),
                  x=TRUE, y=TRUE, data=d)
      
      aicResults[[i]] <- data.frame(
        aic_linear_crude = AIC(fit_linear_crude),
        aic_spline_crude = AIC(fit_spline_crude)
      )
    }
    
    fit_linear_crude <- 
      with(d,glm(as.formula(sprintf("%s ~ Neuroticism",
                                    OUTCOMES[i])),
                 family=binomial()))

    fit_linear_adjusted1 <- 
      with(d,glm(as.formula(sprintf("%s ~ Neuroticism+%s",
                                    OUTCOMES[i],
                                    paste0(CONFOUNDERS_1,collapse="+"))), 
                 family=binomial()))

    fit_linear_adjusted2 <-
      with(d,glm(as.formula(sprintf("%s ~ Neuroticism+%s+%s",
                                    OUTCOMES[i],
                                    paste0(CONFOUNDERS_1,collapse="+"),
                                    CONFOUNDERS_2)),
                 family=binomial()))
    
    if(run=="unimputed"){
      linearResultsCrude[[i]] <- data.frame(coef(summary(fit_linear_crude)))
      linearResultsAdjusted1[[i]] <- data.frame(coef(summary(fit_linear_adjusted1)))
      linearResultsAdjusted2[[i]] <- data.frame(coef(summary(fit_linear_adjusted2)))
    } else {
      linearResultsCrude[[i]] <- data.frame(summary(pool(fit_linear_crude)))
      linearResultsAdjusted1[[i]] <- data.frame(summary(pool(fit_linear_adjusted1)))
      linearResultsAdjusted2[[i]] <- data.frame(summary(pool(fit_linear_adjusted2)))
    }
    
    linearResultsCrude[[i]]$cases <- cases_crude
    linearResultsCrude[[i]]$controls <- controls_crude
    
    linearResultsAdjusted1[[i]]$cases <- cases_adj1
    linearResultsAdjusted1[[i]]$controls <- controls_adj1
    
    linearResultsAdjusted2[[i]]$cases <- cases_adj2
    linearResultsAdjusted2[[i]]$controls <- controls_adj2
    
    linearResultsCrude[[i]]$var <- row.names(linearResultsCrude[[i]])
    linearResultsAdjusted1[[i]]$var <- row.names(linearResultsAdjusted1[[i]])
    linearResultsAdjusted2[[i]]$var <- row.names(linearResultsAdjusted2[[i]])
    
    linearResultsCrude[[i]]$outcome <- OUTCOMES[i]
    linearResultsAdjusted1[[i]]$outcome <- OUTCOMES[i]
    linearResultsAdjusted2[[i]]$outcome <- OUTCOMES[i]
  }

  linearResultsCrude <- rbindlist(linearResultsCrude)
  linearResultsAdjusted1 <- rbindlist(linearResultsAdjusted1)
  linearResultsAdjusted2 <- rbindlist(linearResultsAdjusted2)
  
  if(run=="unimputed"){  
    aicResults <- rbindlist(aicResults)
    aicResults[,aic_linear_minus_spline:=aic_linear_crude-aic_spline_crude]
    openxlsx::write.xlsx(aicResults,file.path(RAWmisc::PROJ$SHARED_TODAY,run,"aic.xlsx"))
  } 
  
  resMerged <- linearResultsCrude[var=="Neuroticism",c("outcome")]
  for(i in c("crude","adj1","adj2")){
    if(i=="crude"){
      res <- copy(linearResultsCrude)
      output <- "crude.xlsx"
      niceNames <- c("outcome","crude_cases","crude_controls","crude_est","crude_pval")
    } else if(i=="adj1"){
      res <- copy(linearResultsAdjusted1)
      output <- "adjusted1.xlsx"
      niceNames <- c("outcome","adj1_cases","adj1_controls","adj1_est","adj1_pval")
    } else if(i=="adj2"){
      res <- copy(linearResultsAdjusted2)
      output <- "adjusted1_and_2.xlsx"
      niceNames <- c("outcome","adj2_cases","adj2_controls","adj2_est","adj2_pval")
    }
    
    if(run=="unimputed"){
      res[,sig:=""]
      res[`Pr...z..`<0.05,sig:="*"]
      res[,estimat:=sprintf("%s%s",
                        RAWmisc::FormatEstCIFromEstSE(
                          beta=Estimate*iqr,
                          se=`Std..Error`*iqr),
                        sig)]
      res[,pval:=RAWmisc::Format(`Pr...z..`,digits=3)]
    } else {
      res[,sig:=""]
      res[`Pr...t..`<0.05,sig:="*"]
      res[,estimat:=sprintf("%s%s",
                        RAWmisc::FormatEstCIFromEstSE(
                          beta=est*iqr,
                          se=se*iqr),
                        sig)]
      res[,pval:=RAWmisc::Format(`Pr...t..`,digits=3)]
    }
    res <- res[var=="Neuroticism",c(
      "outcome","cases","controls","estimat","pval"
    )]
    openxlsx::write.xlsx(res,file.path(RAWmisc::PROJ$SHARED_TODAY,run,output))
    setnames(res,niceNames)
    resMerged <- merge(resMerged, res, by="outcome")
  }
  
  resMerged[,per_units_of_neuroticism:=iqr]
  openxlsx::write.xlsx(resMerged,file.path(RAWmisc::PROJ$SHARED_TODAY,run,"all_results_one_file.xlsx"))
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
