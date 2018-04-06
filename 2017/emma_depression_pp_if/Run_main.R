if(.Platform$OS.type=="unix"){
  RAWmisc::UseRClone()
  RAWmisc::AllowFileManipulationFromInitialiseProject()
  
  if(length(list.files("/dropbox"))>0){
    SHARED <- "/dropbox/analyses/results_shared/code_minor/2017/emma_depression_pp_if/"
    RCLONE_SHARED <- NULL
  } else {
    SHARED <- "/tmp/results_shared/code_minor/2017/emma_depression_pp_if/"
    RCLONE_SHARED <- "data:/analyses/results_shared/code_minor/2017/emma_depression_pp_if/"
  }
  
  RAWmisc::InitialiseProject(
    HOME = "/git/code_minor/2017/emma_depression_pp_if/",
    RAW = "/tmp/data_raw/code_minor/2017/emma_depression_pp_if/",
    SHARED = SHARED,
    RCLONE_RAW = "crypt:/data_raw/code_minor/2017/emma_depression_pp_if/",
    RCLONE_SHARED = RCLONE_SHARED
  )
}

library(data.table)
library(ggplot2)

dir.create(file.path(RAWmisc::PROJ$SHARED_TODAY,"primary_aim"))
dir.create(file.path(RAWmisc::PROJ$SHARED_TODAY,"primary_aim","AvsBC"))
dir.create(file.path(RAWmisc::PROJ$SHARED_TODAY,"primary_aim","AvsB"))

d <- haven::read_sav(file.path(RAWmisc::PROJ$RAW,"Primary aim_PPart_180216_to Richard.sav"))
setDT(d)

# clean outcome
d$AvsBC <- d$main_analysis_groups-1
d$AvsBC[d$sensitivity_analysis_groups==4] <- NA
d$AvsB <- as.numeric(d$AvsBC)
d$AvsB[d$sensitivity_analysis_groups==3] <- NA

# clean exposures
IFs <- names(d)
IFs <- IFs[stringr::str_detect(IFs,"^im_log2_")]
IFs <- IFs[1:which(IFs=="im_log2_196_CSF_1_pp")]
IFandZ <- c(IFs,"Infl_RWall_meanZPP")

# clean confounders
d[,breastfeeding_6vpp:=(breastfeeding_6vpp-1)/2]

# Determining confounders
confoundersPossible <- c(
  "age_at_partus_emma",
  "prepregnancy_bmi_emma",                           
  "education",                             
  "employment",
  "history_of_depression",
  "infl_reum_disease_emma",
  "ppv6_marital_status",
  "parity_dichotomous",
  "infant_gender_emma",
  "ivf_emma",
  "pregnancy_complications_emma",
  "premature_emma",
  "deliverymode_dichotomous",
  "days_from_partus_emma",
  "breastfeeding_6vpp",
  "antibiotics_emma",
  "astma_allergy_emma",
  "hormones_emma",
  "nsaid_emma",
  "paracetamol_emma",
  "levaxin_emma",
  "bloodpressure_med_emma",
  "mini_subgroup_bipolar_psycosis",
  "mini_subgroup_anxiety")

results <- vector("list",10000)
resultsIndex <- 1
for(i in IFs){
  formulaBase <- sprintf("%s ~ %s", "AvsBC",i)
  fitBase <- glm(as.formula(formulaBase),data=d,family=binomial())
  for(j in confoundersPossible){
    formulaC1 <- sprintf("%s ~ %s + %s", "AvsBC",i,j)
    fitC1 <- glm(as.formula(formulaC1),data=d,family=binomial())
    retval <- data.frame(
      confounder=j,
      coefBase=coef(fitBase)[i],
      coefAdjusted=coef(fitC1)[i],
      pvalBase=coef(summary(fitBase))[2,4])
    results[[resultsIndex]] <- retval
    resultsIndex <- resultsIndex + 1
  }
}
results <- rbindlist(results)
results[,change:=(coefAdjusted-coefBase)/coefBase]
results[,changeMoreThan10:=0]
results[abs(change)>0.1,changeMoreThan10:=1]
confounders <- results[pvalBase<0.05,.(numSigIFs=.N,averageChange=mean(abs(change)),changeMoreThan10perc=mean(changeMoreThan10)),by=confounder]
setorder(confounders,-changeMoreThan10perc)
openxlsx::write.xlsx(confounders, file.path(RAWmisc::PROJ$SHARED_TODAY,"primary_aim","confounders_A_vs_BC.xlsx"))

confoundersDecided <- as.character(confounders[changeMoreThan10perc>=0.1]$confounder)
confoundersDecidedTop <- confoundersDecided[1:5]

fileConn<-file(file.path(RAWmisc::PROJ$SHARED_TODAY,"primary_aim","details_A_vs_BC.txt"))
writeLines(c(
  sprintf("\n\n**POTENTIAL_CONFOUNDERS**\n%s",paste0(confoundersPossible,collapse="\n")),
  sprintf("\n\n**CONFOUNDERS_DECIDED**\n%s",paste0(confoundersDecided,collapse="\n")),
  sprintf("\n\n**CONFOUNDERS_DECIDED_TOP5**\n%s",paste0(confoundersDecidedTop,collapse="\n")),
  sprintf("\n\n**IFs**\n%s",paste0(IFandZ,collapse="\n"))
), fileConn)
close(fileConn)

for(OUTCOME in c("AvsBC","AvsB")){
  
  # table 1
  res <- list()
  for(i in confoundersPossible){
    res[[i]] <- SummarizeDispatch(
      var=d[[i]],
      by=d[[OUTCOME]],
      labelLeft=i,
      labelTop0=sprintf("Controls %s",OUTCOME),
      labelTop1=sprintf("Cases %s",OUTCOME))
  }
  
  res <- rbindlist(res)
  
  openxlsx::write.xlsx(res, file=file.path(
    RAWmisc::PROJ$SHARED_TODAY,"primary_aim",OUTCOME,"table_1.xlsx"
  ))
  
  
  # Table 3 A
  # main_analysis_groups 1 (A --) vs 2 (BC x+)
  # Table 3 B
  # sensitivity_analysis_groups 1 (A --) vs 2 (B -+), 1 (A --) vs 3 (C ++), 2 (B -+) vs 3 (C ++)
  d[,outcome:=get(OUTCOME)]
  data <- d[!is.na(outcome),c("outcome",confoundersDecided,IFandZ),with=F]
  
  res <- vector("list",1000)
  resIndex <- 1
  for(i in 1:length(IFandZ)) for(m in c("crude","adj_5_nobf","adjusted_nobf","adjusted_bf")) {
    if(m=="crude"){
      formula <- sprintf("outcome ~ %s",IFandZ[i])  
    } else if(m=="adj_5_nobf"){
      formula <- sprintf("outcome ~ %s + %s",IFandZ[i],paste(confoundersDecidedTop[confoundersDecidedTop!="breastfeeding_6vpp"],collapse="+"))
    } else if(m=="adjusted_nobf"){
      formula <- sprintf("outcome ~ %s + %s",IFandZ[i],paste(confoundersDecided[confoundersDecided!="breastfeeding_6vpp"],collapse="+"))
    } else if(m=="adjusted_bf"){
      formula <- sprintf("outcome ~ %s + %s",IFandZ[i],paste(confoundersDecided,collapse="+"))
    }
    
    fit <- glm2::glm2(as.formula(formula),data=data,family=binomial())
     
    if(m=="crude"){
      vif <- 1
    } else {
      vif <- car::vif(fit)
      vif <- vif[names(vif)==IFandZ[i]]
    }
    retval <- data.frame(coef(summary(fit)))
    retval$var <- row.names(retval)
    retval$varNum <- 1:nrow(retval)
    retval$model <- m
    retval$vif <- vif
    
    res[[resIndex]] <- retval
    resIndex <- resIndex + 1
  }
  
  res <- rbindlist(res)
  res <- res[varNum==2]
  setnames(res,"Pr...z..","p")
  res[,model:=factor(model,levels=c("crude","adj_5_nobf","adjusted_nobf","adjusted_bf"))]
  res[,pbonf:=p*.N/4]
  res[,sig:=ifelse(p<0.05,"*","")]
  res[,sigbonf:=ifelse(pbonf<0.05,"*","")]
  res[,est:=sprintf("%s",
                    RAWmisc::FormatEstCIFromEstSE(beta=`Estimate`,se=`Std..Error`))]
  res[,p:=sprintf("%s%s",RAWmisc::Format(p,digits=3),sig)]
  res[pbonf>1,pbonf:=1]
  res[,pbonf:=sprintf("%s%s",RAWmisc::Format(pbonf,digits=3),sigbonf)]
  
  res[,vif:=RAWmisc::Format(vif,digits=2)]
  
  res <- dcast.data.table(res,var~model,value.var = c("est","p","pbonf","vif"))
  newOrder <- "var"
  for(f1 in c("crude","adj_5_nobf","adjusted_nobf","adjusted_bf")) for(f2 in c("est","p","pbonf","vif")) newOrder <- c(newOrder,paste0(f2,"_",f1))
  setcolorder(res,newOrder)
  
  res[,vif_crude:=NULL]
  
  set.seed(4)
  
  fit <- glmnet::cv.glmnet(as.matrix(data)[,-1],
                           as.numeric(data[[1]]),
                           nfolds=nrow(data),
                           family="binomial")
  lasso <- data.frame(as.matrix(coef(fit)))
  lasso$var <- row.names(lasso)
  setDT(lasso)
  setnames(lasso,"X1","est_lasso")
  lasso[est_lasso!=0]
  sum(lasso$est_lasso!=0)
  lasso[,est_lasso:=RAWmisc::Format(exp(est_lasso))]
  lasso[est_lasso!="1.00",est_lasso:=sprintf("%s*",est_lasso)]
  
  res <- merge(res,lasso,by="var")
  
  openxlsx::write.xlsx(res, file.path(RAWmisc::PROJ$SHARED_TODAY,"primary_aim",OUTCOME,"main_results.xlsx"))
  
  significantIFs <- res[stringr::str_detect(est_lasso,"\\*")]$var
  
  if(length(significantIFs)>0){
    
    # sensitivity_analysis_groups 1 (A --) vs 2 (B -+), 1 (A --) vs 3 (C ++), 2 (B -+) vs 3 (C ++)
    xtabs(~d$sensitivity_analysis_groups)
    dataUnimp <- d[!is.na(get(OUTCOME)),c("sensitivity_analysis_groups",confoundersDecided,significantIFs),with=F]
    
    dataBaselineAvsB <- dataUnimp[sensitivity_analysis_groups %in% c(1,2)]
    dataBaselineAvsC <- dataUnimp[sensitivity_analysis_groups %in% c(1,3)]
    dataBaselineBvsC <- dataUnimp[sensitivity_analysis_groups %in% c(2,3)]
    
    dataBaselineAvsB[,sensitivity_analysis_groups:=sensitivity_analysis_groups-1]
    dataBaselineAvsC[,sensitivity_analysis_groups:=round((sensitivity_analysis_groups-1)/2)]
    dataBaselineBvsC[,sensitivity_analysis_groups:=sensitivity_analysis_groups-2]
    
    xtabs(~dataBaselineAvsB$sensitivity_analysis_groups)
    xtabs(~dataBaselineAvsC$sensitivity_analysis_groups)
    xtabs(~dataBaselineBvsC$sensitivity_analysis_groups)
    
    res <- vector("list",1000)
    resIndex <- 1
    for(i in 1:length(significantIFs)) for(comparison in c("AvsB","AvsC","BvsC")) for(m in c("crude")) {
      outcome <- "sensitivity_analysis_groups"
      if(comparison=="AvsB"){
        data <- dataBaselineAvsB
      } else if(comparison=="AvsC"){
        data <- dataBaselineAvsC
      } else if(comparison=="BvsC"){
        data <- dataBaselineBvsC
      } 
      
      if(m=="crude"){
        formula <- sprintf("sensitivity_analysis_groups ~ %s",significantIFs[i])  
      } else if(m=="adjusted_nobf"){
        formula <- sprintf("sensitivity_analysis_groups ~ %s + %s",significantIFs[i],paste(confoundersDecided[confoundersDecided!="breastfeeding_6vpp"],collapse="+"))
      } else if(m=="adjusted_bf"){
        formula <- sprintf("sensitivity_analysis_groups ~ %s + %s",significantIFs[i],paste(confoundersDecided,collapse="+"))
      }
      
      fit <- glm2::glm2(as.formula(formula),data=data,family=binomial())
      retval <- data.frame(coef(summary(fit)))
      retval$var <- row.names(retval)
      retval$varNum <- 1:nrow(retval)
      retval$comparison <- comparison
      retval$model <- m
      
      retval$outcomeCases <- sum(data[[outcome]]==1)
      retval$outcomeControls <- sum(data[[outcome]]==0)
      retval$outcomeMissing <- sum(is.na(data[[outcome]]))
      
      retval$meanExposureCases <- mean(data[sensitivity_analysis_groups==1][[significantIFs[i]]],na.rm=T)
      retval$meanExposureCasesSD <- sd(data[sensitivity_analysis_groups==1][[significantIFs[i]]],na.rm=T)
      
      retval$meanExposureControls <- mean(data[sensitivity_analysis_groups==0][[significantIFs[i]]],na.rm=T)
      retval$meanExposureControlsSD <- sd(data[sensitivity_analysis_groups==0][[significantIFs[i]]],na.rm=T)
      
      res[[resIndex]] <- retval
      resIndex <- resIndex + 1
    }
    
    res <- rbindlist(res)
    setnames(res,"Pr...z..","p")
    setnames(res,"Std..Error","se")
    res <- res[varNum==2]
    
    res[,est:=sprintf("%s (%s, %s)",
                      RAWmisc::Format(exp(Estimate),2),
                      RAWmisc::Format(exp(Estimate-1.96*se),2),
                      RAWmisc::Format(exp(Estimate+1.96*se),2)
                      )]
    
    res[,exposureCases:=sprintf("%s (%s)",
                                     RAWmisc::Format(meanExposureCases,2),
                                     RAWmisc::Format(meanExposureCasesSD,2))]
    
    res[,exposureControls:=sprintf("%s (%s)",
                                        RAWmisc::Format(meanExposureControls,2),
                                        RAWmisc::Format(meanExposureControlsSD,2))]
    
    IFvals <- unique(res[model=="crude",c("var","exposureCases","exposureControls","comparison")])
    IFvals[comparison %in% c("AvsB","AvsC"),A:=exposureControls]
    IFvals[comparison %in% c("AvsB"),B:=exposureCases]
    IFvals[comparison %in% c("BvsC"),B:=exposureControls]
    IFvals[comparison %in% c("BvsC","AvsC"),C:=exposureCases]
    
    IFvals <- unique(IFvals[,c("var","A","B","C")])
    IFvals <- unique(na.omit(melt.data.table(IFvals,id="var")))
    IFvals <- dcast.data.table(IFvals, variable ~ var)
    
    openxlsx::write.xlsx(IFvals, file.path(RAWmisc::PROJ$SHARED_TODAY,"primary_aim",OUTCOME,"sensitivity_IF_values.xlsx"))
    
    res <- res[,c("var","outcomeMissing","outcomeCases","outcomeControls","est","p","comparison","model")]
    res[,p:=RAWmisc::Format(p,3)]
    res[p<"0.050",p:=sprintf("%s*",p)]
    
    openxlsx::write.xlsx(res, file.path(RAWmisc::PROJ$SHARED_TODAY,"primary_aim",OUTCOME,"sensitivity_logistic_regression_values.xlsx"))
    
    toPlot <- dataUnimp[,c("sensitivity_analysis_groups",significantIFs),with=F]
    toPlot[,sensitivity_analysis_groups:=factor(sensitivity_analysis_groups,levels=c(1,2,3))]
    levels(toPlot$sensitivity_analysis_groups) <- c("A (--)","B (-+)","C (++)")
    toPlot[,id:=1:.N]
    
    toPlot <- melt(toPlot,id=c("id","sensitivity_analysis_groups"))
    
    q <- ggplot(toPlot,aes(x=sensitivity_analysis_groups,y=value))
    q <- q + geom_boxplot()
    q <- q + facet_wrap(~variable,scales="free")
    q <- q + scale_x_discrete("")
    q <- q + scale_y_continuous("log2 NPX values")
    q <- q + theme_gray(16)
    RAWmisc::saveA4(q,filename=file.path(RAWmisc::PROJ$SHARED_TODAY,"primary_aim",OUTCOME,"sensitivity_boxplots.png"))
  }
}

RAWmisc::SaveProject()
  
