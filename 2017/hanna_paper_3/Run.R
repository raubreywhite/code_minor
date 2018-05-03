if(.Platform$OS.type=="unix"){
  RAWmisc::UseRClone()
  RAWmisc::AllowFileManipulationFromInitialiseProject()
  
  if(dir.exists("/dropbox")){
    SHARED <- "/dropbox/clients/hanna/paper_3/richard/"
    RCLONE_SHARED <- NULL
  } else {
    SHARED <- "/tmp/results_shared/code_major/2017/klima_analyses/"
    RCLONE_SHARED <- "data:/clients/hanna/paper_3/richard/"
  }
  
  RAWmisc::InitialiseProject(
    HOME = "/git/code_minor/2017/hanna_paper_3/",
    RAW = "/tmp/data_raw/code_minor/2017/hanna_paper_3/",
    CLEAN = "/tmp/data_clean/code_minor/2017/hanna_paper_3",
    BAKED = "/tmp/results_baked/code_minor/2017/hanna_paper_3/",
    FINAL = "/tmp/results_final/code_minor/2017/hanna_paper_3/",
    SHARED = SHARED,
    RCLONE_RAW = "crypt:/data_raw/code_minor/2017/hanna_paper_3/",
    RCLONE_SHARED = RCLONE_SHARED
  )
}

library(data.table)
library(ggplot2)

CleanData()
ls()

SeasonalAnalysis()
SeasonalAnalysisWithInteraction()
SeasonalAdjustedIMPredictingDepression()

stack_pg <- RAWmisc::CreateStackSkeleton(n=length(pg_ims))
stack_pg$regressionType <- "logistic"
stack_pg$outcome <- pg_depressed
stack_pg$exposure <- sprintf("seasonal_resid_%s",pg_ims)
stack_pg$confounders <- list(c("SIN_366_preg","COS_366_preg"))
stack_pg$data <- "pg"

retval <- vector("list",length=nrow(stack_pg))
for(i in 1:length(retval)){
  retval[[i]] <- RAWmisc::ProcessStack(stack=stack_pg,i=i,formatResults=TRUE)
}
retval <- rbindlist(retval)
retval <- RAWmisc::FormatResultsStack(retval,bonf=T,useWald = TRUE, useLRT=FALSE)

dir.create(file.path(RAWmisc::PROJ$SHARED_TODAY,"code_check_depression_as_outcome_seasonal_adjusted"))
openxlsx::write.xlsx(retval,file.path(RAWmisc::PROJ$SHARED_TODAY,"code_check_depression_as_outcome_seasonal_adjusted","pg.xlsx"))
openxlsx::write.xlsx(stack_pg,file.path(RAWmisc::PROJ$SHARED_TODAY,"code_check_depression_as_outcome_seasonal_adjusted","pg_details.xlsx"))


stack_pg <- RAWmisc::CreateStackSkeleton(n=length(pg_ims))
stack_pg$regressionType <- "logistic"
stack_pg$outcome <- pg_depressed
stack_pg$exposure <- sprintf("abs_seasonal_resid_%s",pg_ims)
stack_pg$confounders <- NA
stack_pg$data <- "pg"

retval <- vector("list",length=nrow(stack_pg))
for(i in 1:length(retval)){
  retval[[i]] <- RAWmisc::ProcessStack(stack=stack_pg,i=i,formatResults = TRUE)
}
retval <- rbindlist(retval)
retval <- RAWmisc::FormatResultsStack(retval,bonf=T,useWald=TRUE, useLRT=FALSE)

dir.create(file.path(RAWmisc::PROJ$SHARED_TODAY,"depression_as_outcome_abs_seasonal_difference"))
openxlsx::write.xlsx(retval,file.path(RAWmisc::PROJ$SHARED_TODAY,"depression_as_outcome_abs_seasonal_difference","pg.xlsx"))
openxlsx::write.xlsx(stack_pg,file.path(RAWmisc::PROJ$SHARED_TODAY,"depression_as_outcome_abs_seasonal_difference","pg_details.xlsx"))

stack_pp <- RAWmisc::CreateStackSkeleton(n=length(pp_ims))
stack_pp$regressionType <- "logistic"
stack_pp$outcome <- pp_depressed
stack_pp$exposure <- sprintf("abs_seasonal_resid_%s",pp_ims)
stack_pp$confounders <- NA
stack_pp$data <- "pp"

retval <- vector("list",length=nrow(stack_pp))
for(i in 1:length(retval)){
  retval[[i]] <- RAWmisc::ProcessStack(stack=stack_pp,i=i,formatResults = TRUE)
}
retval <- rbindlist(retval)
retval <- RAWmisc::FormatResultsStack(retval,bonf=TRUE,useWald=TRUE, useLRT=FALSE)

openxlsx::write.xlsx(retval,file.path(RAWmisc::PROJ$SHARED_TODAY,"depression_as_outcome_abs_seasonal_difference","pp.xlsx"))
openxlsx::write.xlsx(stack_pp,file.path(RAWmisc::PROJ$SHARED_TODAY,"depression_as_outcome_abs_seasonal_difference","pp_details.xlsx"))


