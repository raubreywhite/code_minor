# main analysis
# # adjusting on v32_oversampling, ppv6_EPDS_D_9R
#
# sens analysis (discrepancy analysis)
# same as main analysis, but we exclude according to sensitivity_discrepancy_pg sensitivity_discrepancy_pp
#
# sub analysis
# exclude non-depressed women on SSRIs
# run stratified non-depressed - case_control_pregnancy/case_control_pp
# dont adjust for v32_oversampling, ppv6_EPDS_D_9R
#

# put in more details in the details document

org::AllowFileManipulationFromInitialiseProject()

org::InitialiseProject(
  HOME = "/git/code_minor/2017/hanna_paper_3/",
  RAW = "/Volumes/crypt_data/org//data_raw/code_minor/2017/hanna_paper_3/",
  CLEAN = "/Volumes/crypt_data/org//data_clean/code_minor/2017/hanna_paper_3/",
  SHARED = "/dropbox/clients/hanna/paper_3/richard/"
)

EncryptedExcel <- function(d,fileExcel,fileZip,password="richard321"){
  x <- tempdir()
  f <- file.path(x,fileExcel)
  openxlsx::write.xlsx(d,
                       f)
  
  unlink(fileZip)
  Sys.sleep(1)
  zip(fileZip, 
      files=f, 
      flags = paste("--password", password))
}

library(data.table)
library(ggplot2)


CleanData()
ls()

nrow(pg)
nrow(pp)

xtabs(~pg$sensitivity_discrepancy_pg)
xtabs(~pp$sensitivity_discrepancy_pp)

EncryptedExcel(pg[,c(pg_oversampling_main,
                     pg_subanalysis_depressed,
                     pg_sensitivity,
                     pg_confs,
                     pg_ims),with=F],
               fileExcel="pg_selected_vars.xlsx",
               fileZip=file.path(org::PROJ$SHARED_TODAY,"pg_selected_vars.zip"))

EncryptedExcel(pg,
               fileExcel="pg_selected_vars.xlsx",
               fileZip=file.path(org::PROJ$SHARED_TODAY,"pg.zip"))

EncryptedExcel(pp[,c(pp_oversampling_main,
                     pp_subanalysis_depressed,
                     pp_sensitivity,
                     pp_confs,
                     pp_ims),with=F],
               fileExcel="pp_selected_vars.xlsx",
               fileZip=file.path(org::PROJ$SHARED_TODAY,"pp_selected_vars.zip"))

EncryptedExcel(pp,
               fileExcel="pp.xlsx",
               fileZip=file.path(org::PROJ$SHARED_TODAY,"pp.zip"))


sink(file.path(org::PROJ$SHARED_TODAY,"std_dev_ims_pg_sensitivity_over_main.txt"))
sd(pg$im_sample_day_preg)
sd(pg[sensitivity_discrepancy_pg==1]$im_sample_day_preg)
sink()

length(pg_ims)
length(pp_ims)

sink(file.path(org::PROJ$SHARED_TODAY,"SSRI_CROSSTABS.txt"))
xtabs(~pg$v32_SSRI,addNA=T)
xtabs(~pp$ppv6_SSRI,addNA=T)
sink()

x <- readxl::read_excel("/dropbox/analyses/results_shared/code_minor/2017/hanna_paper_elin/2018-06-21/data_inflammation_factors.xlsx")
setnames(x,"zscorePG","zscoreNEWPG")
setnames(x,"zscorePP","zscoreNEWPP")

y <- merge(pg[,c("CustomDataR","zscorePG","im_sample_day_preg")],x[,c("CustomDataR","zscoreNEWPG")],by="CustomDataR")
plot(y$zscorePG~y$zscoreNEWPG)
cor(y$zscorePG,y$zscoreNEWPG)

png(file.path(org::PROJ$SHARED_TODAY,"lomb_scargle_periodogram_pg.png"),width=1000,height=600)
lomb::lsp(as.matrix(na.omit(y[,c("im_sample_day_preg","zscoreNEWPG")])),type="period",from=30,to=500, ofac=20,alpha=0.05)
dev.off()

LombScargle <- lomb::lsp(as.matrix(na.omit(y[,c("im_sample_day_preg","zscoreNEWPG")])),type="period",from=30,to=500, ofac=20,alpha=0.05)
lomb.df <- data.frame(period=LombScargle$scanned, power=LombScargle$power)
q <- ggplot(lomb.df, aes(period, power))
q <- q + geom_line()
q <- q + labs(y="Normalised power")
q <- q + geom_hline(yintercept=LombScargle$sig.level, linetype="dashed")
q <- q + scale_x_continuous("Period")
q <- q + theme_gray(base_size=20)
RAWmisc::saveA4(q,filename=file.path(org::PROJ$SHARED_TODAY,"FIXED_lomb_scargle_periodogram_pg.png"))


y <- merge(pp[,c("CustomDataR","zscorePP","im_sample_day_pp")],x[,c("CustomDataR","zscoreNEWPP")],by="CustomDataR")
plot(y$zscorePP~y$zscoreNEWPP)
cor(y$zscorePP,y$zscoreNEWPP)

png(file.path(org::PROJ$SHARED_TODAY,"lomb_scargle_periodogram_pp.png"),width=1000,height=600)
lomb::lsp(as.matrix(na.omit(y[,c("im_sample_day_pp","zscoreNEWPP")])),type="period",from=30,to=500, ofac=20,alpha=0.05)
dev.off()

LombScargle <- lomb::lsp(as.matrix(na.omit(y[,c("im_sample_day_pp","zscoreNEWPP")])),type="period",from=30,to=500, ofac=20,alpha=0.05)
lomb.df <- data.frame(period=LombScargle$scanned, power=LombScargle$power)
q <- ggplot(lomb.df, aes(period, power))
q <- q + geom_line()
q <- q + labs(y="Normalised power")
q <- q + geom_hline(yintercept=LombScargle$sig.level, linetype="dashed")
q <- q + scale_x_continuous("Period")
q <- q + theme_gray(base_size=20)
RAWmisc::saveA4(q,filename=file.path(org::PROJ$SHARED_TODAY,"FIXED_lomb_scargle_periodogram_pp.png"))

SeasonalAnalysis()

#SeasonalAnalysisWithInteraction()
#SeasonalAdjustedIMPredictingDepression()


# end?

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

dir.create(file.path(org::PROJ$SHARED_TODAY,"code_check_depression_as_outcome_seasonal_adjusted"))
openxlsx::write.xlsx(retval,file.path(org::PROJ$SHARED_TODAY,"code_check_depression_as_outcome_seasonal_adjusted","pg.xlsx"))
openxlsx::write.xlsx(stack_pg,file.path(org::PROJ$SHARED_TODAY,"code_check_depression_as_outcome_seasonal_adjusted","pg_details.xlsx"))


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

dir.create(file.path(org::PROJ$SHARED_TODAY,"depression_as_outcome_abs_seasonal_difference"))
openxlsx::write.xlsx(retval,file.path(org::PROJ$SHARED_TODAY,"depression_as_outcome_abs_seasonal_difference","pg.xlsx"))
openxlsx::write.xlsx(stack_pg,file.path(org::PROJ$SHARED_TODAY,"depression_as_outcome_abs_seasonal_difference","pg_details.xlsx"))

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

openxlsx::write.xlsx(retval,file.path(org::PROJ$SHARED_TODAY,"depression_as_outcome_abs_seasonal_difference","pp.xlsx"))
openxlsx::write.xlsx(stack_pp,file.path(org::PROJ$SHARED_TODAY,"depression_as_outcome_abs_seasonal_difference","pp_details.xlsx"))


