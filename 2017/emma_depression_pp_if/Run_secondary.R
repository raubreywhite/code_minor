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
    CLEAN = "/tmp/data_clean/code_minor/2017/emma_depression_pp_if",
    BAKED = "/tmp/results_baked/code_minor/2017/emma_depression_pp_if/",
    FINAL = "/tmp/results_final/code_minor/2017/emma_depression_pp_if/",
    SHARED = SHARED,
    RCLONE_RAW = "crypt:/data_raw/code_minor/2017/emma_depression_pp_if/",
    RCLONE_SHARED = RCLONE_SHARED
  )
}

library(data.table)
library(ggplot2)

dir.create(file.path(RAWmisc::PROJ$SHARED_TODAY,"secondary_aim"))
dir.create(file.path(RAWmisc::PROJ$SHARED_TODAY,"secondary_aim","residuals"))

d <- haven::read_sav(file.path(RAWmisc::PROJ$RAW,"Secondary aim_PPart_180216 to Richard.sav"))
setDT(d)
nrow(d)

xtabs(~d$timpoint_pg0_pp1)
xtabs(~d$depressionstatus_at_sampling)
xtabs(~d$interaction_timpoint_depstatus_at_sampl)



# clean outcome
d$AvsBC <- d$main_analysis_groups-1
d$AvsBC[d$sensitivity_analysis_groups==4] <- NA
d$AvsB <- as.numeric(d$AvsBC)
d$AvsB[d$sensitivity_analysis_groups==3] <- NA

# clean exposures
IFs <- names(d)[36:106]

# clean confounders
d[,breastfeeding_6vpp:=(breastfeeding_6vpp-1)/2]

# Determining confounders
confoundersPossible <- names(d)[8:31]

results <- vector("list",10000)
resultsIndex <- 1
for(i in IFs){
  formulaBase <- sprintf("%s ~ timpoint_pg0_pp1 + depressionstatus_at_sampling + interaction_timpoint_depstatus_at_sampl",i)
  fitBase <- lm(as.formula(formulaBase),data=d)
  for(j in confoundersPossible){
    formulaC1 <- sprintf("%s ~ timpoint_pg0_pp1 + depressionstatus_at_sampling + interaction_timpoint_depstatus_at_sampl + %s",i,j)
    fitC1 <- lm(as.formula(formulaC1),data=d)
    retval <- data.frame(
      confounder=j,
      coefBase=coef(fitBase)["interaction_timpoint_depstatus_at_sampl"],
      coefAdjusted=coef(fitC1)["interaction_timpoint_depstatus_at_sampl"],
      pvalBase=coef(summary(fitBase))[4,4])
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
openxlsx::write.xlsx(confounders, file.path(RAWmisc::PROJ$SHARED_TODAY,"secondary_aim","confounders.xlsx"))

confoundersDecided <- as.character(confounders[changeMoreThan10perc>=0.1]$confounder)

fileConn<-file(file.path(RAWmisc::PROJ$SHARED_TODAY,"secondary_aim","details.txt"))
writeLines(c(
  sprintf("\n\n**POTENTIAL_CONFOUNDERS**\n%s",paste0(confoundersPossible,collapse="\n")),
  sprintf("\n\n**CONFOUNDERS_DECIDED**\n%s",paste0(confoundersDecided,collapse="\n")),
  sprintf("\n\n**IFs**\n%s",paste0(IFs,collapse="\n"))
), fileConn)
close(fileConn)


# table 1
res <- list()
for(i in confoundersPossible){
  res[[i]] <- SummarizeDispatch(
    var=d[[i]],
    by=d[["timpoint_pg0_pp1"]],
    labelLeft=i,
    labelTop0="timpoint_pg0_pp1=0",
    labelTop1="timpoint_pg0_pp1=1")
}

res <- rbindlist(res)

openxlsx::write.xlsx(res, file=file.path(
  RAWmisc::PROJ$SHARED_TODAY,"secondary_aim","table_1_by_timpoint_pg0_pp1.xlsx"
))

res <- list()
for(i in confoundersPossible){
  res[[i]] <- SummarizeDispatch(
    var=d[[i]],
    by=d[["depressionstatus_at_sampling"]],
    labelLeft=i,
    labelTop0="depressionstatus_at_sampling=0",
    labelTop1="depressionstatus_at_sampling=1")
}

res <- rbindlist(res)

openxlsx::write.xlsx(res, file=file.path(
  RAWmisc::PROJ$SHARED_TODAY,"secondary_aim","table_1_by_depressionstatus_at_sampling.xlsx"
))


res <- vector("list",1000)
resIndex <- 1
for(i in 1:length(IFs)) for(m in c("crude")) {
  if(m=="crude"){
    formula <- sprintf("%s ~ timpoint_pg0_pp1 + depressionstatus_at_sampling + interaction_timpoint_depstatus_at_sampl",IFs[i])
  }
  
  fit <- lm(as.formula(formula),data=d)
  retval <- data.frame(coef(summary(fit)))
  retval$var <- row.names(retval)
  retval$varNum <- 1:nrow(retval)
  retval$model <- m
  retval$IF <- IFs[i]
  
  plotData <- data.frame(resid=residuals(fit),pred=predict(fit))
  q <- ggplot(plotData,aes(x=pred,y=resid))
  q <- q + geom_hline(yintercept=0,colour="red")
  q <- q + geom_point()
  q <- q + labs(title=IFs[i])
  q <- q + scale_x_continuous("Predicted")
  q <- q + scale_y_continuous("Residuals")
  RAWmisc::saveA4(q,filename=file.path(
    RAWmisc::PROJ$SHARED_TODAY,"secondary_aim","residuals",sprintf("%s_resid_vs_pred.png",IFs[i])
  ))
  
  q <- ggplot(plotData,aes(sample=resid))
  q <- q + geom_qq()
  q <- q + labs(title=IFs[i])
  RAWmisc::saveA4(q,filename=file.path(
    RAWmisc::PROJ$SHARED_TODAY,"secondary_aim","residuals",sprintf("%s_q_q_plot.png",IFs[i])
  ))
  
  res[[resIndex]] <- retval
  resIndex <- resIndex + 1
}

res <- rbindlist(res)
res <- res[varNum==4]
setnames(res,"Pr...t..","p")
res[,model:=factor(model,levels=c("crude"))]
res[,pbonf:=p*.N]
res[,sig:=ifelse(p<0.05,"*","")]
res[,sigbonf:=ifelse(pbonf<0.05,"*","")]
res[,est:=sprintf("%s",
                  RAWmisc::FormatEstCIFromEstSE(beta=`Estimate`,se=`Std..Error`,exp=FALSE))]
res[,p:=sprintf("%s%s",RAWmisc::Format(p,digits=3),sig)]
res[pbonf>1,pbonf:=1]
res[,pbonf:=sprintf("%s%s",RAWmisc::Format(pbonf,digits=3),sigbonf)]

res <- dcast.data.table(res,var+IF~model,value.var = c("est","p","pbonf"))

openxlsx::write.xlsx(res, file.path(RAWmisc::PROJ$SHARED_TODAY,"secondary_aim","main_results.xlsx"))

RAWmisc::SaveProject()

