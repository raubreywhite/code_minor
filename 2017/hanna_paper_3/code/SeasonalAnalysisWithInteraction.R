# LRTest <- function(fit0,fit1){
#   lmtest::lrtest(fit0,fit1)$`Pr(>Chisq)`[2]
# }
# 
# ExtractFits <- function(fit0,fit1){
#   p_lrt <- LRTest(fit0,fit1)
#   res <- data.frame(coef(summary(fit1)))
#   names(res) <- c("b","se","z","p_wald")
#   res$var <- row.names(res)
#   res$n <- sum(!is.na(fit1$fitted.values))  
#   res$p_lrt <- p_lrt
#   res <- res[,c("var","n","b","se","z","p_wald","p_lrt")]
#   setDT(res)
#   return(res)
# }
# 
# ProcessStack <- function(stack,i,formatResults=FALSE){
#   if(!stack$regressionType[[i]] %in% c("logistic","linear")){
#     stop("Non-supported regression type")
#   }
#   if(stack$regressionType[[i]]=="logistic"){
#     analysisFamily <- binomial()
#     expResults <- TRUE
#   } else {
#     analysisFamily <- gaussian()
#     expResults <- FALSE
#   }
#   
#   form_crude0 <- sprintf("%s~%s",
#                         stack$outcome[[i]],
#                         1)
#   form_crude1 <- sprintf("%s~%s",
#                         stack$outcome[[i]],
#                         paste0(stack$exposure[[i]],collapse="+"))
#   
#   adjustedModelExists <- TRUE
#   if(length(stack$confounders[[i]])==1){
#     if(is.na(stack$confounders[[i]])){
#       adjustedModelExists <- FALSE
#     }
#   }
#   
#   if(!adjustedModelExists){
#     form_adj0 <- form_crude0
#     form_adj1 <- form_crude1
#   } else {
#     form_adj0 <- sprintf("%s~%s",
#                         stack$outcome[[i]],
#                         paste0(stack$confounders[[i]],collapse="+"))
#     form_adj1 <- sprintf("%s~%s+%s",
#                         stack$outcome[[i]],
#                         paste0(stack$exposure[[i]],collapse="+"),
#                         paste0(stack$confounders[[i]],collapse="+"))
#   }
#   
#   fit <- list()
#   dataCrude <- copy(get(stack$data[[i]]))
#   for(j in unlist(stringr::str_split(stack$exposure[[i]],"[:*]"))){
#     dataCrude <- dataCrude[!is.na(dataCrude[[j]])]
#   }
#   dataAdj <- copy(dataCrude)
#   for(j in unlist(stringr::str_split(stack$confounders[[i]],"[:*]"))){
#     dataAdj <- dataAdj[!is.na(dataAdj[[j]])]
#   }
#   
#   for(j in c("crude0","crude1","adj0","adj1")){
#     if(j %in% c("crude0","crude1")){
#       dataUse <- dataCrude
#     } else {
#       dataUse <- dataAdj
#     }
#     fit[[j]] <- glm(as.formula(get(sprintf("form_%s",j))),
#                     data=dataUse,
#                     family=analysisFamily)
#   }
#   
#   res_crude <- ExtractFits(fit0=fit[["crude0"]],fit1=fit[["crude1"]])
#   res_adj <- ExtractFits(fit0=fit[["adj0"]],fit1=fit[["adj1"]])
#   
#   setnames(res_crude,c("var","c_n","c_b","c_se","c_z","c_p_wald","c_p_lrt"))
#   setnames(res_adj,c("var","a_n","a_b","a_se","a_z","a_p_wald","a_p_lrt"))
#   
#   res <- merge(res_crude,res_adj,by="var")
#   
#   res[,regressionType:=stack$regressionType[[i]]]
#   
#   if(formatResults){
#     res[,a_est:=RAWmisc::FormatEstCIFromEstSE(beta=a_b,se=a_se, exp=expResults)]
#     res[,c_est:=RAWmisc::FormatEstCIFromEstSE(beta=c_b,se=c_se, exp=expResults)]
#     
#     res <- res[res$var!="(Intercept)",c(
#       "regressionType",
#       "var",
#       "c_n",
#       "c_est",
#       "c_p_wald",
#       "c_p_lrt",
#       "a_n",
#       "a_est",
#       "a_p_wald",
#       "a_p_lrt")]
#   } else {
#     res <- res[res$var!="(Intercept)",c(
#       "regressionType",
#       "var",
#       "c_n",
#       "c_b",
#       "c_se",
#       "c_z",
#       "c_p_wald",
#       "c_p_lrt",
#       "a_n",
#       "a_b",
#       "a_se",
#       "a_z",
#       "a_p_wald",
#       "a_p_lrt")]    
#   }
#   
#   return(res)
# }
# 
# TransformCosSinToAmplitudePeakTrough <- function(cos_b,sin_b){
#   b1 <- sin_b # sin
#   b2 <- cos_b # cos
#   
#   amplitude <- sqrt(b1^2 + b2^2)
#   p <- atan(b1/b2)*366/2/pi
#   if(p>0){
#     peak <- p
#     trough <- p+366/2
#   } else {
#     peak <- p+366/2
#     trough <- p+366
#   }
#   if(b1<0){
#     g <- peak
#     peak <- trough
#     trough <- g
#   }
#   return(list(
#     "amplitude"=amplitude,
#     "peak"=peak,
#     "trough"=trough
#   ))
# }

SeasonalAnalysisWithInteraction <- function(){
  dir.create(file.path(org::PROJ$SHARED_TODAY,"main_analysis_interacting_with_depression"))
  for(an in c("pg","pp")){
    if(an=="pg"){
      stack <- RAWmisc::CreateStackSkeleton(n=length(pg_ims))
      stack$regressionType <- "linear"
      stack$outcome <- pg_ims
      stack$exposure <- list(c("SIN_366_preg","COS_366_preg"))
      stack$confounders <- list(c(pg_confs,pg_depressed))
      stack$data <- "pg"
    } else {
      stack <- RAWmisc::CreateStackSkeleton(n=length(pp_ims))
      stack$regressionType <- "linear"
      stack$outcome <- pp_ims
      stack$exposure <- list(c("SIN_366_pp","COS_366_pp"))
      stack$confounders <- list(c(pp_confs,pp_depressed))
      stack$data <- "pp"
    }
    
    openxlsx::write.xlsx(stack,file=file.path(
      org::PROJ$SHARED_TODAY,
      "main_analysis_interacting_with_depression",
      sprintf("details_main_results_%s.xlsx",an)))
    
    retval_main <- vector("list",length=nrow(stack))
    for(i in 1:length(retval_main)){
      temp <- RAWmisc::ProcessStack(stack=stack,i=i)
  
      ampPeakTrough <- RAWmisc::TransformCosSinToAmplitudePeakTrough(
        cos_b=temp[exposure%in%c("COS_366_preg","COS_366_pp")]$a_b,
        sin_b=temp[exposure%in%c("SIN_366_preg","SIN_366_pp")]$a_b)
      
      retval_main[[i]] <- data.table(
        "regressionType"=temp$regressionType[1],
        "outcome"=stack$outcome[i],
        "amplitude"=ampPeakTrough$amplitude,
        "peak"=ampPeakTrough$peak,
        "trough"=ampPeakTrough$trough,
        "p_seasonality"=temp$a_p_lrt[1]
        )
    }
    retval_main <- rbindlist(retval_main)
    
    retval_main[,p_seasonality_bonf:=p_seasonality*.N]
    retval_main[p_seasonality_bonf>1,p_seasonality_bonf:=1]
    retval_main[,p_seasonality_bonf:=RAWmisc::Format(p_seasonality_bonf,digits=3)]
    retval_main[p_seasonality_bonf<0.05,p_seasonality_bonf:=sprintf("%s*",p_seasonality_bonf)]
    
    retval_main[,p_seasonality:=RAWmisc::Format(p_seasonality,digits=3)]
    retval_main[p_seasonality<0.05,p_seasonality:=sprintf("%s*",p_seasonality)]
    
    if(an=="pg"){
      stack <- RAWmisc::CreateStackSkeleton(n=length(pg_ims))
      stack$regressionType <- "linear"
      stack$outcome <- pg_ims
      stack$exposure <- list(c(sprintf("%s:SIN_366_preg",pg_depressed),
                               sprintf("%s:COS_366_preg",pg_depressed)))
      stack$confounders <- list(c(pg_confs,pg_depressed,"SIN_366_preg","COS_366_preg"))
      stack$data <- "pg"
    } else {
      stack <- RAWmisc::CreateStackSkeleton(n=length(pp_ims))
      stack$regressionType <- "linear"
      stack$outcome <- pp_ims
      stack$exposure <- list(c(sprintf("%s:SIN_366_pp",pp_depressed),
                               sprintf("%s:COS_366_pp",pp_depressed)))
      stack$confounders <- list(c(pp_confs,pp_depressed,"SIN_366_pp","COS_366_pp"))
      stack$data <- "pp"
    }
    
    openxlsx::write.xlsx(stack,file=file.path(
      org::PROJ$SHARED_TODAY,
      "main_analysis_interacting_with_depression",
      sprintf("details_interactions_%s.xlsx",an)))
    
    retval_interaction <- vector("list",length=nrow(stack))
    for(i in 1:length(retval_interaction)){
      temp <- RAWmisc::ProcessStack(stack=stack,i=i)
      retval_interaction[[i]] <- data.table(
        "outcome"=stack$outcome[i],
        "p_interaction"=temp$a_p_lrt[1]
      )
    }
    retval_interaction <- rbindlist(retval_interaction)
    
    retval_interaction[,p_interaction_bonf:=p_interaction*.N]
    retval_interaction[p_interaction_bonf>1,p_interaction_bonf:=1]
    retval_interaction[,p_interaction_bonf:=RAWmisc::Format(p_interaction_bonf,digits=3)]
    retval_interaction[p_interaction_bonf<0.05,p_interaction_bonf:=sprintf("%s*",p_interaction_bonf)]
    
    retval_interaction[,p_interaction:=RAWmisc::Format(p_interaction,digits=3)]
    retval_interaction[p_interaction<0.05,p_interaction:=sprintf("%s*",p_interaction)]
    
    retval <- merge(retval_main,retval_interaction,by="outcome")
    retval[!outcome %in% c(pg_outcome_zscore,pp_outcome_zscore),trough_to_peak_change:=100*(2^(2*amplitude)-1)]
    
    retval[,peak:=RAWmisc::Format(peak,0)]
    retval[,trough:=RAWmisc::Format(trough,0)]
    retval[,trough_to_peak_change:=sprintf("%s%%",RAWmisc::Format(trough_to_peak_change,0))]
    
    openxlsx::write.xlsx(retval,file=file.path(
      org::PROJ$SHARED_TODAY,
      "main_analysis_interacting_with_depression",
      sprintf("results_%s.xlsx",an)))
  }
}