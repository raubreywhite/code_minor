SeasonalAdjustedIMPredictingDepression <- function(){
  dir.create(file.path(RAWmisc::PROJ$SHARED_TODAY,"depression_as_outcome"))
  
  sampleSize <- list()
  pg_retval <- pp_retval <- list()
  for(k in 1:2){
    if(k==1){
      pgorpp <- "pg"
      ims <- pg_ims
      confs <- pg_confs
      dep <- pg_depressed
      data <- pg
      sensitivity <- pg_sensitivity
      seasonalvars <- "SIN_366_preg + COS_366_preg"
    } else if(k==2){
      pgorpp <- "pp"
      ims <- pp_ims
      confs <- pp_confs
      dep <- pp_depressed
      data <- pp
      sensitivity <- pp_sensitivity
      seasonalvars <- "SIN_366_pp + COS_366_pp"
    }
    
    fileConn<-file(file.path(RAWmisc::PROJ$SHARED_TODAY,
                             "depression_as_outcome",
                             sprintf("details_%s.txt",pgorpp)))
    writeLines(c(
      sprintf("\n\n**OUTCOMES**\n%s",paste0(dep,collapse="\n")),
      sprintf("\n\n**CONFOUNDERS**\n%s",paste0(seasonalvars,collapse="\n")),
      sprintf("\n\n**EXPOSURES (IFS)**\n%s",paste0(ims,collapse="\n"))
    ), fileConn)
    close(fileConn)
    
    retval <- vector("list",length(ims))
    for(i in 1:length(ims)){
      formula0 <- sprintf("%s ~ %s",dep,ims[i])
      formula1 <- sprintf("%s ~ %s + %s",dep,ims[i],seasonalvars)
      
      fit0 <- glm(as.formula(formula0),data=data,family="binomial")
      res0 <- data.frame(coef(summary(fit0)))
      names(res0) <- c("beta","se","z","p")
      res0$var <- row.names(res0)
      res0$type <- "crude"
      res0$n <- nrow(data)
      
      fit1 <- glm(as.formula(formula1),data=data,family="binomial")
      res1 <- data.frame(coef(summary(fit1)))
      names(res1) <- c("beta","se","z","p")
      res1$var <- row.names(res1)
      res1$type <- "adj"
      res1$n <- nrow(data)
      
      res <- rbind(res0,res1)
      retval[[i]] <- res
    }
    
    if(k==1){
      pg_retval <- rbindlist(retval)
    } else {
      pp_retval <- rbindlist(retval)
    }
  }
  
  pg_retval <- pg_retval[var %in% pg_ims]
  pp_retval <- pp_retval[var %in% pp_ims]
  
  pg_retval[,sigbonf:=ifelse(p*.N/2 < 0.05,"*","")]
  pp_retval[,sigbonf:=ifelse(p*.N/2 < 0.05,"*","")]
  
  pg_retval[,oddsRatio:=sprintf("%s%s",RAWmisc::FormatEstCIFromEstSE(beta,se,exp = T),sigbonf)]
  pp_retval[,oddsRatio:=sprintf("%s%s",RAWmisc::FormatEstCIFromEstSE(beta,se,exp = T),sigbonf)]
  
  pg_retval[,pbonf:=RAWmisc::Format(ifelse(p*.N/2 > 1, 1, p*.N/2),digits=3)]
  pp_retval[,pbonf:=RAWmisc::Format(ifelse(p*.N/2 > 1, 1, p*.N/2),digits=3)]
  
  pg_retval[,p:=RAWmisc::Format(p,digits=3)]
  pp_retval[,p:=RAWmisc::Format(p,digits=3)]
  
  pg_retval <- pg_retval[,c("var","type","oddsRatio","p","pbonf","n")]
  pp_retval <- pp_retval[,c("var","type","oddsRatio","p","pbonf","n")]
  
  pg_retval <- dcast.data.table(pg_retval,var~type,value.var=c("oddsRatio","p","pbonf","n"))
  pp_retval <- dcast.data.table(pp_retval,var~type,value.var=c("oddsRatio","p","pbonf","n"))
  
  pg_retval[,n_adj:=NULL]
  pp_retval[,n_adj:=NULL]
  
  setcolorder(pg_retval,c("var","n_crude",
                          "oddsRatio_crude","p_crude","pbonf_crude",
                          "oddsRatio_adj","p_adj","pbonf_adj"))
  setcolorder(pp_retval,c("var","n_crude",
                          "oddsRatio_crude","p_crude","pbonf_crude",
                          "oddsRatio_adj","p_adj","pbonf_adj"))
  
  
  openxlsx::write.xlsx(pg_retval,file.path(RAWmisc::PROJ$SHARED_TODAY,"depression_as_outcome","pg.xlsx"))
  openxlsx::write.xlsx(pp_retval,file.path(RAWmisc::PROJ$SHARED_TODAY,"depression_as_outcome","pp.xlsx"))
}