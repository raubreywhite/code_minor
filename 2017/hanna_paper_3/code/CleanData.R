CleanData <- function(){
  
  EPDS_merged <- data.table(haven::read_spss(file.path(org::PROJ$RAW,"EPDS_merged.sav")))
  EPDS_merged[,kontinuerlig_EPDS_merged_D_0_11_vs_12p:=kontinuerlig_EPDS_merged<=11]
  
  xtabs(~kontinuerlig_EPDS_merged+kontinuerlig_EPDS_merged_D,data=EPDS_merged)
  xtabs(~kontinuerlig_EPDS_merged+kontinuerlig_EPDS_merged_D_0_11_vs_12p,data=EPDS_merged)
  
  EPDS_merged_pp <- data.table(haven::read_spss(file.path(org::PROJ$RAW,"EPDS_merged_pp.sav")))
  addedPG <- data.table(haven::read_spss(file.path(org::PROJ$RAW,"final_preg_180308.sav")))
  addedPP <- data.table(haven::read_spss(file.path(org::PROJ$RAW,"final_pp_180308.sav")))
  addedPG[is.na(sensitivity_discrepancy),sensitivity_discrepancy:=0]
  addedPP[is.na(sensitivity_discrepancy),sensitivity_discrepancy:=0]
  setnames(addedPG,"exclude_SSRI","exclude_SSRI_pg")
  setnames(addedPP,"exclude_SSRI","exclude_SSRI_pp")
  setnames(addedPG,"sensitivity_discrepancy","sensitivity_discrepancy_pg")
  setnames(addedPP,"sensitivity_discrepancy","sensitivity_discrepancy_pp")
  
  
  d <- data.table(haven::read_spss(file.path(org::PROJ$RAW,"alla_large_participation_2017_09_01.sav")))
  
  nrow(d)
  d <- merge(d,addedPG,by="CustomDataR",all.x=T)
  nrow(d)
  
  nrow(d)
  d <- merge(d,addedPP,by="CustomDataR",all.x=T)
  nrow(d)
  
  nrow(d)
  d <- merge(d,EPDS_merged,by="CustomDataR",all.x=T)
  nrow(d)
  
  nrow(d)
  d <- merge(d,EPDS_merged_pp,by="CustomDataR",all.x=T)
  nrow(d)
  
  pg_outcome_zscore <- "zscorePG"
  pp_outcome_zscore <- "zscorePP"
  
  pg_ssri <- "v32_SSRI"
  pp_ssri <- "ppv6_SSRI"
  
  pg_oversampling_main <- "kontinuerlig_EPDS_merged_D_0_11_vs_12p"
  pp_oversampling_main <- "ppv6_EPDS_D_9R_190202"
  
  pg_subanalysis_depressed <- "case_control_pregnancy"
  pp_subanalysis_depressed <- "case_control_pp"
  
  pg_sensitivity <- "sensitivity_discrepancy_pg"
  pp_sensitivity <- "sensitivity_discrepancy_pp"
  
  pg_confs <- c("v32_SSRI",
                "im_fasting_sample",
                "im_sample_year_preg")
  
  pp_confs <- c("im_sample_year_pp")
  
  # remove SSRI non-depressed people for subanalysis
  xtabs(~d$case_control_pregnancy)
  d[get(pg_subanalysis_depressed)==0 & exclude_SSRI_pg==1,(pg_subanalysis_depressed):=NA]
  d[get(pp_subanalysis_depressed)==0 & exclude_SSRI_pp==1,(pp_subanalysis_depressed):=NA]
  xtabs(~d$case_control_pregnancy)
  
  # wiping out missing people
  sum(d$im_participating_preg==1,na.rm=T)
  sum(d$im_participating_pp==1,na.rm=T)
  
  for(i in c(
    pg_outcome_zscore,
    pg_confs,
    pg_oversampling_main)) d[is.na(get(i)),im_participating_preg:=0]
  for(i in c(
    pp_outcome_zscore,
    pp_confs,
    pp_oversampling_main)) d[is.na(get(i)),im_participating_pp:=0]
  
  sum(d$im_participating_preg==1,na.rm=T)
  sum(d$im_participating_pp==1,na.rm=T)
  
  
  pg <- d[im_participating_preg==1,unique(c(
    "CustomDataR",
    "im_sample_day_preg",
    pg_outcome_zscore,
    pg_confs,
    pg_oversampling_main,
    pg_subanalysis_depressed,
    pg_sensitivity,
    pg_ssri,
    "SIN_366_preg",
    "COS_366_preg")),with=F]
  
  pp <- d[im_participating_pp==1,c(
    "CustomDataR",
    "im_sample_day_pp",
    pp_outcome_zscore,
    pp_confs,
    pp_oversampling_main,
    pp_subanalysis_depressed,
    pp_sensitivity,
    pp_ssri,
    "SIN_366_pp",
    "COS_366_pp"),with=F
    ]
  
  nrow(pg)
  nrow(pp)
  
  ifs <- readRDS(file=file.path(org::PROJ$RAW,"inflammation_markers_log2_with_lod_sqrt2.RDS"))
  
  n <- names(ifs)
  n <- n[stringr::str_detect(n,"_pg$")]
  ifs_pg <- ifs[,c("CustomDataR",n),with=F]
  
  n <- names(ifs)
  n <- n[stringr::str_detect(n,"_pp$")]
  ifs_pp <- ifs[,c("CustomDataR",n),with=F]
  
  dim(pg)
  unique(pg$CustomDataR)[!unique(pg$CustomDataR) %in% unique(ifs_pg$CustomDataR)]
  pg <- merge(pg,ifs_pg,by="CustomDataR")
  dim(pg)
  
  dim(pp)
  pp <- merge(pp,ifs_pp,by="CustomDataR")
  dim(pp)
  
  pg_ims <- names(ifs_pg)
  pg_ims <- pg_ims[stringr::str_detect(pg_ims,"^im_log2_[0-9]")]
  pg_ims <- stringr::str_replace_all(pg_ims,"im_log2_","")
  
  pp_ims <- names(ifs_pp)
  pp_ims <- pp_ims[stringr::str_detect(pp_ims,"^im_log2_[0-9]")]
  pp_ims <- stringr::str_replace_all(pp_ims,"im_log2_","")
  
  pg_pcUnderLOD <- c()
  pp_pcUnderLOD <- c()
  
  for(i in 1:length(pg_ims)){
    pg_pcUnderLOD <- c(pg_pcUnderLOD,mean(pg[[sprintf("underLOD_%s",pg_ims[i])]]))
    pp_pcUnderLOD <- c(pp_pcUnderLOD,mean(pp[[sprintf("underLOD_%s",pp_ims[i])]]))
  }
  
  sum(pg_pcUnderLOD<0.25)
  sum(pp_pcUnderLOD<0.25)
  
  pg_ims <- pg_ims[pg_pcUnderLOD<0.25]
  pp_ims <- pp_ims[pp_pcUnderLOD<0.25]
  
  # REMOVING BDNF BC IT IS NOT GOOD
  pg_ims <- pg_ims[pg_ims!="103_BDNF_pg"]
  pp_ims <- pp_ims[pp_ims!="103_BDNF_pp"]
  
  pg_ims <- c(paste0("im_log2_",pg_ims),pg_outcome_zscore)
  pp_ims <- c(paste0("im_log2_",pp_ims),pp_outcome_zscore)
  
  for(p in pg_ims){
    fit <- lm(as.formula(sprintf("%s~SIN_366_preg + COS_366_preg",p)),data=pg,na.action=na.exclude)
    pred <- predict(fit)
    pg[[sprintf("seasonal_resid_%s",p)]] <- residuals(fit)
    pg[[sprintf("abs_seasonal_resid_%s",p)]] <- abs(pg[[sprintf("seasonal_resid_%s",p)]])
  }
  
  for(p in pp_ims){
    fit <- lm(as.formula(sprintf("%s~SIN_366_pp + COS_366_pp",p)),data=pp,na.action=na.exclude)
    pred <- predict(fit)
    pp[[sprintf("seasonal_resid_%s",p)]] <- residuals(fit)
    pp[[sprintf("abs_seasonal_resid_%s",p)]] <- abs(pp[[sprintf("seasonal_resid_%s",p)]])
  }
  
  assign("pg", pg, envir=globalenv())
  assign("pg_ims", pg_ims, envir=globalenv())
  assign("pg_outcome_zscore", pg_outcome_zscore, envir=globalenv())
  assign("pg_confs", pg_confs, envir=globalenv())
  assign("pg_oversampling_main", pg_oversampling_main, envir=globalenv())
  assign("pg_subanalysis_depressed", pg_subanalysis_depressed, envir=globalenv())
  assign("pg_sensitivity", pg_sensitivity, envir=globalenv())
  assign("pg_ssri", pg_ssri, envir=globalenv())
  
  assign("pp", pp, envir=globalenv())
  assign("pp_ims", pp_ims, envir=globalenv())
  assign("pp_outcome_zscore", pp_outcome_zscore, envir=globalenv())
  assign("pp_confs", pp_confs, envir=globalenv())
  assign("pp_oversampling_main", pp_oversampling_main, envir=globalenv())
  assign("pp_subanalysis_depressed", pp_subanalysis_depressed, envir=globalenv())
  assign("pp_sensitivity", pp_sensitivity, envir=globalenv())
  assign("pp_ssri", pp_ssri, envir=globalenv()) 
}