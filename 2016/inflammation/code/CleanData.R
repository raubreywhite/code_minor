
CleanData <- function(){
  #file <- system.file("extdata","Date discrepancies with CS 160115.txt",package="inflammation")
  file <- file.path(RPROJ$PROJRAW,"Date discrepancies with CS 160115.txt")
  lengthPregnancies <-data.frame(fread(file))
  names(lengthPregnancies)[1] <- "CustomDataR"
  
  #file <- system.file("extdata","data.sav",package="inflammation")
  file <- file.path(RPROJ$PROJRAW,"data.sav")
  masterData <- foreign::read.spss(file, to.data.frame=TRUE)
  masterData$Medicines_same_day_pregnancy_gr[is.na(masterData$Medicines_same_day_pregnancy_gr)] <- "other"
  masterData$Medicines_same_day_pp_gr[is.na(masterData$Medicines_same_day_pp_gr)] <- 0

  data <- masterData
  nrow(data)
  dim(data)
  data <- merge(data,lengthPregnancies,by="CustomDataR")
  dim(data)
  data$Outcome_new <- as.character(data$Outcome_new)
  data$Outcome_new <- gsub(" ","",data$Outcome_new)
  data$Outcome_new[data$Outcome_new==""] <- NA
  data$Outcome_new[data$CustomDataR==4115] <- "+-"
  data$Outcome_new[data$CustomDataR==6070] <- "+-"
  data$Outcome_new[data$CustomDataR==9212] <- "++"
  levels(data$Outcome_new)
  # twins
  unique(data$Twin)
  data$c_twin <- NA
  data$c_twin[!is.na(data$Twin)] <- FALSE
  data$c_twin[!is.na(data$c_twin) & data$Twin=="twin"] <- TRUE
  label(data$c_twin) <- "Twin"
  
  # Smoking
  table(data$ppv6_smoking, exclude=NULL)
  
  recode_c_smoker <- data.frame(
    "ppv6_smoking" = c(
      "Yes",
      "No"
    ),
    "c_smokerPP" = c(
      1,
      0
    )
  )
  
  label(recode_c_smoker$c_smokerPP) <- c("Smoker PP")
  
  table(data$ppv6_smoking, exclude=NULL)
  dim(data)
  data <- merge(data,recode_c_smoker, by="ppv6_smoking", all.x=TRUE)
  dim(data)
  table(data$ppv6_smoking, exclude=NULL)
  table(data$c_smokerPP, exclude=NULL)
  
  ## SMOKING PREGNANCY
  
  recode_c_smoker <- data.frame(
    "Smoking_preg" = c(
      "yes",
      "no"
    ),
    "c_smokerPG" = c(
      1,
      0
    )
  )
  
  label(recode_c_smoker$c_smokerPG) <- c("Smoker PG")
  
  table(data$Smoking_preg, exclude=NULL)
  dim(data)
  data <- merge(data,recode_c_smoker, by="Smoking_preg", all.x=TRUE)
  dim(data)
  table(data$Smoking_preg, exclude=NULL)
  table(data$c_smokerPG, exclude=NULL)
  
  ####
  
  keep <- !is.na(data$Outcome_new)
  nrow(data)
  sum(keep)
  #keep[data$c_twin==TRUE] <- FALSE
  sum(keep)
  #keep[data$c_smoker==TRUE] <- FALSE
  sum(keep)
  nrow(data)
  data <- data[keep,]
  nrow(data)
  
  # Manual recoding/fixing
 # data$Participation_type[data$CustomDataR==10014] <- "pp"
  
  # Depression with EPDS or MINI
  table(data$Outcome_new, exclude=NULL)
  # recode into
  # -- vs x+
  # -- vs -+
  # -- vs +x
  recode_o_pp_sensitive <- data.frame(
    "Outcome_new" = c(
      "--",
      "-+",
      "-X",
      "+-",
      "++",
      "+X",
      "X-",
      "X+"
      ),
    "o_pp_sensitive_1" = factor(c(
      0,
      1,
      NA,
      NA,
      NA,
      NA,
      NA,
      NA
    )),
    "o_pp_sensitive_2" = factor(c(
      0,
      1,
      NA,
      0,
      1,
      NA,
      0,
      1
    )),
    "o_pp_sensitive_3" = factor(c(
      0,
      0,
      0,
      1,
      1,
      1,
      NA,
      NA
    )),
    "o_pp_sensitive_4" = factor(c(
      0,
      1,
      NA,
      NA,
      1,
      NA,
      NA,
      1
    )),
    "o_pp_sensitive_5" = factor(c(
      0,
      NA,
      NA,
      1,
      1,
      1,
      NA,
      NA
    ))
  )
  
  
  
  levels(recode_o_pp_sensitive$o_pp_sensitive_1) <- c("Control","Case")
  label(recode_o_pp_sensitive$o_pp_sensitive_1) <- c("-- vs -+ (EPDS or MINI)")
  
  levels(recode_o_pp_sensitive$o_pp_sensitive_2) <- c("Control","Case")
  label(recode_o_pp_sensitive$o_pp_sensitive_2) <- c("x- vs x+ (EPDS or MINI)")
  
  levels(recode_o_pp_sensitive$o_pp_sensitive_3) <- c("Control","Case")
  label(recode_o_pp_sensitive$o_pp_sensitive_3) <- c("-x vs +x (EPDS or MINI)")
  
  levels(recode_o_pp_sensitive$o_pp_sensitive_4) <- c("Control","Case")
  label(recode_o_pp_sensitive$o_pp_sensitive_4) <- c("-- vs x+ (EPDS or MINI)")
  
  levels(recode_o_pp_sensitive$o_pp_sensitive_5) <- c("Control","Case")
  label(recode_o_pp_sensitive$o_pp_sensitive_5) <- c("-- vs +x (EPDS or MINI)")
  
  data$Outcome_new <- gsub(" ","",as.character(data$Outcome_new))
  recode_o_pp_sensitive$Outcome_new <- as.character(recode_o_pp_sensitive$Outcome_new)
  table(data$Outcome_new, exclude=NULL)
  dim(data)
  data <- merge(data,recode_o_pp_sensitive, by="Outcome_new", all.x=TRUE)
  dim(data)
  table(data$Outcome_new, exclude=NULL)
  table(data$o_pp_sensitive_1, exclude=NULL)
  table(data$o_pp_sensitive_2, exclude=NULL)
  table(data$o_pp_sensitive_3, exclude=NULL)
  
  # Covariates
  #
  # blood type
  xtabs(~data$Blood_type_pregnant)
  xtabs(~data$Blood_type_pp)
  
  data$c_bloodSerumPG <- NA
  data$c_bloodSerumPG[data$Blood_type_pregnant=="Plasma/EDTA"] <- FALSE
  data$c_bloodSerumPG[data$Blood_type_pregnant=="Serum"] <- TRUE
  
  data$c_bloodSerumPP <- NA
  data$c_bloodSerumPP[data$Blood_type_pp=="Plasma/EDTA"] <- FALSE
  data$c_bloodSerumPP[data$Blood_type_pp=="Serum"] <- TRUE

  # fasting at sampling
  
  xtabs(~data$fasting_at_sampling)
  data$c_fastingAtSample <- NA
  data$c_fastingAtSample[data$fasting_at_sampling==0] <- FALSE
  data$c_fastingAtSample[data$fasting_at_sampling==1] <- TRUE
  label(data$c_fastingAtSample) <- "Fasting at sample"
  
  # baby sex
  xtabs(~data$F_kon)
  data$c_babyMale <- NA
  data$c_babyMale[data$F_kon=="Girl"] <- FALSE
  data$c_babyMale[data$F_kon=="Boy"] <- TRUE
  label(data$c_babyMale) <- "Child is male"
  
  # pregnancy length
  data$c_lengthPG <- data$PregnSampl_to_Partus_days
  label(data$c_lengthPG) <- "Days to partus"
  
  # pregnancy length
  data$c_lengthPP <- data$Days_from_partus_until_sampling_pp
  label(data$c_lengthPP) <- "Days after delivery"
  
  # Age
  
  data$c_age <- data$B_age
  label(data$c_age) <- "Age (years)"
  data$c_ageCat <- cut(data$c_age, breaks=c(0,30,35,100),labels=c("<=30","31-35","36+"))
  label(data$c_ageCat) <- "Age (years)"
  
  # Parity
  table(data$F_parity)
  data$c_parity <- data$F_parity
  
  data$c_firstBorn <- NA
  data$c_firstBorn[data$c_parity==0 & !is.na(data$c_parity)] <- 1
  data$c_firstBorn[data$c_parity>0 & !is.na(data$c_parity)] <- 0
  label(data$c_firstBorn) <- "First born"
  
  data$c_parity <- cut(data$c_parity, breaks=c(0,1,2,10), right=FALSE, labels=c("0","1","2+"))
  label(data$c_parity) <- "Parity"
  
  # Prepregnancy BMI
  summary(data$v17_BMI_before)
  data$c_preBMIcont <- data$v17_BMI_before
  label(data$c_preBMIcont) <- "Prepregnancy BMI"
  data$c_preBMI <- data$v17_BMI_before
  data$c_preBMI <- cut(data$c_preBMI, breaks=c(0,18.5, 25, 35, 100), right=FALSE, labels=c("<18.50", "18.50-24.99","25.00-34.99","35.00+"))
  label(data$c_preBMI) <- "Prepregnancy BMI"
  data$c_preBMIOrdered <- factor(data$c_preBMI, levels=c("18.50-24.99","25.00-34.99","35.00+","<18.50"))
  label(data$c_preBMIOrdered) <- "Prepregnancy BMI"
  data$c_abnormalBMI <- data$c_preBMI
  levels(data$c_abnormalBMI) <- c("1","0","1","1")
  data$c_abnormalBMI <- as.numeric(as.character(data$c_abnormalBMI))
  label(data$c_abnormalBMI) <- "Prepregnancy BMI outside of 18.50-24.99"
  

  # Education
  table(data$v17_education_R, exclude=NULL)
  
  recode_c_educ <- data.frame(
    "v17_education_R" = c(
      "University/College/Folk high school",
      "Primary/Secondary school"
    ),
    "c_educ" = c(
      1,
      0
    )
  )
  
  label(recode_c_educ$c_educ) <- c("More than high school education")
  recode_c_educ
  
  table(data$v17_education_R, exclude=NULL)
  dim(data)
  data <- merge(data,recode_c_educ, by="v17_education_R", all.x=TRUE)
  dim(data)
  table(data$v17_education_R, exclude=NULL)
  table(data$c_educ, exclude=NULL)
  
  # participation type
  table(data$Participation_type, exclude=NULL)
  
  recode_c_participation <- data.frame(
    "Participation_type" = c(
      "pregnant",
      "pregnant&pp",
      "pp",
      "CS",
      "CS&pp",
      "pregnant&CS"
    ),
    "c_participationType" = c(
      0,
      1,
      2,
      3,
      4,
      5
    )
  )
  
  label(recode_c_participation$c_participationType) <- c("Participation type")
  recode_c_participation$c_participationType <- factor(recode_c_participation$c_participationType)
  levels(recode_c_participation$c_participationType) <- c("Preg", "Preg+PP", "PP", "CS", "PP+CS", "Preg+CS")
  recode_c_participation
  
  table(data$Participation_type, exclude=NULL)
  dim(data)
  data <- merge(data,recode_c_participation, by="Participation_type", all.x=TRUE)
  dim(data)
  table(data$Participation_type, exclude=NULL)
  table(data$c_participationType, exclude=NULL)
  
  # medicines pp
  #data$Medicines_same_day_pp_gr

  data$c_medPPNSAID <- NA
  data$c_medPPNSAID[!is.na(data$Medicines_same_day_pp_gr)] <- FALSE
  data$c_medPPNSAID[grep("2",data$Medicines_same_day_pp_gr)] <- TRUE
  label(data$c_medPPNSAID) <- "Post-partum NSAID"
  
  data$c_medPPAntibiotics <- NA
  data$c_medPPAntibiotics[!is.na(data$Medicines_same_day_pp_gr)] <- FALSE
  data$c_medPPAntibiotics[grep("6",data$Medicines_same_day_pp_gr)] <- TRUE
  label(data$c_medPPAntibiotics) <- "Post-partum antibiotics"

  data$c_medPPParacetamol <- NA
  data$c_medPPParacetamol[!is.na(data$Medicines_same_day_pp_gr)] <- FALSE
  data$c_medPPParacetamol[grep("8",data$Medicines_same_day_pp_gr)] <- TRUE
  label(data$c_medPPParacetamol) <- "Post-partum paracetamol"
  
  #Medicines_same_day_pregnancy_gr
  levels(data$Medicines_same_day_pregnancy_gr)
  data$c_medPGNSAID <- NA
  data$c_medPGNSAID[!is.na(data$Medicines_same_day_pregnancy_gr)] <- FALSE
  data$c_medPGNSAID[grep("NSAID",data$Medicines_same_day_pregnancy_gr)] <- TRUE
  label(data$c_medPGNSAID) <- "Pregnancy NSAID"
  
  data$c_medPGAntibiotics <- NA
  data$c_medPGAntibiotics[!is.na(data$Medicines_same_day_pregnancy_gr)] <- FALSE
  data$c_medPGAntibiotics[grep("antibiotics",data$Medicines_same_day_pregnancy_gr)] <- TRUE
  label(data$c_medPGAntibiotics) <- "Pregnancy antibiotics"
  
  data$c_medPGParacetamol <- NA
  data$c_medPGParacetamol[!is.na(data$Medicines_same_day_pregnancy_gr)] <- FALSE
  data$c_medPGParacetamol[grep("paracetamol",data$Medicines_same_day_pregnancy_gr)] <- TRUE
  label(data$c_medPGParacetamol) <- "Pregnancy paracetamol"
  
  # chronic diseases
  data$c_chronicInflam <- NA
  data$c_chronicInflam[!is.na(data$Chronic_disease)] <- FALSE
  data$c_chronicInflam[grep("1",data$Chronic_disease)] <- TRUE
  label(data$c_chronicInflam) <- "Chronic inflammatory disease"
    
  data$c_chronicRheum <- NA
  data$c_chronicRheum[!is.na(data$Chronic_disease)] <- FALSE
  data$c_chronicRheum[grep("2",data$Chronic_disease)] <- TRUE
  label(data$c_chronicRheum) <- "Chronic rheumatic disease"
  
  data$c_chronicAsthma <- NA
  data$c_chronicAsthma[!is.na(data$Chronic_disease)] <- FALSE
  data$c_chronicAsthma[grep("3",data$Chronic_disease)] <- TRUE
  label(data$c_chronicAsthma) <- "Chronic asthma"
  
  data$c_chronicOther <- NA
  data$c_chronicOther[!is.na(data$Chronic_disease)] <- FALSE
  data$c_chronicOther[grep("4",data$Chronic_disease)] <- TRUE
  label(data$c_chronicOther) <- "Chronic other disease"
  
  data$c_chronicInflamOrRheum <- data$c_chronicInflam | data$c_chronicRheum
  label(data$c_chronicInflamOrRheum) <- "Chronic inflammatory or rheumatic disease"
  
  unique(data$Chronic_disease)
  
  # breastfeeding
  unique(data$ppv6_breastfeeding)
  data$c_breastfeeding <- NA
  data$c_breastfeeding[!is.na(data$ppv6_breastfeeding)] <- "Formula only"
  data$c_breastfeeding[grep("Breastfeed and bottle-feed",data$ppv6_breastfeeding)] <- "Partial"
  data$c_breastfeeding[grep("Yes",data$ppv6_breastfeeding)] <- "Breastfeed only"
  data$c_breastfeeding <- factor(data$c_breastfeeding, levels=c(
    "Breastfeed only",
    "Partial",
    "Formula only"
  ))
  label(data$c_breastfeeding) <- "Breastfeeding"
  
  data$c_formulaOnly <- NA
  data$c_formulaOnly[!is.na(data$c_breastfeeding)] <- FALSE
  data$c_formulaOnly[!is.na(data$c_breastfeeding) & data$c_breastfeeding=="Formula only"] <- TRUE
  label(data$c_formulaOnly) <- "Formula only"
  
  
  data$c_breastfeedingOnly <- NA
  data$c_breastfeedingOnly[!is.na(data$c_breastfeeding)] <- FALSE
  data$c_breastfeedingOnly[!is.na(data$c_breastfeeding) & data$c_breastfeeding=="Breastfeed only"] <- TRUE
  label(data$c_breastfeedingOnly) <- "Breastfeeding only"
  
  # earlier depression
  unique(data$MD_earlier)
  data$c_earlierDepression <- NA
  data$c_earlierDepression[!is.na(data$MD_earlier)] <- FALSE
  data$c_earlierDepression[!is.na(data$c_earlierDepression) & data$MD_earlier=="Major depression erlier in life"] <- TRUE
  label(data$c_earlierDepression) <- "Depression earlier in life"
  xtabs(~data$MD_earlier)
  xtabs(~data$c_earlierDepression)
  
  # caesarean
  unique(data$Delivery_mode_2gr)
  data$c_caesarean <- NA
  data$c_caesarean[!is.na(data$Delivery_mode_2gr)] <- FALSE
  data$c_caesarean[!is.na(data$c_caesarean) & data$Delivery_mode_2gr=="CS"] <- TRUE
  label(data$c_caesarean) <- "Caesarean"
  xtabs(~data$Delivery_mode_2gr)
  xtabs(~data$c_caesarean)
  
  # SSRI
  unique(data$SSRI)
  data$c_ssriPG <- NA
  data$c_ssriPG[!is.na(data$SSRI)] <- FALSE
  data$c_ssriPG[!is.na(data$SSRI) & data$SSRI=="use in pregnancy"] <- TRUE
  data$c_ssriPG[!is.na(data$SSRI) & data$SSRI=="use in pregnancy and pp"] <- TRUE
  label(data$c_ssriPG) <- "SSRI in pregnancy"
  
  data$c_ssriPP <- NA
  data$c_ssriPP[!is.na(data$SSRI)] <- FALSE
  data$c_ssriPP[!is.na(data$SSRI) & data$SSRI=="use in pp"] <- TRUE
  data$c_ssriPP[!is.na(data$SSRI) & data$SSRI=="use in pregnancy and pp"] <- TRUE
  label(data$c_ssriPP) <- "SSRI post-partum"
  
  # INFLMATTION MARKERS
  
  dataNamesRaw <- c("Outcome_new")
  dataNamesOutcome <- names(data)[stringr::str_detect(names(data),"^o_")]
  dataNamesCovariates <- names(data)[stringr::str_detect(names(data),"^c_")]
  dataNamesIMPG <- names(data)[stringr::str_detect(names(data),"^im_[0-9a-zA-Z_]*")]
  dataNamesIMPP <- names(data)[stringr::str_detect(names(data),"^im_[0-9a-zA-Z_]*_pp")]
  dataNamesIMPG <- dataNamesIMPG[!dataNamesIMPG %in% dataNamesIMPP]
  
  #file <- system.file("extdata","lod.txt",package="inflammation")
  file <- file.path(RPROJ$PROJRAW,"lod.txt")
  lod <- as.data.frame(fread(file))
  lod$lod <- as.numeric(str_replace_all(lod$lod, ",", "."))
  lod$imNum <- str_extract(lod$factor, "^[0-9][0-9][0-9]")
  lod <- lod[,-1]
  lod$lod <- exp(lod$lod)
  
  dataDemographic <- data[,c("CustomDataR",dataNamesOutcome,dataNamesCovariates)]
  dataRaw <- data[,c("CustomDataR",dataNamesRaw)]
  
  dataIMPG <- data[,c("CustomDataR","Participation_type",dataNamesIMPG)]
  for(i in 3:ncol(dataIMPG)){
    dataIMPG[dataIMPG$Participation_type %in% c(
      "pregnant&pp",
      "CS&pp",
      "pregnant",
      "CS"
    ) & is.na(dataIMPG[,i]),i] <- -99
    
    # missing samples
    dataIMPG[dataIMPG$CustomDataR %in% c(
      2690,
      3770
    ),i] <- NA
  }
  dataIMPG <- dataIMPG[,-2]
  
  dataIMPG %<>% gather(key=im, value=val, -CustomDataR)
  index <- dataIMPG$val!=-99
  index[is.na(index)] <- FALSE
  dataIMPG$val[index] <- exp(dataIMPG$val[index])
  dataIMPG$imNum <- str_extract(dataIMPG$im, "[0-9][0-9][0-9]")
  
  dataIMPG$val[
    dataIMPG$im %in% c(
      "im_152_PDL1",
      "im_175_ENRAGE"
    ) &
    dataIMPG$CustomDataR %in% c(
      631,
      4686,
      4756,
      4791,
      4635,
      4867,
      4709, 
      5449,
      5984,
      5757,
      5961,
      6009,
      6051,
      6005,
      6070,
      6087,
      6067,
      6128,
      6013,
      6134,
      5152,
      6135
    )
  ] <- NA
  dataIMPG <- dataIMPG[!dataIMPG$im %in% c("im_152_PDL1","im_175_ENRAGE"),]
  
  dim(dataIMPG)
  dataIMPG <- merge(dataIMPG, lod, by="imNum")
  dim(dataIMPG)
  
  dataBinIMPG <- dataIMPG
  dataBinIMPG$val[dataBinIMPG$val>0 & !is.na(dataBinIMPG$val)] <- 1
  dataBinIMPG$val[dataBinIMPG$val==-99 & !is.na(dataBinIMPG$val)] <- 0
  summaryIMPG <- as.data.table(dataBinIMPG)
  summaryIMPG <- summaryIMPG[,list(perc=mean(val,na.rm=T),lod=mean(lod,na.rm=T)),by=list(im,imNum)]
  dataNamesConIMPG <- as.character(summaryIMPG[perc>0.5]$im)
  dataNamesBinIMPG <- as.character(summaryIMPG[perc>0.05 & perc<0.95]$im)
  
  dataBinIMPG$im <- paste0("b_",dataBinIMPG$im)
  dataBinIMPG <- dataBinIMPG[,c("CustomDataR","im","val")] %>%
    spread(key=im, value=val)
  
  dataBinIMPG$hasBinIMPG <- FALSE
  for(i in 2:(ncol(dataBinIMPG)-1)){
    dataBinIMPG$hasBinIMPG[!is.na(dataBinIMPG[,i])] <- TRUE
  }
  table(dataBinIMPG$hasBinIMPG)
  
  index <- dataIMPG$val==-99 & !is.na(dataIMPG$val)
  dataIMPG$val[index] <- dataIMPG$lod[index]/sqrt(2)
  summaryConIMPG <- data.table(dataIMPG)
  summaryConIMPG <- summaryConIMPG[,list(
    p25=quantile(val,probs=c(0.25),na.rm=T),
    p50=quantile(val,probs=c(0.5),na.rm=T),
    p75=quantile(val,probs=c(0.75),na.rm=T)),by=list(imNum)]
  
  dataConIMPG <- dataIMPG[dataIMPG$im %in% dataNamesConIMPG,]
  dataLog2IMPG <-  dataIMPG[dataIMPG$im %in% dataNamesConIMPG,]
  
  dataConIMPG$im <- paste0("c_",dataConIMPG$im)
  dataConIMPG <- dataConIMPG[,c("CustomDataR","im","val")] %>%
    spread(key=im, value=val)
  
  dataConIMPG$hasConIMPG <- FALSE
  for(i in 2:(ncol(dataConIMPG)-1)){
    dataConIMPG$hasConIMPG[!is.na(dataConIMPG[,i])] <- TRUE
  }
  table(dataConIMPG$hasConIMPG)
  
  dataLog2IMPG$im <- paste0("l_",dataLog2IMPG$im)
  dataLog2IMPG$val <- log2(dataLog2IMPG$val+1)
  dataLog2IMPG <- dataLog2IMPG[,c("CustomDataR","im","val")] %>%
    spread(key=im, value=val)
  
  dataLog2IMPG$nonaLog2IMPG <- TRUE
  dataLog2IMPG$hasLog2IMPG <- FALSE
  for(i in 2:(ncol(dataLog2IMPG)-2)){
    dataLog2IMPG$nonaLog2IMPG[is.na(dataLog2IMPG[,i])] <- FALSE
    dataLog2IMPG$hasLog2IMPG[!is.na(dataLog2IMPG[,i])] <- TRUE
  }
  table(dataLog2IMPG$hasLog2IMPG)
  
  #inflamation marks postpartum
  dataIMPP <- data[,c("CustomDataR","Participation_type",dataNamesIMPP)]
  for(i in 3:ncol(dataIMPP)){
    dataIMPP[dataIMPP$Participation_type %in% c(
      "pregnant&pp",
      "CS&pp",
      "pp"
    ) & is.na(dataIMPP[,i]),i] <- -99
    
    # missing samples
    dataIMPP[dataIMPP$CustomDataR %in% c(
            154,
             7616,
            7726,
             10318,
             7594,
             8751,
            7175
             ),i] <- NA
  }
  dataIMPP <- dataIMPP[,-2]
  
  dataIMPP %<>% gather(key=im, value=val, -CustomDataR)
  index <- dataIMPP$val!=-99
  index[is.na(index)] <- FALSE
  dataIMPP$val[index] <- exp(dataIMPP$val[index])
  dataIMPP$imNum <- str_extract(dataIMPP$im, "[0-9][0-9][0-9]")
  
  dim(dataIMPP)
  dataIMPP <- merge(dataIMPP, lod, by="imNum")
  dim(dataIMPP)
  
  dataBinIMPP <- dataIMPP
  dataBinIMPP$val[dataBinIMPP$val>0 & !is.na(dataBinIMPP$val)] <- 1
  dataBinIMPP$val[dataBinIMPP$val==-99 & !is.na(dataBinIMPP$val)] <- 0
  summaryIMPP <- data.table(dataBinIMPP)
  summaryIMPP <- summaryIMPP[,list(perc=mean(val,na.rm=T), lod=mean(lod,na.rm=T)),by=list(im,imNum)]
  dataNamesConIMPP <- as.character(summaryIMPP[perc>0.5]$im)
  dataNamesBinIMPP <- as.character(summaryIMPP[perc>0.05 & perc<0.95]$im)
  
  dataBinIMPP$im <- paste0("b_",dataBinIMPP$im)
  dataBinIMPP <- dataBinIMPP[,c("CustomDataR","im","val")] %>%
    spread(key=im, value=val)
  
  dataBinIMPP$hasBinIMPP <- FALSE
  for(i in 2:(ncol(dataBinIMPP)-1)){
    dataBinIMPP$hasBinIMPP[!is.na(dataBinIMPP[,i])] <- TRUE
  }
  table(dataBinIMPP$hasBinIMPP)
  
  index <- dataIMPP$val==-99 & !is.na(dataIMPP$val)
  dataIMPP$val[index] <- dataIMPP$lod[index]/sqrt(2)
  summaryConIMPP <- data.table(dataIMPP)
  summaryConIMPP <- summaryConIMPP[,list(
    p25=quantile(val,probs=c(0.25),na.rm=T),
    p50=quantile(val,probs=c(0.5),na.rm=T),
    p75=quantile(val,probs=c(0.75),na.rm=T)),by=list(imNum)]
  
  dataConIMPP <- dataIMPP[dataIMPP$im %in% dataNamesConIMPP,]
  dataLog2IMPP <- dataIMPP[dataIMPP$im %in% dataNamesConIMPP,]
  
  dataConIMPP$im <- paste0("c_",dataConIMPP$im)
  dataConIMPP <- dataConIMPP[,c("CustomDataR","im","val")] %>%
    spread(key=im, value=val)
  
  dataConIMPP$hasConIMPP <- FALSE
  for(i in 2:(ncol(dataConIMPP)-1)){
    dataConIMPP$hasConIMPP[!is.na(dataConIMPP[,i])] <- TRUE
  }
  table(dataConIMPP$hasConIMPP)
  
  dataLog2IMPP$im <- paste0("l_",dataLog2IMPP$im)
  dataLog2IMPP$val <- log2(dataLog2IMPP$val+1)
  dataLog2IMPP <- dataLog2IMPP[,c("CustomDataR","im","val")] %>%
    spread(key=im, value=val)
  
  dataLog2IMPP$nonaLog2IMPP <- TRUE
  dataLog2IMPP$hasLog2IMPP <- FALSE
  for(i in 2:(ncol(dataLog2IMPP)-1)){
    dataLog2IMPP$nonaLog2IMPP[is.na(dataLog2IMPP[,i])] <- FALSE
    dataLog2IMPP$hasLog2IMPP[!is.na(dataLog2IMPP[,i])] <- TRUE
  }
  table(dataLog2IMPP$hasLog2IMPP)
  
  #################
  #calculating summary IF measures
  
  zLog2IMPG <- dataLog2IMPG[,c("CustomDataR",paste0("l_",dataNamesConIMPG))]
  for(i in paste0("l_",dataNamesConIMPG)){
    zLog2IMPG[,i] <- (zLog2IMPG[,i]-mean(zLog2IMPG[dataDemographic$o_pp_sensitive_1=="Control",i],na.rm=T))/sd(zLog2IMPG[dataDemographic$o_pp_sensitive_1=="Control",i],na.rm=T)
  }
  
  meanZPG <- apply(zLog2IMPG[,-1],1,function(x) mean(x,na.rm=T))
  meanZPG[is.nan(meanZPG)] <- NA
  meanZPG <- (meanZPG-mean(meanZPG[dataDemographic$o_pp_sensitive_1=="Control"],na.rm=T))/sd(meanZPG[dataDemographic$o_pp_sensitive_1=="Control"],na.rm=T)
  sdZPG <- apply(zLog2IMPG[,-1],1,function(x) sd(x,na.rm=T))
  #
  zLog2IMPP <- dataLog2IMPP[,c("CustomDataR",paste0("l_",dataNamesConIMPP))]
  for(i in paste0("l_",dataNamesConIMPP)){
    zLog2IMPP[,i] <- (zLog2IMPP[,i]-mean(zLog2IMPP[dataDemographic$o_pp_sensitive_1=="Control",i],na.rm=T))/sd(zLog2IMPP[dataDemographic$o_pp_sensitive_1=="Control",i],na.rm=T)
  }
  
  meanZPP <- apply(zLog2IMPP[,-1],1,function(x) mean(x,na.rm=T))
  meanZPP[is.nan(meanZPP)] <- NA
  meanZPP <- (meanZPP-mean(meanZPP[dataDemographic$o_pp_sensitive_1=="Control"],na.rm=T))/sd(meanZPP[dataDemographic$o_pp_sensitive_1=="Control"],na.rm=T)
  sdZPP <- apply(zLog2IMPP[,-1],1,function(x) sd(x,na.rm=T))
  
  meanZdiff <- meanZPP-meanZPG
  meanZdiff <- (meanZdiff-mean(meanZdiff,na.rm=T))/sd(meanZdiff,na.rm=T)
  
  zscores <- data.frame(
    CustomDataR=dataLog2IMPG$CustomDataR,
    meanZPG,
    sdZPG,
    meanZPP,
    sdZPP,
    meanZdiff
  )
  
  label(zscores$meanZPG) <- c("Z-score of mean PG IF Z-scores")
  label(zscores$meanZPP) <- c("Z-score of mean PP IF Z-scores")
  label(zscores$meanZdiff) <- c("Z-score of PP minus PG")
  
  #################
  
  #generating up and down groupings
  keep <- dataDemographic$o_pp_sensitive_1=="Control"
  keep[is.na(keep)] <- FALSE
  pg <- dataLog2IMPG[keep,c("CustomDataR",paste0("l_",dataNamesConIMPG))]
  pp <- dataLog2IMPP[keep,c("CustomDataR",paste0("l_",dataNamesConIMPP))]
  pg %<>% gather(key=im, value=PG, -CustomDataR)
  pp %<>% gather(key=im, value=PP, -CustomDataR)
  pg$im <- gsub("l_im_","",pg$im)
  pp$im <- gsub("l_im_","",pp$im)
  pp$im <- gsub("_pp","",pp$im)
  res <- merge(pg,pp,by=c("CustomDataR","im"))
  dim(res)
  dim(pg)
  dim(pp)
  res <- data.table(res)
  
  res[,PPminusPG:=PP-PG]
  res <- res[,.(PPminusPGpval=t.test(PPminusPG)$p.value,PPminusPGest=t.test(PPminusPG)$estimate),by=.(im)]
  res[,PPminusPGpval:=PPminusPGpval*nrow(res)]
  res <- res[PPminusPGpval<0.05]
 
  #res <- reshape2::melt(res,id=c("CustomDataR","im"))
  downIF <- res[PPminusPGest<0]$im
  upIF <- res[PPminusPGest>0]$im
    
  zLog2downIMPG <- dataLog2IMPG[,c("CustomDataR",paste0("l_im_",downIF))]
  for(i in paste0("l_im_",downIF)){
    zLog2downIMPG[,i] <- (zLog2downIMPG[,i]-mean(zLog2downIMPG[dataDemographic$o_pp_sensitive_1=="Control",i],na.rm=T))/sd(zLog2downIMPG[dataDemographic$o_pp_sensitive_1=="Control",i],na.rm=T)
  }
  
  meanZdownPG <- apply(zLog2downIMPG[,-1],1,function(x) mean(x,na.rm=T))
  meanZdownPG[is.nan(meanZdownPG)] <- NA
  meanZdownPG <- (meanZdownPG-mean(meanZdownPG[dataDemographic$o_pp_sensitive_1=="Control"],na.rm=T))/sd(meanZdownPG[dataDemographic$o_pp_sensitive_1=="Control"],na.rm=T)
  #
  zLog2upIMPG <- dataLog2IMPG[,c("CustomDataR",paste0("l_im_",upIF))]
  for(i in paste0("l_im_",upIF)){
    zLog2upIMPG[,i] <- (zLog2upIMPG[,i]-mean(zLog2upIMPG[dataDemographic$o_pp_sensitive_1=="Control",i],na.rm=T))/sd(zLog2upIMPG[dataDemographic$o_pp_sensitive_1=="Control",i],na.rm=T)
  }
  
  meanZupPG <- apply(zLog2upIMPG[,-1],1,function(x) mean(x,na.rm=T))
  meanZupPG[is.nan(meanZupPG)] <- NA
  meanZupPG <- (meanZupPG-mean(meanZupPG[dataDemographic$o_pp_sensitive_1=="Control"],na.rm=T))/sd(meanZupPG[dataDemographic$o_pp_sensitive_1=="Control"],na.rm=T)
  
  ######
  zLogdown2IMPP <- dataLog2IMPP[,c("CustomDataR",paste0("l_im_",downIF,"_pp"))]
  for(i in paste0("l_im_",downIF,"_pp")){
    zLogdown2IMPP[,i] <- (zLogdown2IMPP[,i]-mean(zLogdown2IMPP[dataDemographic$o_pp_sensitive_1=="Control",i],na.rm=T))/sd(zLogdown2IMPP[dataDemographic$o_pp_sensitive_1=="Control",i],na.rm=T)
  }
  
  meanZdownPP <- apply(zLogdown2IMPP[,-1],1,function(x) mean(x,na.rm=T))
  meanZdownPP[is.nan(meanZdownPP)] <- NA
  meanZdownPP <- (meanZdownPP-mean(meanZdownPP[dataDemographic$o_pp_sensitive_1=="Control"],na.rm=T))/sd(meanZdownPP[dataDemographic$o_pp_sensitive_1=="Control"],na.rm=T)
  #
  zLogup2IMPP <- dataLog2IMPP[,c("CustomDataR",paste0("l_im_",upIF,"_pp"))]
  for(i in paste0("l_im_",upIF,"_pp")){
    zLogup2IMPP[,i] <- (zLogup2IMPP[,i]-mean(zLogup2IMPP[dataDemographic$o_pp_sensitive_1=="Control",i],na.rm=T))/sd(zLogup2IMPP[dataDemographic$o_pp_sensitive_1=="Control",i],na.rm=T)
  }
  
  meanZupPP <- apply(zLogup2IMPP[,-1],1,function(x) mean(x,na.rm=T))
  meanZdownPP[is.nan(meanZupPP)] <- NA
  meanZupPP <- (meanZupPP-mean(meanZupPP[dataDemographic$o_pp_sensitive_1=="Control"],na.rm=T))/sd(meanZupPP[dataDemographic$o_pp_sensitive_1=="Control"],na.rm=T)
  
  zscores <- data.frame(zscores,meanZdownPG,meanZdownPP,meanZupPG,meanZupPP)
  label(zscores$meanZdownPG) <- c("Z-score of mean (down) PG IF Z-scores")
  label(zscores$meanZdownPP) <- c("Z-score of mean (down) PP IF Z-scores")
  label(zscores$meanZupPG) <- c("Z-score of mean (up) PG IF Z-scores")
  label(zscores$meanZupPP) <- c("Z-score of mean (up) PP IF Z-scores")
  
  
  #### Z SCORES WITH PPvalues CONTROLS AS REFERENCE
  ######
  zLog2downIMPG <- dataLog2IMPG[,c("CustomDataR",paste0("l_im_",downIF))]
  zLog2upIMPG <- dataLog2IMPG[,c("CustomDataR",paste0("l_im_",upIF))]
  zLog2downIMPP <- dataLog2IMPP[,c("CustomDataR",paste0("l_im_",downIF,"_pp"))]
  zLog2upIMPP <- dataLog2IMPP[,c("CustomDataR",paste0("l_im_",upIF,"_pp"))]
  
  for(i in paste0("l_im_",downIF)){
    zLog2downIMPG[,i] <- (zLog2downIMPG[,i]-mean(zLog2downIMPP[dataDemographic$o_pp_sensitive_1=="Control",paste0(i,"_pp")],na.rm=T))/sd(zLog2downIMPP[dataDemographic$o_pp_sensitive_1=="Control",paste0(i,"_pp")],na.rm=T)
  }
  for(i in paste0("l_im_",upIF)){
    zLog2upIMPG[,i] <- (zLog2upIMPG[,i]-mean(zLog2upIMPP[dataDemographic$o_pp_sensitive_1=="Control",paste0(i,"_pp")],na.rm=T))/sd(zLog2upIMPP[dataDemographic$o_pp_sensitive_1=="Control",paste0(i,"_pp")],na.rm=T)
  }
  for(i in paste0("l_im_",downIF,"_pp")){
    zLog2downIMPP[,i] <- (zLog2downIMPP[,i]-mean(zLog2downIMPP[dataDemographic$o_pp_sensitive_1=="Control",i],na.rm=T))/sd(zLog2downIMPP[dataDemographic$o_pp_sensitive_1=="Control",i],na.rm=T)
  }
  for(i in paste0("l_im_",upIF,"_pp")){
    zLog2upIMPP[,i] <- (zLog2upIMPP[,i]-mean(zLog2upIMPP[dataDemographic$o_pp_sensitive_1=="Control",i],na.rm=T))/sd(zLog2upIMPP[dataDemographic$o_pp_sensitive_1=="Control",i],na.rm=T)
  }
  
  meanZdownPG <- apply(zLog2downIMPG[,-1],1,function(x) mean(x,na.rm=T))
  meanZdownPG[is.nan(meanZdownPG)] <- NA
  meanZupPG <- apply(zLog2upIMPG[,-1],1,function(x) mean(x,na.rm=T))
  meanZupPG[is.nan(meanZupPG)] <- NA
  meanZdownPP <- apply(zLog2downIMPP[,-1],1,function(x) mean(x,na.rm=T))
  meanZdownPP[is.nan(meanZdownPP)] <- NA
  meanZupPP <- apply(zLog2upIMPP[,-1],1,function(x) mean(x,na.rm=T))
  meanZdownPP[is.nan(meanZupPP)] <- NA
  
  PPREFmeanZdownPG <- (meanZdownPG-mean(meanZdownPP[dataDemographic$o_pp_sensitive_1=="Control"],na.rm=T))/sd(meanZdownPP[dataDemographic$o_pp_sensitive_1=="Control"],na.rm=T)
  PPREFmeanZupPG <- (meanZupPG-mean(meanZupPP[dataDemographic$o_pp_sensitive_1=="Control"],na.rm=T))/sd(meanZupPP[dataDemographic$o_pp_sensitive_1=="Control"],na.rm=T)
  PPREFmeanZdownPP <- (meanZdownPP-mean(meanZdownPP[dataDemographic$o_pp_sensitive_1=="Control"],na.rm=T))/sd(meanZdownPP[dataDemographic$o_pp_sensitive_1=="Control"],na.rm=T)
  PPREFmeanZupPP <- (meanZupPP-mean(meanZupPP[dataDemographic$o_pp_sensitive_1=="Control"],na.rm=T))/sd(meanZupPP[dataDemographic$o_pp_sensitive_1=="Control"],na.rm=T)
  
  PPREFzscores <- data.frame(CustomDataR=dataLog2IMPG$CustomDataR,PPREFmeanZdownPG,PPREFmeanZdownPP,PPREFmeanZupPG,PPREFmeanZupPP)
  label(PPREFzscores$PPREFmeanZdownPG) <- c("Z-score of mean (down) PG IF Z-scores using PP as reference")
  label(PPREFzscores$PPREFmeanZdownPP) <- c("Z-score of mean (down) PP IF Z-scores using PP as reference")
  label(PPREFzscores$PPREFmeanZupPG) <- c("Z-score of mean (up) PG IF Z-scores using PP as reference")
  label(PPREFzscores$PPREFmeanZupPP) <- c("Z-score of mean (up) PP IF Z-scores using PP as reference")
  
  #################
  dim(dataDemographic)
  mergedData <- merge(dataDemographic, dataConIMPG, by="CustomDataR")
  dim(mergedData)
  mergedData <- merge(mergedData, dataRaw, by="CustomDataR")
  dim(mergedData)
  mergedData <- merge(mergedData, dataConIMPP, by="CustomDataR")
  dim(mergedData)
  mergedData <- merge(mergedData, dataLog2IMPG, by="CustomDataR")
  dim(mergedData)
  mergedData <- merge(mergedData, dataLog2IMPP, by="CustomDataR")
  dim(mergedData)
  mergedData <- merge(mergedData, dataBinIMPG, by="CustomDataR")
  dim(mergedData)
  mergedData <- merge(mergedData, dataBinIMPP, by="CustomDataR")
  dim(mergedData)
  mergedData <- merge(mergedData, zscores, by="CustomDataR")
  dim(mergedData)
  mergedData <- merge(mergedData, PPREFzscores, by="CustomDataR")
  dim(mergedData)
  
  setnames(summaryIMPG,c("PregIM","imNum","PregPerc","PregLOD"))
  setnames(summaryIMPP,c("PPIM","imNum","PPPerc","PPLOD"))
  
  setnames(summaryConIMPG, c("imNum","Pregp25","Pregp50","Pregp75"))
  setnames(summaryConIMPP, c("imNum","PPp25","PPp50","PPp75"))
  
  dim(summaryIMPG)
  summaryIM <- merge(summaryIMPG, summaryIMPP, by="imNum")
  dim(summaryIM)
  summaryIM <- merge(summaryIM, summaryConIMPG, by="imNum")
  dim(summaryIM)
  summaryIM <- merge(summaryIM, summaryConIMPP, by="imNum")
  dim(summaryIM)
  
  # PG
  analysisDAPCIMPG_pp_sensitive_1 <- mergedData$nonaLog2IMPG
  analysisDAPCIMPG_pp_sensitive_1[is.na(mergedData$o_pp_sensitive_1)] <- FALSE
  analysisDAPCIMPG_pp_sensitive_1[mergedData$c_smokerPG==1] <- FALSE
  analysisDAPCIMPG_pp_sensitive_1[mergedData$c_twin==1] <- FALSE
  
  analysisDAPCIMPG_pp_sensitive_2 <- mergedData$nonaLog2IMPG
  analysisDAPCIMPG_pp_sensitive_2[is.na(mergedData$o_pp_sensitive_2)] <- FALSE
  analysisDAPCIMPG_pp_sensitive_2[mergedData$c_smokerPG==1] <- FALSE
  analysisDAPCIMPG_pp_sensitive_2[mergedData$c_twin==1] <- FALSE
  
  analysisDAPCIMPG_pp_sensitive_3 <- mergedData$nonaLog2IMPG
  analysisDAPCIMPG_pp_sensitive_3[is.na(mergedData$o_pp_sensitive_3)] <- FALSE
  analysisDAPCIMPG_pp_sensitive_3[mergedData$c_smokerPG==1] <- FALSE
  analysisDAPCIMPG_pp_sensitive_3[mergedData$c_twin==1] <- FALSE
  
  analysisDAPCIMPG_pp_sensitive_4 <- mergedData$nonaLog2IMPG
  analysisDAPCIMPG_pp_sensitive_4[is.na(mergedData$o_pp_sensitive_4)] <- FALSE
  analysisDAPCIMPG_pp_sensitive_4[mergedData$c_smokerPG==1] <- FALSE
  analysisDAPCIMPG_pp_sensitive_4[mergedData$c_twin==1] <- FALSE
  
  analysisDAPCIMPG_pp_sensitive_5 <- mergedData$nonaLog2IMPG
  analysisDAPCIMPG_pp_sensitive_5[is.na(mergedData$o_pp_sensitive_5)] <- FALSE
  analysisDAPCIMPG_pp_sensitive_5[mergedData$c_smokerPG==1] <- FALSE
  analysisDAPCIMPG_pp_sensitive_5[mergedData$c_twin==1] <- FALSE
  
  analysisDAPCIMPG_pp_specific_1 <- mergedData$nonaLog2IMPG
  analysisDAPCIMPG_pp_specific_1[is.na(mergedData$o_pp_specific_1)] <- FALSE
  analysisDAPCIMPG_pp_specific_1[mergedData$c_smokerPG==1] <- FALSE
  analysisDAPCIMPG_pp_specific_1[mergedData$c_twin==1] <- FALSE
  
  analysisDAPCIMPG_pp_specific_2 <- mergedData$nonaLog2IMPG
  analysisDAPCIMPG_pp_specific_2[is.na(mergedData$o_pp_specific_2)] <- FALSE
  analysisDAPCIMPG_pp_specific_2[mergedData$c_smokerPG==1] <- FALSE
  analysisDAPCIMPG_pp_specific_2[mergedData$c_twin==1] <- FALSE
  
  analysisDAPCIMPG_pp_specific_3 <- mergedData$nonaLog2IMPG
  analysisDAPCIMPG_pp_specific_3[is.na(mergedData$o_pp_specific_3)] <- FALSE
  analysisDAPCIMPG_pp_specific_3[mergedData$c_smokerPG==1] <- FALSE
  analysisDAPCIMPG_pp_specific_3[mergedData$c_twin==1] <- FALSE
  
  # PP
  analysisDAPCIMPP_pp_sensitive_1 <- mergedData$nonaLog2IMPP
  analysisDAPCIMPP_pp_sensitive_1[is.na(mergedData$o_pp_sensitive_1)] <- FALSE
  analysisDAPCIMPP_pp_sensitive_1[mergedData$c_smokerPP==1] <- FALSE
  analysisDAPCIMPP_pp_sensitive_1[mergedData$c_twin==1] <- FALSE
  
  analysisDAPCIMPP_pp_sensitive_2 <- mergedData$nonaLog2IMPP
  analysisDAPCIMPP_pp_sensitive_2[is.na(mergedData$o_pp_sensitive_2)] <- FALSE
  analysisDAPCIMPP_pp_sensitive_2[mergedData$c_smokerPP==1] <- FALSE
  analysisDAPCIMPP_pp_sensitive_2[mergedData$c_twin==1] <- FALSE
  
  analysisDAPCIMPP_pp_sensitive_3 <- mergedData$nonaLog2IMPP
  analysisDAPCIMPP_pp_sensitive_3[is.na(mergedData$o_pp_sensitive_3)] <- FALSE
  analysisDAPCIMPP_pp_sensitive_3[mergedData$c_smokerPP==1] <- FALSE
  analysisDAPCIMPP_pp_sensitive_3[mergedData$c_twin==1] <- FALSE
  
  analysisDAPCIMPP_pp_sensitive_4 <- mergedData$nonaLog2IMPP
  analysisDAPCIMPP_pp_sensitive_4[is.na(mergedData$o_pp_sensitive_4)] <- FALSE
  analysisDAPCIMPP_pp_sensitive_4[mergedData$c_smokerPP==1] <- FALSE
  analysisDAPCIMPP_pp_sensitive_4[mergedData$c_twin==1] <- FALSE
  
  analysisDAPCIMPP_pp_sensitive_5 <- mergedData$nonaLog2IMPP
  analysisDAPCIMPP_pp_sensitive_5[is.na(mergedData$o_pp_sensitive_5)] <- FALSE
  analysisDAPCIMPP_pp_sensitive_5[mergedData$c_smokerPP==1] <- FALSE
  analysisDAPCIMPP_pp_sensitive_5[mergedData$c_twin==1] <- FALSE
  
  analysisDAPCIMPP_pp_specific_1 <- mergedData$nonaLog2IMPP
  analysisDAPCIMPP_pp_specific_1[is.na(mergedData$o_pp_specific_1)] <- FALSE
  analysisDAPCIMPP_pp_specific_1[mergedData$c_smokerPP==1] <- FALSE
  analysisDAPCIMPP_pp_specific_1[mergedData$c_twin==1] <- FALSE
  
  analysisDAPCIMPP_pp_specific_2 <- mergedData$nonaLog2IMPP
  analysisDAPCIMPP_pp_specific_2[is.na(mergedData$o_pp_specific_2)] <- FALSE
  analysisDAPCIMPP_pp_specific_2[mergedData$c_smokerPP==1] <- FALSE
  analysisDAPCIMPP_pp_specific_2[mergedData$c_twin==1] <- FALSE
  
  analysisDAPCIMPP_pp_specific_3 <- mergedData$nonaLog2IMPP
  analysisDAPCIMPP_pp_specific_3[is.na(mergedData$o_pp_specific_3)] <- FALSE
  analysisDAPCIMPP_pp_specific_3[mergedData$c_smokerPP==1] <- FALSE
  analysisDAPCIMPP_pp_specific_3[mergedData$c_twin==1] <- FALSE
  
  #summaryIMPG <- summaryIMPG[,list(perc=mean(val,na.rm=T),lod=mean(lod,na.rm=T)),by=list(im)]

  # analyses removing antibiotics and NSAIDS
  for(i in 1:5) for(exp in c("PG","PP")) for(run in 1:10){
    if(run==1){
      var <- paste0("mergedData$c_med",exp,"NSAID==1 | mergedData$c_med",exp,"Antibiotics==1")
      name <- "antiNSAIDS"
    }
    else if(run==2){
      var <- "mergedData$c_smoker==1"
      name <- "smoker"
    }
    else if(run==3){
      var <- "mergedData$c_chronicInflam==1"
      name <- "inflam"
    }
    else if(run==4){
      var <- "mergedData$c_chronicRheum==1"
      name <- "rheum"
    }
    else if(run==5){
      var <- "mergedData$c_chronicAsthma==1"
      name <- "asthma"
    }
    else if(run==6){
      var <- "mergedData$c_chronicOther==1"
      name <- "otherchron"
    }
    else if(run==7){
      var <- "mergedData$c_twin==1"
      name <- "twin"
    }
    else if(run==8){
      var <- "mergedData$c_earlierDepression==1"
      name <- "earlierdep"
    }
    else if(run==9){
      var <- "mergedData$c_caesarean==1"
      name <- "caesarean"
    }
    else if(run==10){
      var <- "mergedData$c_chronicInflamOrRheum==1"
      name <- "inflamOrRheum"
    }
    
    txt <- paste0("analysisDAPCIM",exp,"_pp_sensitive_",i,"_",name,"=analysisDAPCIM",exp,"_pp_sensitive_",i)
    eval(parse(text=txt))
    txt <- paste0("analysisDAPCIM",exp,"_pp_sensitive_",i,"_",name,"[",var,"] <- FALSE")
    eval(parse(text=txt))
  }
 

  modelPG1 <- modelPG2 <- c(
    "c_age",
    "c_abnormalBMI",
    "c_educ",
    "c_chronicInflamOrRheum",
    "c_fastingAtSample",
    "c_ssriPG",
    "c_earlierDepression",
    "c_babyMale",
    "c_lengthPG"
  )
  
  
  modelPP1 <- modelPP2 <- c(
    "c_age",
    "c_abnormalBMI",
    "c_firstBorn",
    "c_educ",
    "c_caesarean",
    "c_chronicInflamOrRheum",
    "c_breastfeedingOnly",
    "c_medPPNSAID",
    "c_ssriPP",
    "c_earlierDepression",
    "c_lengthPP"
  )
  
  # adjusted analysis
  analysisDAPCIMPG_pp_sensitive_1_adjusted=analysisDAPCIMPG_pp_sensitive_1
  for(i in modelPG1){
    analysisDAPCIMPG_pp_sensitive_1_adjusted[is.na(mergedData[,i])] <- FALSE
  }
  analysisDAPCIMPG_pp_sensitive_2_adjusted=analysisDAPCIMPG_pp_sensitive_2
  for(i in modelPG1){
    analysisDAPCIMPG_pp_sensitive_2_adjusted[is.na(mergedData[,i])] <- FALSE
  }
  analysisDAPCIMPG_pp_sensitive_3_adjusted=analysisDAPCIMPG_pp_sensitive_3
  for(i in modelPG1){
    analysisDAPCIMPG_pp_sensitive_3_adjusted[is.na(mergedData[,i])] <- FALSE
  }
  
  analysisDAPCIMPP_pp_sensitive_1_adjusted=analysisDAPCIMPP_pp_sensitive_1
  for(i in modelPP1){
    analysisDAPCIMPP_pp_sensitive_1_adjusted[is.na(mergedData[,i])] <- FALSE
  }
  analysisDAPCIMPP_pp_sensitive_2_adjusted=analysisDAPCIMPP_pp_sensitive_2
  for(i in modelPP1){
    analysisDAPCIMPP_pp_sensitive_2_adjusted[is.na(mergedData[,i])] <- FALSE
  }
  analysisDAPCIMPP_pp_sensitive_3_adjusted=analysisDAPCIMPP_pp_sensitive_3
  for(i in modelPP1){
    analysisDAPCIMPP_pp_sensitive_3_adjusted[is.na(mergedData[,i])] <- FALSE
  }
  

  return(list(data=mergedData,
    modelPG1=modelPG1,
    modelPG2=modelPG2,
    modelPP1=modelPP1,
    modelPP2=modelPP2,
    downIF=downIF,
    upIF=upIF,
    namesRaw=dataNamesRaw,
    namesOutcome=dataNamesOutcome,
    namesCovariates=dataNamesCovariates,
    namesConIMPG=paste0("c_",dataNamesConIMPG),
    namesConIMPP=paste0("c_",dataNamesConIMPP),
    namesLog2IMPG=paste0("l_",dataNamesConIMPG),
    namesLog2IMPP=paste0("l_",dataNamesConIMPP),
    namesBinIMPG=paste0("b_",dataNamesBinIMPG),
    namesBinIMPP=paste0("b_",dataNamesBinIMPP),
    analysisDAPCIMPG_pp_sensitive_1=analysisDAPCIMPG_pp_sensitive_1,
    analysisDAPCIMPG_pp_sensitive_2=analysisDAPCIMPG_pp_sensitive_2,
    analysisDAPCIMPG_pp_sensitive_3=analysisDAPCIMPG_pp_sensitive_3,
    analysisDAPCIMPG_pp_sensitive_4=analysisDAPCIMPG_pp_sensitive_4,
    analysisDAPCIMPG_pp_sensitive_5=analysisDAPCIMPG_pp_sensitive_5,
    analysisDAPCIMPG_pp_specific_1=analysisDAPCIMPG_pp_specific_1,
    analysisDAPCIMPG_pp_specific_2=analysisDAPCIMPG_pp_specific_2,
    analysisDAPCIMPG_pp_specific_3=analysisDAPCIMPG_pp_specific_3,
    analysisDAPCIMPP_pp_sensitive_1=analysisDAPCIMPP_pp_sensitive_1,
    analysisDAPCIMPP_pp_sensitive_2=analysisDAPCIMPP_pp_sensitive_2,
    analysisDAPCIMPP_pp_sensitive_3=analysisDAPCIMPP_pp_sensitive_3,
    analysisDAPCIMPP_pp_sensitive_4=analysisDAPCIMPP_pp_sensitive_4,
    analysisDAPCIMPP_pp_sensitive_5=analysisDAPCIMPP_pp_sensitive_5,
    analysisDAPCIMPP_pp_specific_1=analysisDAPCIMPP_pp_specific_1,
    analysisDAPCIMPP_pp_specific_2=analysisDAPCIMPP_pp_specific_2,
    analysisDAPCIMPP_pp_specific_3=analysisDAPCIMPP_pp_specific_3,
    analysisDAPCIMPG_pp_sensitive_1_antiNSAIDS=analysisDAPCIMPG_pp_sensitive_1_antiNSAIDS,
    analysisDAPCIMPG_pp_sensitive_2_antiNSAIDS=analysisDAPCIMPG_pp_sensitive_2_antiNSAIDS,
    analysisDAPCIMPG_pp_sensitive_3_antiNSAIDS=analysisDAPCIMPG_pp_sensitive_3_antiNSAIDS,
    analysisDAPCIMPG_pp_sensitive_4_antiNSAIDS=analysisDAPCIMPG_pp_sensitive_4_antiNSAIDS,
    analysisDAPCIMPG_pp_sensitive_5_antiNSAIDS=analysisDAPCIMPG_pp_sensitive_5_antiNSAIDS,
    analysisDAPCIMPP_pp_sensitive_1_antiNSAIDS=analysisDAPCIMPP_pp_sensitive_1_antiNSAIDS,
    analysisDAPCIMPP_pp_sensitive_2_antiNSAIDS=analysisDAPCIMPP_pp_sensitive_2_antiNSAIDS,
    analysisDAPCIMPP_pp_sensitive_3_antiNSAIDS=analysisDAPCIMPP_pp_sensitive_3_antiNSAIDS,
    analysisDAPCIMPP_pp_sensitive_4_antiNSAIDS=analysisDAPCIMPP_pp_sensitive_4_antiNSAIDS,
    analysisDAPCIMPP_pp_sensitive_5_antiNSAIDS=analysisDAPCIMPP_pp_sensitive_5_antiNSAIDS,
    analysisDAPCIMPG_pp_sensitive_1_smoker=analysisDAPCIMPG_pp_sensitive_1_smoker,
    analysisDAPCIMPG_pp_sensitive_2_smoker=analysisDAPCIMPG_pp_sensitive_2_smoker,
    analysisDAPCIMPG_pp_sensitive_3_smoker=analysisDAPCIMPG_pp_sensitive_3_smoker,
    analysisDAPCIMPG_pp_sensitive_4_smoker=analysisDAPCIMPG_pp_sensitive_4_smoker,
    analysisDAPCIMPG_pp_sensitive_5_smoker=analysisDAPCIMPG_pp_sensitive_5_smoker,
    analysisDAPCIMPP_pp_sensitive_1_smoker=analysisDAPCIMPP_pp_sensitive_1_smoker,
    analysisDAPCIMPP_pp_sensitive_2_smoker=analysisDAPCIMPP_pp_sensitive_2_smoker,
    analysisDAPCIMPP_pp_sensitive_3_smoker=analysisDAPCIMPP_pp_sensitive_3_smoker,
    analysisDAPCIMPP_pp_sensitive_4_smoker=analysisDAPCIMPP_pp_sensitive_4_smoker,
    analysisDAPCIMPP_pp_sensitive_5_smoker=analysisDAPCIMPP_pp_sensitive_5_smoker,
    analysisDAPCIMPG_pp_sensitive_1_inflam=analysisDAPCIMPG_pp_sensitive_1_inflam,
    analysisDAPCIMPG_pp_sensitive_2_inflam=analysisDAPCIMPG_pp_sensitive_2_inflam,
    analysisDAPCIMPG_pp_sensitive_3_inflam=analysisDAPCIMPG_pp_sensitive_3_inflam,
    analysisDAPCIMPG_pp_sensitive_4_inflam=analysisDAPCIMPG_pp_sensitive_4_inflam,
    analysisDAPCIMPG_pp_sensitive_5_inflam=analysisDAPCIMPG_pp_sensitive_5_inflam,
    analysisDAPCIMPP_pp_sensitive_1_inflam=analysisDAPCIMPP_pp_sensitive_1_inflam,
    analysisDAPCIMPP_pp_sensitive_2_inflam=analysisDAPCIMPP_pp_sensitive_2_inflam,
    analysisDAPCIMPP_pp_sensitive_3_inflam=analysisDAPCIMPP_pp_sensitive_3_inflam,
    analysisDAPCIMPP_pp_sensitive_4_inflam=analysisDAPCIMPP_pp_sensitive_4_inflam,
    analysisDAPCIMPP_pp_sensitive_5_inflam=analysisDAPCIMPP_pp_sensitive_5_inflam,
    analysisDAPCIMPG_pp_sensitive_1_rheum=analysisDAPCIMPG_pp_sensitive_1_rheum,
    analysisDAPCIMPG_pp_sensitive_2_rheum=analysisDAPCIMPG_pp_sensitive_2_rheum,
    analysisDAPCIMPG_pp_sensitive_3_rheum=analysisDAPCIMPG_pp_sensitive_3_rheum,
    analysisDAPCIMPG_pp_sensitive_4_rheum=analysisDAPCIMPG_pp_sensitive_4_rheum,
    analysisDAPCIMPG_pp_sensitive_5_rheum=analysisDAPCIMPG_pp_sensitive_5_rheum,
    analysisDAPCIMPP_pp_sensitive_1_rheum=analysisDAPCIMPP_pp_sensitive_1_rheum,
    analysisDAPCIMPP_pp_sensitive_2_rheum=analysisDAPCIMPP_pp_sensitive_2_rheum,
    analysisDAPCIMPP_pp_sensitive_3_rheum=analysisDAPCIMPP_pp_sensitive_3_rheum,
    analysisDAPCIMPP_pp_sensitive_4_rheum=analysisDAPCIMPP_pp_sensitive_4_rheum,
    analysisDAPCIMPP_pp_sensitive_5_rheum=analysisDAPCIMPP_pp_sensitive_5_rheum,
    analysisDAPCIMPG_pp_sensitive_1_asthma=analysisDAPCIMPG_pp_sensitive_1_asthma,
    analysisDAPCIMPG_pp_sensitive_2_asthma=analysisDAPCIMPG_pp_sensitive_2_asthma,
    analysisDAPCIMPG_pp_sensitive_3_asthma=analysisDAPCIMPG_pp_sensitive_3_asthma,
    analysisDAPCIMPG_pp_sensitive_4_asthma=analysisDAPCIMPG_pp_sensitive_4_asthma,
    analysisDAPCIMPG_pp_sensitive_5_asthma=analysisDAPCIMPG_pp_sensitive_5_asthma,
    analysisDAPCIMPP_pp_sensitive_1_asthma=analysisDAPCIMPP_pp_sensitive_1_asthma,
    analysisDAPCIMPP_pp_sensitive_2_asthma=analysisDAPCIMPP_pp_sensitive_2_asthma,
    analysisDAPCIMPP_pp_sensitive_3_asthma=analysisDAPCIMPP_pp_sensitive_3_asthma,
    analysisDAPCIMPP_pp_sensitive_4_asthma=analysisDAPCIMPP_pp_sensitive_4_asthma,
    analysisDAPCIMPP_pp_sensitive_5_asthma=analysisDAPCIMPP_pp_sensitive_5_asthma,
    analysisDAPCIMPG_pp_sensitive_1_otherchron=analysisDAPCIMPG_pp_sensitive_1_otherchron,
    analysisDAPCIMPG_pp_sensitive_2_otherchron=analysisDAPCIMPG_pp_sensitive_2_otherchron,
    analysisDAPCIMPG_pp_sensitive_3_otherchron=analysisDAPCIMPG_pp_sensitive_3_otherchron,
    analysisDAPCIMPG_pp_sensitive_4_otherchron=analysisDAPCIMPG_pp_sensitive_4_otherchron,
    analysisDAPCIMPG_pp_sensitive_5_otherchron=analysisDAPCIMPG_pp_sensitive_5_otherchron,
    analysisDAPCIMPP_pp_sensitive_1_otherchron=analysisDAPCIMPP_pp_sensitive_1_otherchron,
    analysisDAPCIMPP_pp_sensitive_2_otherchron=analysisDAPCIMPP_pp_sensitive_2_otherchron,
    analysisDAPCIMPP_pp_sensitive_3_otherchron=analysisDAPCIMPP_pp_sensitive_3_otherchron,
    analysisDAPCIMPP_pp_sensitive_4_otherchron=analysisDAPCIMPP_pp_sensitive_4_otherchron,
    analysisDAPCIMPP_pp_sensitive_5_otherchron=analysisDAPCIMPP_pp_sensitive_5_otherchron,
    analysisDAPCIMPG_pp_sensitive_1_twin=analysisDAPCIMPG_pp_sensitive_1_twin,
    analysisDAPCIMPG_pp_sensitive_2_twin=analysisDAPCIMPG_pp_sensitive_2_twin,
    analysisDAPCIMPG_pp_sensitive_3_twin=analysisDAPCIMPG_pp_sensitive_3_twin,
    analysisDAPCIMPG_pp_sensitive_4_twin=analysisDAPCIMPG_pp_sensitive_4_twin,
    analysisDAPCIMPG_pp_sensitive_5_twin=analysisDAPCIMPG_pp_sensitive_5_twin,
    analysisDAPCIMPP_pp_sensitive_1_twin=analysisDAPCIMPP_pp_sensitive_1_twin,
    analysisDAPCIMPP_pp_sensitive_2_twin=analysisDAPCIMPP_pp_sensitive_2_twin,
    analysisDAPCIMPP_pp_sensitive_3_twin=analysisDAPCIMPP_pp_sensitive_3_twin,
    analysisDAPCIMPP_pp_sensitive_4_twin=analysisDAPCIMPP_pp_sensitive_4_twin,
    analysisDAPCIMPP_pp_sensitive_5_twin=analysisDAPCIMPP_pp_sensitive_5_twin,
    analysisDAPCIMPG_pp_sensitive_1_earlierdep=analysisDAPCIMPG_pp_sensitive_1_earlierdep,
    analysisDAPCIMPG_pp_sensitive_2_earlierdep=analysisDAPCIMPG_pp_sensitive_2_earlierdep,
    analysisDAPCIMPG_pp_sensitive_3_earlierdep=analysisDAPCIMPG_pp_sensitive_3_earlierdep,
    analysisDAPCIMPG_pp_sensitive_4_earlierdep=analysisDAPCIMPG_pp_sensitive_4_earlierdep,
    analysisDAPCIMPG_pp_sensitive_5_earlierdep=analysisDAPCIMPG_pp_sensitive_5_earlierdep,
    analysisDAPCIMPP_pp_sensitive_1_earlierdep=analysisDAPCIMPP_pp_sensitive_1_earlierdep,
    analysisDAPCIMPP_pp_sensitive_2_earlierdep=analysisDAPCIMPP_pp_sensitive_2_earlierdep,
    analysisDAPCIMPP_pp_sensitive_3_earlierdep=analysisDAPCIMPP_pp_sensitive_3_earlierdep,
    analysisDAPCIMPP_pp_sensitive_4_earlierdep=analysisDAPCIMPP_pp_sensitive_4_earlierdep,
    analysisDAPCIMPP_pp_sensitive_5_earlierdep=analysisDAPCIMPP_pp_sensitive_5_earlierdep,
    analysisDAPCIMPG_pp_sensitive_1_caesarean=analysisDAPCIMPG_pp_sensitive_1_caesarean,
    analysisDAPCIMPG_pp_sensitive_2_caesarean=analysisDAPCIMPG_pp_sensitive_2_caesarean,
    analysisDAPCIMPG_pp_sensitive_3_caesarean=analysisDAPCIMPG_pp_sensitive_3_caesarean,
    analysisDAPCIMPG_pp_sensitive_4_caesarean=analysisDAPCIMPG_pp_sensitive_4_caesarean,
    analysisDAPCIMPG_pp_sensitive_5_caesarean=analysisDAPCIMPG_pp_sensitive_5_caesarean,
    analysisDAPCIMPP_pp_sensitive_1_caesarean=analysisDAPCIMPP_pp_sensitive_1_caesarean,
    analysisDAPCIMPP_pp_sensitive_2_caesarean=analysisDAPCIMPP_pp_sensitive_2_caesarean,
    analysisDAPCIMPP_pp_sensitive_3_caesarean=analysisDAPCIMPP_pp_sensitive_3_caesarean,
    analysisDAPCIMPP_pp_sensitive_4_caesarean=analysisDAPCIMPP_pp_sensitive_4_caesarean,
    analysisDAPCIMPP_pp_sensitive_5_caesarean=analysisDAPCIMPP_pp_sensitive_5_caesarean,
    analysisDAPCIMPG_pp_sensitive_1_inflamOrRheum=analysisDAPCIMPG_pp_sensitive_1_inflamOrRheum,
    analysisDAPCIMPG_pp_sensitive_2_inflamOrRheum=analysisDAPCIMPG_pp_sensitive_2_inflamOrRheum,
    analysisDAPCIMPG_pp_sensitive_3_inflamOrRheum=analysisDAPCIMPG_pp_sensitive_3_inflamOrRheum,
    analysisDAPCIMPG_pp_sensitive_4_inflamOrRheum=analysisDAPCIMPG_pp_sensitive_4_inflamOrRheum,
    analysisDAPCIMPG_pp_sensitive_5_inflamOrRheum=analysisDAPCIMPG_pp_sensitive_5_inflamOrRheum,
    analysisDAPCIMPP_pp_sensitive_1_inflamOrRheum=analysisDAPCIMPP_pp_sensitive_1_inflamOrRheum,
    analysisDAPCIMPP_pp_sensitive_2_inflamOrRheum=analysisDAPCIMPP_pp_sensitive_2_inflamOrRheum,
    analysisDAPCIMPP_pp_sensitive_3_inflamOrRheum=analysisDAPCIMPP_pp_sensitive_3_inflamOrRheum,
    analysisDAPCIMPP_pp_sensitive_4_inflamOrRheum=analysisDAPCIMPP_pp_sensitive_4_inflamOrRheum,
    analysisDAPCIMPP_pp_sensitive_5_inflamOrRheum=analysisDAPCIMPP_pp_sensitive_5_inflamOrRheum,
    analysisDAPCIMPG_pp_sensitive_1_adjusted=analysisDAPCIMPG_pp_sensitive_1_adjusted,
    analysisDAPCIMPG_pp_sensitive_2_adjusted=analysisDAPCIMPG_pp_sensitive_2_adjusted,
    analysisDAPCIMPG_pp_sensitive_3_adjusted=analysisDAPCIMPG_pp_sensitive_3_adjusted,
    analysisDAPCIMPP_pp_sensitive_1_adjusted=analysisDAPCIMPP_pp_sensitive_1_adjusted,
    analysisDAPCIMPP_pp_sensitive_2_adjusted=analysisDAPCIMPP_pp_sensitive_2_adjusted,
    analysisDAPCIMPP_pp_sensitive_3_adjusted=analysisDAPCIMPP_pp_sensitive_3_adjusted,
    summaryIM=summaryIM
    ))
}






