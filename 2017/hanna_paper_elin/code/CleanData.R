ObtainLODdivsqrt2 <- function(){
  file <- file.path(RAWmisc::PROJ$RAW,"lod.txt")
  lod <- as.data.frame(fread(file))
  lod$lod <- as.numeric(stringr::str_replace_all(lod$lod, ",", "."))
  names(lod) <- c("factor","lod_base_e")
  #lod$imNum <- stringr::str_extract(lod$factor, "^[0-9][0-9][0-9]")
  #lod <- lod[,-1]
  lod$lod_base_10 <- exp(lod$lod_base_e)
  lod$lod_base_10_div_sqrt_2 <- lod$lod_base_10/sqrt(2)
  lod$substitution_for_base_e <- log(lod$lod_base_10_div_sqrt_2)
  openxlsx::write.xlsx(lod,file.path(RPROJ$PROJBAKED,"data_lod.xlsx"))  
}

FixLOD <- function(dataIM){
  #file <- system.file("extdata","lod.txt",package="inflammation")
  file <- file.path(RAWmisc::PROJ$RAW,"lod.txt")
  lod <- as.data.frame(fread(file))
  lod$lod <- as.numeric(stringr::str_replace_all(lod$lod, ",", "."))
  lod$imNum <- stringr::str_extract(lod$factor, "^[0-9][0-9][0-9]")
  lod <- lod[,-1]
  lod$lod <- exp(lod$lod)
  
  dataIMPre <- dataIM[,-2]
  dataIMPost <- dataIM
  for(i in 3:ncol(dataIMPost)){
    dataIMPost[dataIMPost[,2]==1 & is.na(dataIMPost[,i]),i] <- -99
  }
  dataIMPost <- dataIMPost[,-2]
  
  dataIMPost %<>% gather(key=im, value=val, -CustomDataR)
  index <- dataIMPost$val!=-99
  index[is.na(index)] <- FALSE
  dataIMPost$val[index] <- exp(dataIMPost$val[index])
  dataIMPost$imNum <- stringr::str_extract(dataIMPost$im, "[0-9][0-9][0-9]")
  
  dim(dataIMPost)
  dataIMPost <- merge(dataIMPost, lod, by="imNum")
  dim(dataIMPost)
  
  index <- dataIMPost$val==-99 & !is.na(dataIMPost$val)
  dataIMPost$post <- dataIMPost$val
  dataIMPost$post[index] <- dataIMPost$lod[index]/sqrt(2)
  
  dataIMPost$val <- log(dataIMPost$val)
  dataIMPost$post <- log(dataIMPost$post)
  dataIMPost$lod <- log(dataIMPost$lod)
  dataIMPost <- data.table(dataIMPost)
  setnames(dataIMPost,"val","pre")
  dataIMPost[!is.na(post),underLOD:=0]
  dataIMPost[!is.na(post) & is.na(pre), underLOD:=1]
  return(dataIMPost)
}


CleanData <- function(){
  numbers <- list()
  #file <- system.file("extdata","Date discrepancies with CS 160115.txt",package="inflammation")
  file <- file.path(RAWmisc::PROJ$RAW,"inflammation_170131.sav")
  data <- haven::read_spss(file)
  
  data$im_participating_preg[data$CustomDataR==631] <- 0
  data$im_152_PDL1[data$CustomDataR==631] <- NA
  
  data <- data[!is.na(data$im_participating_pp) | !is.na(data$im_participating_preg),]
 
  
  # INFLMATTION MARKERS
  
  dataNamesIMPG <- names(data)[stringr::str_detect(names(data),"^im_[0-9a-zA-Z_]*")]
  dataNamesIMPP <- names(data)[stringr::str_detect(names(data),"^im_[0-9a-zA-Z_]*_pp")]
  dataNamesIMPG <- dataNamesIMPG[!dataNamesIMPG %in% dataNamesIMPP]
  dataNamesIMPG <- dataNamesIMPG[!dataNamesIMPG %in% c("im_time_point","im_participating_preg","im_participating_pp")]
  
  dataNamesIMPG <- dataNamesIMPG[!dataNamesIMPG %in% c("im_103_BDNF")]
  dataNamesIMPP <- dataNamesIMPP[!dataNamesIMPP %in% c("im_103_BDNF_pp")]
  
  dataIMPG <- FixLOD(dataIM = data[,c("CustomDataR","im_participating_preg",dataNamesIMPG)])
  dataIMPP <- FixLOD(dataIM = data[,c("CustomDataR","im_participating_pp",dataNamesIMPP)])
  
  sumPG <- dataIMPG[,
                    .(n=sum(!is.na(post)),
                      LOD=mean(lod,na.rm=T),
                      percUnderLOD=100*mean(underLOD,na.rm=T),
                      meanVal=mean(post,na.rm=T),
                      sdVal=sd(post,na.rm=T),
                      medianVal=median(post,na.rm=T),
                      p25=quantile(post,probs=0.25,na.rm=T),
                      p75=quantile(post,probs=0.75,na.rm=T)
                    ),
                    by=im]
  openxlsx::write.xlsx(sumPG,file.path(RAWmisc::PROJ$SHARED_TODAY,"summary_pg.xlsx"))
  
  sumPP <- dataIMPP[,
                    .(n=sum(!is.na(post)),
                      LOD=mean(lod,na.rm=T),
                      percUnderLOD=100*mean(underLOD,na.rm=T),
                      meanVal=mean(post,na.rm=T),
                      sdVal=sd(post,na.rm=T),
                      medianVal=median(post,na.rm=T),
                      p25=quantile(post,probs=0.25,na.rm=T),
                      p75=quantile(post,probs=0.75,na.rm=T)
                    ),
                    by=im]
  openxlsx::write.xlsx(sumPG,file.path(RAWmisc::PROJ$SHARED_TODAY,"summary_pp.xlsx"))
  
  numbers[["Number of IMPGs (original)"]] <- length(unique(dataIMPG$im))
  dataIMPG <- dataIMPG[im %in% sumPG[percUnderLOD<25]$im]
  numbers[["Number of IMPGs (after processing)"]] <- length(unique(dataIMPG$im))
  
  numbers[["Number of IMPPs (original)"]] <- length(unique(dataIMPP$im))
  dataIMPP <- dataIMPP[im %in% sumPP[percUnderLOD<25]$im]
  numbers[["Number of IMPPs (after processing)"]] <- length(unique(dataIMPP$im))
  
  saveRDS(numbers,file.path(RAWmisc::PROJ$BAKED,"numbers.RDS"))
  
  ## PG ZSCORE
  dataIMPG[,zscore:=(post-mean(post,na.rm=T))/sd(post,na.rm=T)]
  dataIMPGz <- dataIMPG[,
                        .(zscore=mean(zscore,na.rm=T)
                          ),
                        by=CustomDataR]
  dataIMPGz[,zscorePG:=(zscore-mean(zscore,na.rm=T))/sd(zscore,na.rm=T)]
  dataIMPGz[,zscore:=NULL]
  dataIMPG[,zscore:=NULL]
  dim(dataIMPG)
  dataIMPG <- merge(dataIMPG,dataIMPGz,by="CustomDataR")
  dim(dataIMPG)
  
  ## PP ZSCORE
  dataIMPP[,zscore:=(post-mean(post,na.rm=T))/sd(post,na.rm=T)]
  dataIMPPz <- dataIMPP[,
                        .(zscore=mean(zscore,na.rm=T)
                        ),
                        by=CustomDataR]
  dataIMPPz[,zscorePP:=(zscore-mean(zscore,na.rm=T))/sd(zscore,na.rm=T)]
  dataIMPPz[,zscore:=NULL]
  dataIMPP[,zscore:=NULL]
  dim(dataIMPP)
  dataIMPP <- merge(dataIMPP,dataIMPPz,by="CustomDataR")
  dim(dataIMPP)
  
  ## RESHAPING TO WIDE
  dataIMPG[,imNum:=NULL]
  dataIMPG[,pre:=NULL]
  dataIMPG[,lod:=NULL]
  dataIMPG[,underLOD:=NULL]
  dataIMPG <- dcast(dataIMPG,CustomDataR+zscorePG~im,value.var="post")
  
  dataIMPP[,imNum:=NULL]
  dataIMPP[,pre:=NULL]
  dataIMPP[,lod:=NULL]
  dataIMPP[,underLOD:=NULL]
  dataIMPP <- dcast(dataIMPP,CustomDataR+zscorePP~im,value.var="post")
  
  data <- merge(dataIMPG,dataIMPP, by="CustomDataR")
  dim(data)
  dim(dataIMPG)
  dim(dataIMPP)
  
  openxlsx::write.xlsx(data,file.path(RAWmisc::PROJ$SHARED_TODAY,"data_inflammation_factors.xlsx"))
  
  return(data)
}






