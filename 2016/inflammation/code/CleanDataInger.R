
CleanDataInger <- function(){
  d <- CleanData()
  
  file <- file.path(RAWmisc::PROJ$RAW,"InflammationsfilAsaArbetsfil.sav")
  #masterData <- foreign::read.spss(file, to.data.frame=TRUE)
  masterData <- haven::read_spss(file)
  dim(masterData)
  masterData$Include = haven::as_factor(masterData$Include)
  masterData <- masterData[masterData$Include=="ja",]
  masterData <- as.data.frame(masterData)
  dim(masterData)
  
  IFinger <- names(masterData)[which(names(masterData)=="CSF1"):which(names(masterData)=="ST1A1")]
  IFinger <- masterData[,c("KOD",IFinger)]
  names(IFinger)[1] <- "CustomDataR"
  
  names(masterData)[which(names(masterData)=="ålder")] <- "alder"
  
  
  confounders <- c(
    "alder",
    "parity",
    "BMI",
    "Approx_dagartillPartus",
    "Fastande_vid_blodprov",
    "preeclampsia_hypertension",
    "Smoking",
    "inflammatorisk_sjd")
  
  masterData <- masterData[,c("KOD","Group",confounders)]
  names(masterData)[1] <- "CustomDataR"
  IF <- d$data[d$data$CustomDataR %in% IFinger$CustomDataR,c("CustomDataR",d$namesLog2IMPG)]
  
  miss <- IFinger$CustomDataR[!IFinger$CustomDataR %in% IF$CustomDataR]
  
  longIF <- reshape2::melt(IF,id=c("CustomDataR"))
  longIF$variable <- gsub("l_im_","",as.character(longIF$variable))
  for(i in 1:nrow(longIF)) longIF$variable[i] <- gsub(stringr::str_sub(longIF$variable[i],1,4),"",as.character(longIF$variable[i]))
  
  replacementIFs <- reshape2::melt(IFinger[IFinger$CustomDataR %in% miss,],id="CustomDataR")
  replacementIFs$value <- log2(exp(replacementIFs$value)+1)
  replacementIFs$variable <- as.character(replacementIFs$variable)
  
  IF <- rbind(longIF,replacementIFs)
  IF <- reshape2::dcast(IF,CustomDataR~variable,value.var="value")
  
  percExists <- apply(IF,2,function(x){sum(!is.na(x))/length(x)})
  IF <- IF[percExists>0.95]
  
  study <- unlist(xtabs(~masterData[,c("Group")]))/sum(!is.na(masterData$Group))
  background <- c(0.14,0.04)
  background <- c(1-sum(background),background)
  weightsGroup <- background/study
  
  masterData$weights <- 0
  for(i in names(weightsGroup)) masterData$weights[masterData$Group==i] <- weightsGroup[i]

  IFnames <- names(IF)[-1]
  masterData2 <- merge(masterData,IF,by="CustomDataR")
  dim(masterData2)
  dim(masterData)
  
  return(list(
    ID=masterData2$CustomDataR,
    IF=masterData2[,IFnames],
    groups=masterData2[,c("Group")],
    confounders=masterData2[,confounders],
    weights=masterData2$weights
  ))
}






