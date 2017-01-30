BoxPlotsByCase <- function(d){
 
  keep <- !is.na(d$data$o_pp_sensitive_2)
  keep[is.na(keep)] <- FALSE
  pg <- d$data[keep,c("CustomDataR","o_pp_sensitive_2",d$namesLog2IMPG)]
  pp <- d$data[keep,c("CustomDataR","o_pp_sensitive_2",d$namesLog2IMPP)]
  pg$o_pp_sensitive_2 <- as.character(pg$o_pp_sensitive_2)
  pp$o_pp_sensitive_2 <- as.character(pp$o_pp_sensitive_2)
  pg %<>% gather(key=im, value=value, -CustomDataR, -o_pp_sensitive_2)
  pp %<>% gather(key=im, value=value, -CustomDataR, -o_pp_sensitive_2)

  pg$im <- gsub("_","-",gsub("l_im_[0-9][0-9][0-9]_","",as.character(pg$im)))
  pp$im <- gsub("_","-",gsub("l_im_[0-9][0-9][0-9]_","",as.character(pp$im)))
  
  #res <- reshape2::melt(res,id=c("CustomDataR","im"))
  q <- ggplot(pg, aes(y=value, x=o_pp_sensitive_2,colour=o_pp_sensitive_2))
  q <- q + geom_boxplot()
  q <- q + facet_wrap(~im,scales="free")
  q <- SMAOFormatGGPlot(q, sizeMultiplier=1)
  q <- q + scale_x_discrete("",breaks=NULL)
  q <- q + scale_y_continuous("")
  q <- q + scale_colour_brewer("",palette="Set1")
  SMAOpng(paste0("results/boxplotsPG_control_case.png"), landscape=FALSE)
  print(q)
  dev.off()
  
  q <- ggplot(pp, aes(y=value, x=o_pp_sensitive_2,colour=o_pp_sensitive_2))
  q <- q + geom_boxplot()
  q <- q + facet_wrap(~im,scales="free")
  q <- SMAOFormatGGPlot(q, sizeMultiplier=1)
  q <- q + scale_x_discrete("",breaks=NULL)
  q <- q + scale_y_continuous("")
  q <- q + scale_colour_brewer("",palette="Set1")
  SMAOpng(paste0("results/boxplotsPP_control_case.png"), landscape=FALSE)
  print(q)
  dev.off()
}

BoxPlotsByCaseSpecific <- function(d,pgKeep=NULL,ppKeep=NULL){
  
  keep <- !is.na(d$data$o_pp_sensitive_2)
  keep[is.na(keep)] <- FALSE
  pg <- d$data[keep,c("CustomDataR","o_pp_sensitive_2",d$namesLog2IMPG)]
  pp <- d$data[keep,c("CustomDataR","o_pp_sensitive_2",d$namesLog2IMPP)]
  pg$o_pp_sensitive_2 <- as.character(pg$o_pp_sensitive_2)
  pp$o_pp_sensitive_2 <- as.character(pp$o_pp_sensitive_2)
  pg %<>% gather(key=im, value=value, -CustomDataR, -o_pp_sensitive_2)
  pp %<>% gather(key=im, value=value, -CustomDataR, -o_pp_sensitive_2)
  
  if(!is.null(pgKeep)) pg <- pg[pg$im %in% pgKeep,]
  if(!is.null(ppKeep)) pp <- pp[pp$im %in% ppKeep,]
  
  pg$im <- gsub("_","-",gsub("l_im_[0-9][0-9][0-9]_","",as.character(pg$im)))
  pp$im <- gsub("_","-",gsub("l_im_[0-9][0-9][0-9]_","",as.character(pp$im)))
  
  #res <- reshape2::melt(res,id=c("CustomDataR","im"))
  q <- ggplot(pg, aes(y=value, x=o_pp_sensitive_2,colour=o_pp_sensitive_2))
  q <- q + geom_boxplot(lwd=2,outlier.size=4)
  q <- q + facet_wrap(~im,scales="free")
  q <- SMAOFormatGGPlot(q)
  q <- q + scale_x_discrete("",breaks=NULL)
  q <- q + scale_y_continuous("")
  q <- q + scale_colour_brewer("",palette="Set1")
  SMAOpng(paste0("results/boxplotsPG_control_case_specific.png"), landscape=TRUE)
  print(q)
  dev.off()
  
  q <- ggplot(pp, aes(y=value, x=o_pp_sensitive_2,colour=o_pp_sensitive_2))
  q <- q + geom_boxplot()
  q <- q + facet_wrap(~im,scales="free")
  q <- SMAOFormatGGPlot(q, sizeMultiplier=1)
  q <- q + scale_x_discrete("",breaks=NULL)
  q <- q + scale_y_continuous("")
  q <- q + scale_colour_brewer("",palette="Set1")
  SMAOpng(paste0("results/boxplotsPP_control_case_specific.png"), landscape=FALSE)
  print(q)
  dev.off()
}


JointBoxplots <- function(d){
  pg <- d$data[,c("CustomDataR",d$namesLog2IMPG)]
  pp <- d$data[,c("CustomDataR",d$namesLog2IMPP)]
  pg %<>% gather(key=im, value=PG, -CustomDataR)
  pp %<>% gather(key=im, value=PP, -CustomDataR)
  pg$im <- gsub("c_im_","",pg$im)
  pp$im <- gsub("c_im_","",pp$im)
  pp$im <- gsub("_pp","",pp$im)
  names(pp)[3] <- "value"
  pp$variable <- "PP"
  names(pg)[3] <- "value"
  pg$variable <- "PG"
  res <- rbind(pg,pp)#merge(pg,pp,by=c("CustomDataR","im"))
  dim(res)
  dim(pg)
  dim(pp)
  #res <- reshape2::melt(res,id=c("CustomDataR","im"))
  q <- ggplot(res, aes(y=value, x=variable,colour=variable))
  q <- q + geom_boxplot()
  q <- q + facet_wrap(~im,scales="free")
  q <- SMAOFormatGGPlot(q, sizeMultiplier=1)
  q <- q + scale_x_discrete("",breaks=NULL)
  q <- q + scale_y_continuous("")
  SMAOpng(paste0("results/boxplots.png"), landscape=FALSE)
  print(q)
  dev.off()
  
  keep <- d$data$o_pp_sensitive_1=="Control"
  keep[is.na(keep)] <- FALSE
  pg <- d$data[keep,c("CustomDataR",d$namesLog2IMPG)]
  pp <- d$data[keep,c("CustomDataR",d$namesLog2IMPP)]
  pg %<>% gather(key=im, value=PG, -CustomDataR)
  pp %<>% gather(key=im, value=PP, -CustomDataR)
  pg$im <- gsub("c_im_","",pg$im)
  pp$im <- gsub("c_im_","",pp$im)
  pp$im <- gsub("_pp","",pp$im)
  names(pp)[3] <- "value"
  pp$variable <- "PP"
  names(pg)[3] <- "value"
  pg$variable <- "PG"
  res <- rbind(pg,pp)#res <- merge(pg,pp,by=c("CustomDataR","im"))
  dim(res)
  dim(pg)
  dim(pp)
  #res <- reshape2::melt(res,id=c("CustomDataR","im"))
  q <- ggplot(res, aes(y=value, x=variable,colour=variable))
  q <- q + geom_boxplot()
  q <- q + facet_wrap(~im,scales="free")
  q <- SMAOFormatGGPlot(q, sizeMultiplier=1)
  q <- q + scale_x_discrete("",breaks=NULL)
  q <- q + scale_y_continuous("")
  SMAOpng(paste0("results/boxplots_control.png"), landscape=FALSE)
  print(q)
  dev.off()
  
  keep <- !is.na(d$data$o_pp_sensitive_2)
  keep[is.na(keep)] <- FALSE
  pg <- d$data[keep,c("CustomDataR","o_pp_sensitive_2",d$namesLog2IMPG)]
  pp <- d$data[keep,c("CustomDataR","o_pp_sensitive_2",d$namesLog2IMPP)]
  pg$o_pp_sensitive_2 <- as.numeric(pg$o_pp_sensitive_2)-1
  pp$o_pp_sensitive_2 <- as.numeric(pp$o_pp_sensitive_2)-1
  pg %<>% gather(key=im, value=PG, -CustomDataR,-o_pp_sensitive_2)
  pp %<>% gather(key=im, value=PP, -CustomDataR,-o_pp_sensitive_2)
  pg$im <- gsub("c_im_","",pg$im)
  pp$im <- gsub("c_im_","",pp$im)
  pp$im <- gsub("_pp","",pp$im)
  names(pp)[4] <- "value"
  pp$variable <- "PP"
  names(pg)[4] <- "value"
  pg$variable <- "PG"
  res <- rbind(pg,pp)#res <- merge(pg,pp,by=c("CustomDataR","im"))
  dim(res)
  dim(pg)
  dim(pp)
  res$variable[res$o_pp_sensitive_2==0] <- paste0(res$variable[res$o_pp_sensitive_2==0]," control")
  res$variable[res$o_pp_sensitive_2==1] <- paste0(res$variable[res$o_pp_sensitive_2==1]," case")
  res$variable <- factor(res$variable,levels=c("PG control","PG case","PP control","PP case"))
  #res <- reshape2::melt(res,id=c("CustomDataR","im"))
  q <- ggplot(res, aes(y=value, x=variable,colour=variable))
  q <- q + geom_boxplot()
  q <- q + facet_wrap(~im,scales="free")
  q <- SMAOFormatGGPlot(q, sizeMultiplier=1,ncol=4)
  q <- q + scale_colour_brewer("",palette="Set1")
  q <- q + scale_x_discrete("",breaks=NULL)
  q <- q + scale_y_continuous("")
  SMAOpng(paste0("results/boxplots_pregpp_by_case.png"), landscape=FALSE)
  print(q)
  dev.off()
}

NormalVsLogDataQQandBoxPlots <- function(data, logData, fileName){
  
  data %<>% gather(key=im, value=val, -CustomDataR)
  logData %<>% gather(key=im, value=val, -CustomDataR)
  q <- ggplot(data, aes(y=val, x=im))
  q <- q + geom_boxplot()
  q <- q + facet_wrap(~im,scales="free")
  q <- SMAOFormatGGPlot(q, sizeMultiplier=1)
  q <- q + scale_x_discrete("",breaks=NULL)
  q <- q + scale_y_continuous("")
  SMAOpng(paste0("results/boxplots_im",fileName,".png"), landscape=FALSE)
  print(q)
  dev.off()
  
  q <- ggplot(data, aes(sample=val))
  q <- q + stat_qq()
  q <- q + facet_wrap(~im,scales="free")
  q <- SMAOFormatGGPlot(q, sizeMultiplier=1)
  q <- q + scale_x_continuous("")
  q <- q + scale_y_continuous("")
  SMAOpng(paste0("results/qqplot_im",fileName,".png"), landscape=FALSE)
  print(q)
  dev.off()
  
  q <- ggplot(logData, aes(sample=val))
  q <- q + stat_qq()
  q <- q + facet_wrap(~im,scales="free")
  q <- SMAOFormatGGPlot(q, sizeMultiplier=1)
  q <- q + scale_x_continuous("")
  q <- q + scale_y_continuous("")
  SMAOpng(paste0("results/qqplotlog2_im",fileName,".png"), landscape=FALSE)
  print(q)
  dev.off()

}


