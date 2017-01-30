
RunDAPC <- function(data, group, file){
  
  set.seed(4)
  crossval <- xvalDapc(data, group, 
           n.pca.max=ceiling(ncol(data)/3), 
           result="groupMean",
           center=TRUE, scale=FALSE,
           n.pca=NULL, n.rep=200, xval.plot=FALSE)
  cv <- data.table(crossval[[1]])
  cv <- cv[,list(m=mean(success),s=sd(success)),by=n.pca]
  cv[,l:=m-1*s]
  cv[,u:=m+1*s]
  cv[u>1,u:=1]
  cv[m==max(cv$m),pick:=n.pca]
  
  r <- crossval$`Median and Confidence Interval for Random Chance`
  r <- data.frame(l=r[1],m=r[2],u=r[3])
  
  q <- ggplot(cv)
  q <- q + geom_rect(data=r, mapping=aes(xmin=-Inf,xmax=Inf,ymin=l,ymax=u),fill="grey",alpha=0.4)
  q <- q + geom_hline(data=r, mapping=aes(yintercept=m),lwd=2)
  q <- q + geom_vline(aes(xintercept=pick),lwd=10,alpha=0.2,col="red")
  q <- q + geom_point(aes(x=n.pca,y=m,ymin=l,ymax=u),size=10)
  q <- q + geom_errorbar(aes(x=n.pca,y=m,ymin=l,ymax=u),lwd=2,width=0)
  q <- q + scale_x_continuous("Number PCAs")
  q <- q + scale_y_continuous("Cross-validated proportion of successful outcome prediction (+/-1sd)\n",lim=c(0,1))
  q <- SMAOFormatGGPlot(q)
  SMAOpng(paste0("results/dapc_",file,"_crossvalidation.png"))
  print(q)
  RAWmisc::MakeFootnote("Horizontal grey bar indicates random assignment (median, 2.5, 97.5 percentiles). Vertical indicates CV choice.")
  dev.off()
  r$max <- max(cv$m)
  saveRDS(r,file=paste0("results/dapc_",file,"_summarycrossvalidation.RDS"))
  
  saveRDS(crossval[2:6],file=paste0("results/dapc_",file,"_crossvalidation.RDS"))
  
  dapcOptim <- crossval$DAPC
  
  SMAOpng(paste0("results/dapc_",file,"_scatter.png"))
  scatter(dapcOptim,
          scree.pca=TRUE,posi.pca="bottomleft",
          legend=TRUE, solid=0.4)
  dev.off()
  
  SMAOpng(paste0("results/dapc_",file,"_xaxis.png"),landscape=FALSE)
  axis1 <- data.frame(val=dapcOptim$var.contr[,1])
  axis1$names <- row.names(axis1)
  threshold <- 1/nrow(axis1)
  axis1$threshold <- threshold
  axis1 <- axis1[order(axis1$val),]
  axis1$x <- 1:nrow(axis1)
  axis1$names <- factor(axis1$names,levels=axis1$names)
  q <- ggplot(axis1, aes(x=names, y=val, label=names))
  q <- q + geom_bar(stat="identity")
  q <- q + geom_hline(yintercept=threshold, col="red")
  q <- q + coord_flip()
  q <- SMAOFormatGGPlot(q, sizeMultiplier=2)
  q <- q + scale_x_discrete("")
  q <- q + scale_y_continuous("Loadings")
  print(q)
  RAWmisc::MakeFootnote("Variables shown if loadings > 1/#variables (red line)")
  dev.off()
  saveRDS(axis1,paste0("results/dapc_",file,"_xaxis.RDS"))
  
  vals <- unique(group)
  vals <- vals[order(vals)]
  axis1 <- axis1[order(-axis1$x),]
  axis1 <- axis1[,c("names","val","threshold")]
  axis1[,paste0("G_",vals)] <- NA
  for(i in 1:nrow(axis1)){
    for(j in vals){
      axis1[i,paste0("G_",j)] <- mean(c(data[group==j,as.character(axis1$names[i])][[1]]))
    }
  }
  axis1$val <- format(round(axis1$val,2),2)
  for(j in vals) axis1[,paste0("G_",j)] <- format(round(axis1[,paste0("G_",j)],2),2)
  row.names(axis1) <- NULL
  axis1$names <- as.character(axis1$names)
  
  if(ncol(dapcOptim$tab)==1){
    axis1$pval <- summary(aov(as.matrix(dapcOptim$tab)~group))[[1]][["Pr(>F)"]][[1]]
  } else {
    axis1$pval <- summary(manova(as.matrix(dapcOptim$tab)~group))$stats[1,6] 
  }

  sig <- FALSE
  for(i in 1:nrow(dapcOptim$var.contr)){
    useit <- which(rank(-dapcOptim$var.contr[,1])<=i)
    f <- glm(group ~ as.matrix(data[,useit]), family=binomial())
    summary(f)
    devdiff <- with(f,null.deviance-deviance)
    dfdiff <- with(f,df.null-df.residual)
    p <- pchisq(abs(devdiff),df=dfdiff,lower.tail=FALSE)
    if(p<0.05){
      newthresh <- (min(as.numeric(axis1$val[axis1$names %in% names(data)[useit]]))-0.0001)
      if(axis1$threshold>newthresh){
        axis1$threshold <- 1000
      } else {
        axis1$threshold <- newthresh
      }
      break
    } 
  }
  
  saveRDS(axis1,file=paste0("results/dapc_",file,"_loadings.RDS"))
  
  if(length(unique(group))>2){
    SMAOpng(paste0("results/dapc_",file,"_yaxis.png"))
    axis1 <- data.frame(val=dapcOptim$var.contr[,2])
    axis1$names <- row.names(axis1)
    threshold <- 1/nrow(axis1)
    axis1 <- axis1[axis1$val > threshold,]
    axis1 <- axis1[order(axis1$val),]
    axis1$x <- 1:nrow(axis1)
    axis1$names <- factor(axis1$names,levels=axis1$names)
    q <- ggplot(axis1, aes(x=names, y=val, label=names))
    q <- q + geom_bar(stat="identity")
    q <- q + geom_hline(xintercept=threshold, col="red")
    q <- q + coord_flip()
    q <- SMAOFormatGGPlot(q, sizeMultiplier=2)
    q <- q + scale_x_discrete("")
    q <- q + scale_y_continuous("Loadings")
    print(q)
    RAWmisc::MakeFootnote("Variables shown if loadings > 1/#variables (red line)")
    dev.off()
  }
  
}

RunMultipleDAPCs <- function(
  d,
  pganalysis1,
  pganalysis2,
  pganalysis3,
  ppanalysis1,
  ppanalysis2,
  ppanalysis3,
  naming){
  RunDAPC(data=d$data[pganalysis1,d$namesLog2IMPG],
          group=d$data$o_pp_sensitive_1[pganalysis1], 
          file=paste0(naming,"-IMPG_pp_sensitive_1"))
  
  RunDAPC(data=d$data[pganalysis2,d$namesLog2IMPG],
          group=d$data$o_pp_sensitive_2[pganalysis2], 
          file=paste0(naming,"-IMPG_pp_sensitive_2"))
  
  RunDAPC(data=d$data[pganalysis3,d$namesLog2IMPG],
          group=d$data$o_pp_sensitive_3[pganalysis3], 
          file=paste0(naming,"-IMPG_pp_sensitive_3"))
  
  RunDAPC(data=d$data[ppanalysis1,d$namesLog2IMPP],
          group=d$data$o_pp_sensitive_1[ppanalysis1], 
          file=paste0(naming,"-IMPP_pp_sensitive_1"))
  
  RunDAPC(data=d$data[ppanalysis2,d$namesLog2IMPP],
          group=d$data$o_pp_sensitive_2[ppanalysis2], 
          file=paste0(naming,"-IMPP_pp_sensitive_2"))
  
  RunDAPC(data=d$data[ppanalysis3,d$namesLog2IMPP],
          group=d$data$o_pp_sensitive_3[ppanalysis3], 
          file=paste0(naming,"-IMPP_pp_sensitive_3"))
  
  
  d1 <- readRDS(paste0("results/dapc_",naming,"-IMPG_pp_sensitive_1_summarycrossvalidation.RDS"))
  d2 <- readRDS(paste0("results/dapc_",naming,"-IMPG_pp_sensitive_2_summarycrossvalidation.RDS"))
  d3 <- readRDS(paste0("results/dapc_",naming,"-IMPG_pp_sensitive_3_summarycrossvalidation.RDS"))
  d4 <- readRDS(paste0("results/dapc_",naming,"-IMPP_pp_sensitive_1_summarycrossvalidation.RDS"))
  d5 <- readRDS(paste0("results/dapc_",naming,"-IMPP_pp_sensitive_2_summarycrossvalidation.RDS"))
  d6 <- readRDS(paste0("results/dapc_",naming,"-IMPP_pp_sensitive_3_summarycrossvalidation.RDS"))
  
  dx <- rbind(d1,d2,d3,d4,d5,d6)
  dx$outcome <- c(
    "-- vs x+",
    "-- vs -+",
    "-- vs +x",
    "-- vs x+",
    "-- vs -+",
    "-- vs +x"
  )
  dx$exposure <- c(
    "PG",
    "PG",
    "PG",
    "PP",
    "PP",
    "PP"
  )
  saveRDS(dx,file=paste0("results/dapc_",naming,"-crossvalidation.RDS"))
  #tab <- htmlTable(dx,
  #          header=c("Cross-Validation mean","Random assignment (95% CI)"),
  #          rnames=c(
  #            label(d$data$o_pp_sensitive_1),
  #            label(d$data$o_pp_sensitive_2),
  #            label(d$data$o_pp_sensitive_3),
  #            label(d$data$o_pp_sensitive_1),
  #            label(d$data$o_pp_sensitive_2),
  #            label(d$data$o_pp_sensitive_3)),
  #          rgroup=c("Pregnant Inflammation Factors","Post-partum Inflammation Factors"),
  #          n.rgroup=c(3,3),
  #          caption="Cross-validated proportion of successful group assignment using discriminant analysis of principle components (DAPC)")
  #saveRDS(tab,file="results/dapc_crossvalidation.RDS")
  
  dx <- readRDS(paste0("results/dapc_",naming,"-IMPG_pp_sensitive_1_loadings.RDS"))
  dx$G_Control <- as.numeric(dx$G_Control)
  dx$G_Case <- as.numeric(dx$G_Case)
  dx$supports <- "Nothing"
  dx$supports[dx$val > dx$threshold] <- "Controls"
  dx$supports[dx$val > dx$threshold & dx$G_Case > dx$G_Control] <- "Cases"
  dx$names <- gsub("_pg$","",dx$names)
  dx$outcome <- c("-- vs -+")
  dx$exposure <- "PG"
  d1 <- dx
  
  dx <- readRDS(paste0("results/dapc_",naming,"-IMPG_pp_sensitive_2_loadings.RDS"))
  dx$G_Control <- as.numeric(dx$G_Control)
  dx$G_Case <- as.numeric(dx$G_Case)
  dx$supports <- "Nothing"
  dx$supports[dx$val > dx$threshold] <- "Controls"
  dx$supports[dx$val > dx$threshold & dx$G_Case > dx$G_Control] <- "Cases"
  dx$names <- gsub("_pg$","",dx$names)
  dx$outcome <- c("x- vs x+")
  dx$exposure <- "PG"
  d2 <- dx
  
  dx <- readRDS(paste0("results/dapc_",naming,"-IMPG_pp_sensitive_3_loadings.RDS"))
  dx$G_Control <- as.numeric(dx$G_Control)
  dx$G_Case <- as.numeric(dx$G_Case)
  dx$supports <- "Nothing"
  dx$supports[dx$val > dx$threshold] <- "Controls"
  dx$supports[dx$val > dx$threshold & dx$G_Case > dx$G_Control] <- "Cases"
  dx$names <- gsub("_pg$","",dx$names)
  dx$outcome <- c("-x vs +x")
  dx$exposure <- "PG"
  d3 <- dx
  
  
  dx <- readRDS(paste0("results/dapc_",naming,"-IMPP_pp_sensitive_1_loadings.RDS"))
  dx$G_Control <- as.numeric(dx$G_Control)
  dx$G_Case <- as.numeric(dx$G_Case)
  dx$supports <- "Nothing"
  dx$supports[dx$val > dx$threshold] <- "Controls"
  dx$supports[dx$val > dx$threshold & dx$G_Case > dx$G_Control] <- "Cases"
  dx$names <- gsub("_pp$","",dx$names)
  dx$outcome <- c("-- vs -+")
  dx$exposure <- "PP"
  d4 <- dx
  
  dx <- readRDS(paste0("results/dapc_",naming,"-IMPP_pp_sensitive_2_loadings.RDS"))
  dx$G_Control <- as.numeric(dx$G_Control)
  dx$G_Case <- as.numeric(dx$G_Case)
  dx$supports <- "Nothing"
  dx$supports[dx$val > dx$threshold] <- "Controls"
  dx$supports[dx$val > dx$threshold & dx$G_Case > dx$G_Control] <- "Cases"
  dx$names <- gsub("_pp$","",dx$names)
  dx$outcome <- c("x- vs x+")
  dx$exposure <- "PP"
  d5 <- dx
  
  dx <- readRDS(paste0("results/dapc_",naming,"-IMPP_pp_sensitive_3_loadings.RDS"))
  dx$G_Control <- as.numeric(dx$G_Control)
  dx$G_Case <- as.numeric(dx$G_Case)
  dx$supports <- "Nothing"
  dx$supports[dx$val > dx$threshold] <- "Controls"
  dx$supports[dx$val > dx$threshold & dx$G_Case > dx$G_Control] <- "Cases"
  dx$names <- gsub("_pp$","",dx$names)
  dx$outcome <- c("-x vs +x")
  dx$exposure <- "PP"
  d6 <- dx
  
  loadings <- rbind(d1,d2,d3,d4,d5,d6)
  saveRDS(loadings,file=paste0("results/dapc_",naming,"-loadings.RDS"))
}


RunIFSummary <- function(data, group, file){
  res <- data.frame(names=names(data))
  res$controlN <- 0
  res$controlMean <- 0
  res$controlSD <- 0
  res$caseN <- 0
  res$caseMean <- 0
  res$caseSD <- 0
  
  res$wilcoxpval <- 100
  res$wilcoxpvalbonf <- 100
  res$OR <- 0
  res$ORpval <- 100
  res$ORpvalbonf <- 100
  
  for(i in 1:nrow(res)){
    val <- data[group=="Control",as.character(res$names[i])]
    res$controlN[i] <- sum(!is.na(val))
    res$controlMean[i] <- mean(val,na.rm=T)
    res$controlSD[i] <- sd(val,na.rm=T)
    
    val <- data[group=="Case",as.character(res$names[i])]
    res$caseN[i] <- sum(!is.na(val))
    res$caseMean[i] <- mean(val,na.rm=T)
    res$caseSD[i] <- sd(val,na.rm=T)
    
    form <- paste0("group~",as.character(res$names[i]))
    fit <- glm(as.formula(form),data=data,family=binomial())
    if(nrow(summary(fit)$coefficients)==2){
      res$OR[i] <- summary(fit)$coefficients[2,1]
      res$ORpval[i] <- summary(fit)$coefficients[2,4]
      form <- paste0("as.numeric(data$",as.character(res$names[i]),")~group")
      fit <- wilcox.test(as.formula(form))
      res$wilcoxpval[i] <- fit$p.value
    }
  }
  
  res$wilcoxpvalbonf <- res$wilcoxpval*nrow(res)
  res$ORpvalbonf <- res$ORpval*nrow(res)
  saveRDS(res,file=paste0("results/IFsummary_",file,"_OR.RDS"))
}

RunMultipleIFSummaries <- function(
  d,
  pganalysis1,
  pganalysis2,
  pganalysis3,
  ppanalysis1,
  ppanalysis2,
  ppanalysis3,
  naming,
  modelPG=NULL,modelPP=NULL){
  
  if(!is.null(pganalysis1)) RunIFSummary(data=d$data[pganalysis1,c(d$namesLog2IMPG,modelPG)],
                                  group=d$data$o_pp_sensitive_1[pganalysis1], 
                                  file=paste0(naming,"-IMPG_pp_sensitive_1"))
  
  if(!is.null(pganalysis2)) RunIFSummary(data=d$data[pganalysis2,c(d$namesLog2IMPG,modelPG)],
                                  group=d$data$o_pp_sensitive_2[pganalysis2], 
                                  file=paste0(naming,"-IMPG_pp_sensitive_2"))
  
  if(!is.null(pganalysis3)) RunIFSummary(data=d$data[pganalysis3,c(d$namesLog2IMPG,modelPG)],
                                  group=d$data$o_pp_sensitive_3[pganalysis3], 
                                  file=paste0(naming,"-IMPG_pp_sensitive_3"))
  
  if(!is.null(ppanalysis1)) RunIFSummary(data=d$data[ppanalysis1,c(d$namesLog2IMPP,modelPP)],
                                  group=d$data$o_pp_sensitive_1[ppanalysis1], 
                                  file=paste0(naming,"-IMPP_pp_sensitive_1"))
  
  if(!is.null(ppanalysis2)) RunIFSummary(data=d$data[ppanalysis2,c(d$namesLog2IMPP,modelPP)],
                                  group=d$data$o_pp_sensitive_2[ppanalysis2], 
                                  file=paste0(naming,"-IMPP_pp_sensitive_2"))
  
  if(!is.null(ppanalysis3)) RunIFSummary(data=d$data[ppanalysis3,c(d$namesLog2IMPP,modelPP)],
                                  group=d$data$o_pp_sensitive_3[ppanalysis3], 
                                  file=paste0(naming,"-IMPP_pp_sensitive_3"))
  
  
  dxres <- list()
  if(!is.null(pganalysis1)){
    dx <- readRDS(file=paste0("results/IFsummary_",naming,"-IMPG_pp_sensitive_1","_OR.RDS"))
    if(nrow(dx)>1){
      dx$names <- gsub("_pg$","",dx$names)
      dx$outcome <- c("-- vs -+")
      dx$exposure <- "PG"
      dxres[[length(dxres)+1]] <- dx
    }
  }
  if(!is.null(pganalysis2)){
    dx <- readRDS(paste0("results/IFsummary_",naming,"-IMPG_pp_sensitive_2","_OR.RDS"))
    if(nrow(dx)>1){
      dx$names <- gsub("_pg$","",dx$names)
      dx$outcome <- c("x- vs x+")
      dx$exposure <- "PG"
      dxres[[length(dxres)+1]] <- dx
    }
  }
  
  if(!is.null(pganalysis3)){
    dx <- readRDS(paste0("results/IFsummary_",naming,"-IMPG_pp_sensitive_3","_OR.RDS"))
    if(nrow(dx)>1){
      dx$names <- gsub("_pg$","",dx$names)
      dx$outcome <- c("-x vs +x")
      dx$exposure <- "PG"
      dxres[[length(dxres)+1]] <- dx
    }
  }
  
  if(!is.null(ppanalysis1)){
    dx <- readRDS(paste0("results/IFsummary_",naming,"-IMPP_pp_sensitive_1","_OR.RDS"))
    if(nrow(dx)>1){
      dx$names <- gsub("_pp$","",dx$names)
      dx$outcome <- c("-- vs -+")
      dx$exposure <- "PP"
      dxres[[length(dxres)+1]] <- dx
    }
  }
  
  if(!is.null(ppanalysis2)){
    dx <- readRDS(paste0("results/IFsummary_",naming,"-IMPP_pp_sensitive_2","_OR.RDS"))
    if(nrow(dx)>1){
      dx$names <- gsub("_pp$","",dx$names)
      dx$outcome <- c("x- vs x+")
      dx$exposure <- "PP"
      dxres[[length(dxres)+1]] <- dx
    }
  }
  
  if(!is.null(ppanalysis3)){
    dx <- readRDS(paste0("results/IFsummary_",naming,"-IMPP_pp_sensitive_3","_OR.RDS"))
    if(nrow(dx)>1){
      dx$names <- gsub("_pp$","",dx$names)
      dx$outcome <- c("-x vs +x")
      dx$exposure <- "PP"
      dxres[[length(dxres)+1]] <- dx
    }
  }
  
  dxres <- rbindlist(dxres)
  
  saveRDS(dxres,file=paste0("results/IFsummaries_",naming,"-loadings.RDS"))
  
}




RunEN <- function(data, group, file, lasso=FALSE){
  
  alphas <- seq(0, 1, by=.02)
  mses <- numeric(length(alphas))
  mins <- numeric(length(alphas))
  maxes <- numeric(length(alphas))
  
  for(i in 1:length(alphas)){
    cvfits <- glmnet::cv.glmnet(na.omit(scale(as.matrix(data))), group, alpha=alphas[i], nfolds=10, family="binomial")
    loc <- which(cvfits$lambda==cvfits$lambda.min)
    maxes[i] <- cvfits$lambda %>% max
    mins[i] <- cvfits$lambda %>% min
    mses[i] <- cvfits$cvm[loc]
  }
  
  cvFit <- data.frame(mse=mses, alpha=alphas)
  cvFit$mse <- caTools::runmean(cvFit$mse,10)
  alpha <- cvFit$alpha[which(cvFit$mse==min(cvFit$mse))]
  q <- ggplot(cvFit,aes(x=alpha,y=mse))
  q <- q + geom_point(size=5)
  q <- q + scale_x_continuous("Alpha parameter (0=ridge, 1=lasso)")
  q <- q + scale_y_continuous("LOOCV mean squared error (moving average of width 10 points")
  q <- SMAOFormatGGPlot(q, sizeMultiplier=2)
  SMAOpng(paste0("results/en_",file,"_error.png"),w=1)
  print(q)
  dev.off()
  
  if(lasso){
    alpha=1
  } 
  fit <- glmnet::cv.glmnet(na.omit(scale(as.matrix(data))), group, alpha=alpha, family="binomial")
  oddsRatios <- data.frame((matrix(coef(fit))))
  oddsRatios$names <- row.names(coef(fit))
  row.names(oddsRatios) <- NULL
  oddsRatios <- oddsRatios[-1,]
  names(oddsRatios)[1] <- "OR"
  oddsRatios$names <- as.character(oddsRatios$names)
  #oddsRatios <- oddsRatios[oddsRatios$OR!=0,]
  oddsRatios$alpha <- alpha
  saveRDS(oddsRatios,file=paste0("results/en_",file,"_OR.RDS"))
}

RunMultipleENs <- function(
  d,
  pganalysis1,
  pganalysis2,
  pganalysis3,
  ppanalysis1,
  ppanalysis2,
  ppanalysis3,
  naming,
  lasso=FALSE,
  modelPG=NULL,modelPP=NULL){
    
  if(!is.null(pganalysis1)) RunEN(data=d$data[pganalysis1,c(d$namesLog2IMPG,modelPG)],
          group=d$data$o_pp_sensitive_1[pganalysis1], 
          file=paste0(naming,"-IMPG_pp_sensitive_1"),lasso=lasso)
  
  if(!is.null(pganalysis2)) RunEN(data=d$data[pganalysis2,c(d$namesLog2IMPG,modelPG)],
          group=d$data$o_pp_sensitive_2[pganalysis2], 
          file=paste0(naming,"-IMPG_pp_sensitive_2"),lasso=lasso)
  
  if(!is.null(pganalysis3)) RunEN(data=d$data[pganalysis3,c(d$namesLog2IMPG,modelPG)],
          group=d$data$o_pp_sensitive_3[pganalysis3], 
          file=paste0(naming,"-IMPG_pp_sensitive_3"),lasso=lasso)
  
  if(!is.null(ppanalysis1)) RunEN(data=d$data[ppanalysis1,c(d$namesLog2IMPP,modelPP)],
          group=d$data$o_pp_sensitive_1[ppanalysis1], 
          file=paste0(naming,"-IMPP_pp_sensitive_1"),lasso=lasso)
  
  if(!is.null(ppanalysis2)) RunEN(data=d$data[ppanalysis2,c(d$namesLog2IMPP,modelPP)],
          group=d$data$o_pp_sensitive_2[ppanalysis2], 
          file=paste0(naming,"-IMPP_pp_sensitive_2"),lasso=lasso)
  
  if(!is.null(ppanalysis3)) RunEN(data=d$data[ppanalysis3,c(d$namesLog2IMPP,modelPP)],
          group=d$data$o_pp_sensitive_3[ppanalysis3], 
          file=paste0(naming,"-IMPP_pp_sensitive_3"),lasso=lasso)

  
  dxres <- list()
  if(!is.null(pganalysis1)){
    dx <- readRDS(file=paste0("results/en_",naming,"-IMPG_pp_sensitive_1","_OR.RDS"))
    if(nrow(dx)>1){
      dx$names <- gsub("_pg$","",dx$names)
      dx$outcome <- c("-- vs -+")
      dx$exposure <- "PG"
      dxres[[length(dxres)+1]] <- dx
    }
  }
  if(!is.null(pganalysis2)){
    dx <- readRDS(paste0("results/en_",naming,"-IMPG_pp_sensitive_2","_OR.RDS"))
    if(nrow(dx)>1){
      dx$names <- gsub("_pg$","",dx$names)
      dx$outcome <- c("x- vs x+")
      dx$exposure <- "PG"
      dxres[[length(dxres)+1]] <- dx
    }
  }
  
  if(!is.null(pganalysis3)){
    dx <- readRDS(paste0("results/en_",naming,"-IMPG_pp_sensitive_3","_OR.RDS"))
    if(nrow(dx)>1){
      dx$names <- gsub("_pg$","",dx$names)
      dx$outcome <- c("-x vs +x")
      dx$exposure <- "PG"
      dxres[[length(dxres)+1]] <- dx
    }
  }
  
  if(!is.null(ppanalysis1)){
    dx <- readRDS(paste0("results/en_",naming,"-IMPP_pp_sensitive_1","_OR.RDS"))
    if(nrow(dx)>1){
      dx$names <- gsub("_pp$","",dx$names)
      dx$outcome <- c("-- vs -+")
      dx$exposure <- "PP"
      dxres[[length(dxres)+1]] <- dx
    }
  }
  
  if(!is.null(ppanalysis2)){
    dx <- readRDS(paste0("results/en_",naming,"-IMPP_pp_sensitive_2","_OR.RDS"))
    if(nrow(dx)>1){
      dx$names <- gsub("_pp$","",dx$names)
      dx$outcome <- c("x- vs x+")
      dx$exposure <- "PP"
      dxres[[length(dxres)+1]] <- dx
    }
  }
  
  if(!is.null(ppanalysis3)){
    dx <- readRDS(paste0("results/en_",naming,"-IMPP_pp_sensitive_3","_OR.RDS"))
    if(nrow(dx)>1){
      dx$names <- gsub("_pp$","",dx$names)
      dx$outcome <- c("-x vs +x")
      dx$exposure <- "PP"
      dxres[[length(dxres)+1]] <- dx
    }
  }
  
  if(length(dxres)>0){
    dxres <- rbindlist(dxres)
    if(lasso){
      dxres[,method:="LASSO"]
    } else dxres[,method:="Elastic-net"]
  } else dxres <- NULL
  saveRDS(dxres,file=paste0("results/en_",naming,"-loadings.RDS"))
  
}



RunLogistic <- function(data, group, file, adjust=NULL){
  res <- data.frame(names=names(data),stringsAsFactors=FALSE)
  res$OR <- 0
  res$pval <- 100
  for(i in 1:nrow(res)){
    form <- paste0("group~",res$names[i])
    if(!is.null(adjust)){
      for(j in adjust) form <- paste0(form,"+",j)
    }
    fit <- glm(as.formula(form),data=data,family=binomial())
    if(nrow(summary(fit)$coefficients)>=2){
      res$OR[i] <- summary(fit)$coefficients[2,1]
      res$pval[i] <- summary(fit)$coefficients[2,4]
    }
  }
  
  saveRDS(res,file=paste0("results/logistic_",file,"_loadings.RDS"))
  
  res$pval <- res$pval*nrow(res)
  saveRDS(res,file=paste0("results/bonf_",file,"_loadings.RDS"))
}


RunMultipleLogistics <- function(
  d,
  pganalysis1,
  pganalysis2,
  pganalysis3,
  ppanalysis1,
  ppanalysis2,
  ppanalysis3,
  naming,
  modelPG=NULL,modelPP=NULL,adjust=FALSE){
  if(adjust==FALSE){
    adjustPG=NULL
    adjustPP=NULL
  } else {
    adjustPG=modelPG
    adjustPP=modelPP
  }
  
  if(!is.null(pganalysis1)) RunLogistic(data=d$data[pganalysis1,c(d$namesLog2IMPG,modelPG)],
                                  group=d$data$o_pp_sensitive_1[pganalysis1], 
                                  file=paste0(naming,"-IMPG_pp_sensitive_1"),adjust=adjustPG)
  
  if(!is.null(pganalysis2)) RunLogistic(data=d$data[pganalysis2,c(d$namesLog2IMPG,modelPG)],
                                  group=d$data$o_pp_sensitive_2[pganalysis2], 
                                  file=paste0(naming,"-IMPG_pp_sensitive_2"),adjust=adjustPG)
  
  if(!is.null(pganalysis3)) RunLogistic(data=d$data[pganalysis3,c(d$namesLog2IMPG,modelPG)],
                                  group=d$data$o_pp_sensitive_3[pganalysis3], 
                                  file=paste0(naming,"-IMPG_pp_sensitive_3"),adjust=adjustPG)
  
  if(!is.null(ppanalysis1)) RunLogistic(data=d$data[ppanalysis1,c(d$namesLog2IMPP,modelPP)],
                                  group=d$data$o_pp_sensitive_1[ppanalysis1], 
                                  file=paste0(naming,"-IMPP_pp_sensitive_1"),adjust=adjustPP)
  
  if(!is.null(ppanalysis2)) RunLogistic(data=d$data[ppanalysis2,c(d$namesLog2IMPP,modelPP)],
                                  group=d$data$o_pp_sensitive_2[ppanalysis2], 
                                  file=paste0(naming,"-IMPP_pp_sensitive_2"),adjust=adjustPP)
  
  if(!is.null(ppanalysis3)) RunLogistic(data=d$data[ppanalysis3,c(d$namesLog2IMPP,modelPP)],
                                  group=d$data$o_pp_sensitive_3[ppanalysis3], 
                                  file=paste0(naming,"-IMPP_pp_sensitive_3"),adjust=adjustPP)
  
  
  dxres <- list()
  if(!is.null(pganalysis1)){
    dx <- readRDS(file=paste0("results/logistic_",naming,"-IMPG_pp_sensitive_1","_loadings.RDS"))
    if(nrow(dx)>1){
      dx$names <- gsub("_pg$","",dx$names)
      dx$outcome <- c("-- vs -+")
      dx$exposure <- "PG"
      dxres[[length(dxres)+1]] <- dx
    }
  }
  if(!is.null(pganalysis2)){
    dx <- readRDS(paste0("results/logistic_",naming,"-IMPG_pp_sensitive_2","_loadings.RDS"))
    if(nrow(dx)>1){
      dx$names <- gsub("_pg$","",dx$names)
      dx$outcome <- c("x- vs x+")
      dx$exposure <- "PG"
      dxres[[length(dxres)+1]] <- dx
    }
  }
  
  if(!is.null(pganalysis3)){
    dx <- readRDS(paste0("results/logistic_",naming,"-IMPG_pp_sensitive_3","_loadings.RDS"))
    if(nrow(dx)>1){
      dx$names <- gsub("_pg$","",dx$names)
      dx$outcome <- c("-x vs +x")
      dx$exposure <- "PG"
      dxres[[length(dxres)+1]] <- dx
    }
  }
  
  if(!is.null(ppanalysis1)){
    dx <- readRDS(paste0("results/logistic_",naming,"-IMPP_pp_sensitive_1","_loadings.RDS"))
    if(nrow(dx)>1){
      dx$names <- gsub("_pp$","",dx$names)
      dx$outcome <- c("-- vs -+")
      dx$exposure <- "PP"
      dxres[[length(dxres)+1]] <- dx
    }
  }
  
  if(!is.null(ppanalysis2)){
    dx <- readRDS(paste0("results/logistic_",naming,"-IMPP_pp_sensitive_2","_loadings.RDS"))
    if(nrow(dx)>1){
      dx$names <- gsub("_pp$","",dx$names)
      dx$outcome <- c("x- vs x+")
      dx$exposure <- "PP"
      dxres[[length(dxres)+1]] <- dx
    }
  }
  
  if(!is.null(ppanalysis3)){
    dx <- readRDS(paste0("results/logistic_",naming,"-IMPP_pp_sensitive_3","_loadings.RDS"))
    if(nrow(dx)>1){
      dx$names <- gsub("_pp$","",dx$names)
      dx$outcome <- c("-x vs +x")
      dx$exposure <- "PP"
      dxres[[length(dxres)+1]] <- dx
    }
  }
  
  if(length(dxres)>0){
    dxres <- rbindlist(dxres)
    dxres[,method:="Logistic"]
  } else dxres <- NULL
  saveRDS(dxres,file=paste0("results/logistic_",naming,"-loadings.RDS"))
  
  
  
  ############BONFERONI
  
  dxres <- list()
  if(!is.null(pganalysis1)){
    dx <- readRDS(file=paste0("results/bonf_",naming,"-IMPG_pp_sensitive_1","_loadings.RDS"))
    if(nrow(dx)>1){
      dx$names <- gsub("_pg$","",dx$names)
      dx$outcome <- c("-- vs -+")
      dx$exposure <- "PG"
      dxres[[length(dxres)+1]] <- dx
    }
  }
  if(!is.null(pganalysis2)){
    dx <- readRDS(paste0("results/bonf_",naming,"-IMPG_pp_sensitive_2","_loadings.RDS"))
    if(nrow(dx)>1){
      dx$names <- gsub("_pg$","",dx$names)
      dx$outcome <- c("x- vs x+")
      dx$exposure <- "PG"
      dxres[[length(dxres)+1]] <- dx
    }
  }
  
  if(!is.null(pganalysis3)){
    dx <- readRDS(paste0("results/bonf_",naming,"-IMPG_pp_sensitive_3","_loadings.RDS"))
    if(nrow(dx)>1){
      dx$names <- gsub("_pg$","",dx$names)
      dx$outcome <- c("-x vs +x")
      dx$exposure <- "PG"
      dxres[[length(dxres)+1]] <- dx
    }
  }
  
  if(!is.null(ppanalysis1)){
    dx <- readRDS(paste0("results/bonf_",naming,"-IMPP_pp_sensitive_1","_loadings.RDS"))
    if(nrow(dx)>1){
      dx$names <- gsub("_pp$","",dx$names)
      dx$outcome <- c("-- vs -+")
      dx$exposure <- "PP"
      dxres[[length(dxres)+1]] <- dx
    }
  }
  
  if(!is.null(ppanalysis2)){
    dx <- readRDS(paste0("results/bonf_",naming,"-IMPP_pp_sensitive_2","_loadings.RDS"))
    if(nrow(dx)>1){
      dx$names <- gsub("_pp$","",dx$names)
      dx$outcome <- c("x- vs x+")
      dx$exposure <- "PP"
      dxres[[length(dxres)+1]] <- dx
    }
  }
  
  if(!is.null(ppanalysis3)){
    dx <- readRDS(paste0("results/bonf_",naming,"-IMPP_pp_sensitive_3","_loadings.RDS"))
    if(nrow(dx)>1){
      dx$names <- gsub("_pp$","",dx$names)
      dx$outcome <- c("-x vs +x")
      dx$exposure <- "PP"
      dxres[[length(dxres)+1]] <- dx
    }
  }
  
  if(length(dxres)>0){
    dxres <- rbindlist(dxres)
    dxres[,method:="Bonferroni Logistic"]
  } else dxres <- NULL
  saveRDS(dxres,file=paste0("results/bonf_",naming,"-loadings.RDS"))
  
}

###############
##############
#############
##############
##############



RunWilcoxon <- function(data, group, file){
  res <- data.frame(names=names(data),stringsAsFactors=FALSE)
  res$OR <- 0
  res$pval <- 100
  for(i in 1:nrow(res)){
    form <- paste0("group~",res$names[i])
    fit <- glm(as.formula(form),data=data,family=binomial())
    if(nrow(summary(fit)$coefficients)==2){
      res$OR[i] <- summary(fit)$coefficients[2,1]
      form <- paste0("as.numeric(data$",res$names[i],")~group")
      fit <- wilcox.test(as.formula(form))
      res$pval[i] <- fit$p.value
    }
  }
  
  saveRDS(res,file=paste0("results/wilcoxon_",file,"_loadings.RDS"))
  
  res$pval <- res$pval*nrow(res)
  saveRDS(res,file=paste0("results/bonfwilcoxon_",file,"_loadings.RDS"))
}


RunMultipleWilcoxon <- function(
  d,
  pganalysis1,
  pganalysis2,
  pganalysis3,
  ppanalysis1,
  ppanalysis2,
  ppanalysis3,
  naming,
  modelPG=NULL,modelPP=NULL){
  
  if(!is.null(pganalysis1)) RunWilcoxon(data=d$data[pganalysis1,c(d$namesLog2IMPG,modelPG)],
                                        group=d$data$o_pp_sensitive_1[pganalysis1], 
                                        file=paste0(naming,"-IMPG_pp_sensitive_1"))
  
  if(!is.null(pganalysis2)) RunWilcoxon(data=d$data[pganalysis2,c(d$namesLog2IMPG,modelPG)],
                                        group=d$data$o_pp_sensitive_2[pganalysis2], 
                                        file=paste0(naming,"-IMPG_pp_sensitive_2"))
  
  if(!is.null(pganalysis3)) RunWilcoxon(data=d$data[pganalysis3,c(d$namesLog2IMPG,modelPG)],
                                        group=d$data$o_pp_sensitive_3[pganalysis3], 
                                        file=paste0(naming,"-IMPG_pp_sensitive_3"))
  
  if(!is.null(ppanalysis1)) RunWilcoxon(data=d$data[ppanalysis1,c(d$namesLog2IMPP,modelPP)],
                                        group=d$data$o_pp_sensitive_1[ppanalysis1], 
                                        file=paste0(naming,"-IMPP_pp_sensitive_1"))
  
  if(!is.null(ppanalysis2)) RunWilcoxon(data=d$data[ppanalysis2,c(d$namesLog2IMPP,modelPP)],
                                        group=d$data$o_pp_sensitive_2[ppanalysis2], 
                                        file=paste0(naming,"-IMPP_pp_sensitive_2"))
  
  if(!is.null(ppanalysis3)) RunWilcoxon(data=d$data[ppanalysis3,c(d$namesLog2IMPP,modelPP)],
                                        group=d$data$o_pp_sensitive_3[ppanalysis3], 
                                        file=paste0(naming,"-IMPP_pp_sensitive_3"))
  
  
  dxres <- list()
  if(!is.null(pganalysis1)){
    dx <- readRDS(file=paste0("results/wilcoxon_",naming,"-IMPG_pp_sensitive_1","_loadings.RDS"))
    if(nrow(dx)>1){
      dx$names <- gsub("_pg$","",dx$names)
      dx$outcome <- c("-- vs -+")
      dx$exposure <- "PG"
      dxres[[length(dxres)+1]] <- dx
    }
  }
  if(!is.null(pganalysis2)){
    dx <- readRDS(paste0("results/wilcoxon_",naming,"-IMPG_pp_sensitive_2","_loadings.RDS"))
    if(nrow(dx)>1){
      dx$names <- gsub("_pg$","",dx$names)
      dx$outcome <- c("x- vs x+")
      dx$exposure <- "PG"
      dxres[[length(dxres)+1]] <- dx
    }
  }
  
  if(!is.null(pganalysis3)){
    dx <- readRDS(paste0("results/wilcoxon_",naming,"-IMPG_pp_sensitive_3","_loadings.RDS"))
    if(nrow(dx)>1){
      dx$names <- gsub("_pg$","",dx$names)
      dx$outcome <- c("-x vs +x")
      dx$exposure <- "PG"
      dxres[[length(dxres)+1]] <- dx
    }
  }
  
  if(!is.null(ppanalysis1)){
    dx <- readRDS(paste0("results/wilcoxon_",naming,"-IMPP_pp_sensitive_1","_loadings.RDS"))
    if(nrow(dx)>1){
      dx$names <- gsub("_pp$","",dx$names)
      dx$outcome <- c("-- vs -+")
      dx$exposure <- "PP"
      dxres[[length(dxres)+1]] <- dx
    }
  }
  
  if(!is.null(ppanalysis2)){
    dx <- readRDS(paste0("results/wilcoxon_",naming,"-IMPP_pp_sensitive_2","_loadings.RDS"))
    if(nrow(dx)>1){
      dx$names <- gsub("_pp$","",dx$names)
      dx$outcome <- c("x- vs x+")
      dx$exposure <- "PP"
      dxres[[length(dxres)+1]] <- dx
    }
  }
  
  if(!is.null(ppanalysis3)){
    dx <- readRDS(paste0("results/wilcoxon_",naming,"-IMPP_pp_sensitive_3","_loadings.RDS"))
    if(nrow(dx)>1){
      dx$names <- gsub("_pp$","",dx$names)
      dx$outcome <- c("-x vs +x")
      dx$exposure <- "PP"
      dxres[[length(dxres)+1]] <- dx
    }
  }
  
  if(length(dxres)>0){
    dxres <- rbindlist(dxres)
    dxres[,method:="Wilcoxon"]
  } else dxres <- NULL
  saveRDS(dxres,file=paste0("results/wilcoxon_",naming,"-loadings.RDS"))
  
  
  
  ############BONFERONI
  
  dxres <- list()
  if(!is.null(pganalysis1)){
    dx <- readRDS(file=paste0("results/bonfwilcoxon_",naming,"-IMPG_pp_sensitive_1","_loadings.RDS"))
    if(nrow(dx)>1){
      dx$names <- gsub("_pg$","",dx$names)
      dx$outcome <- c("-- vs -+")
      dx$exposure <- "PG"
      dxres[[length(dxres)+1]] <- dx
    }
  }
  if(!is.null(pganalysis2)){
    dx <- readRDS(paste0("results/bonfwilcoxon_",naming,"-IMPG_pp_sensitive_2","_loadings.RDS"))
    if(nrow(dx)>1){
      dx$names <- gsub("_pg$","",dx$names)
      dx$outcome <- c("x- vs x+")
      dx$exposure <- "PG"
      dxres[[length(dxres)+1]] <- dx
    }
  }
  
  if(!is.null(pganalysis3)){
    dx <- readRDS(paste0("results/bonfwilcoxon_",naming,"-IMPG_pp_sensitive_3","_loadings.RDS"))
    if(nrow(dx)>1){
      dx$names <- gsub("_pg$","",dx$names)
      dx$outcome <- c("-x vs +x")
      dx$exposure <- "PG"
      dxres[[length(dxres)+1]] <- dx
    }
  }
  
  if(!is.null(ppanalysis1)){
    dx <- readRDS(paste0("results/bonfwilcoxon_",naming,"-IMPP_pp_sensitive_1","_loadings.RDS"))
    if(nrow(dx)>1){
      dx$names <- gsub("_pp$","",dx$names)
      dx$outcome <- c("-- vs -+")
      dx$exposure <- "PP"
      dxres[[length(dxres)+1]] <- dx
    }
  }
  
  if(!is.null(ppanalysis2)){
    dx <- readRDS(paste0("results/bonfwilcoxon_",naming,"-IMPP_pp_sensitive_2","_loadings.RDS"))
    if(nrow(dx)>1){
      dx$names <- gsub("_pp$","",dx$names)
      dx$outcome <- c("x- vs x+")
      dx$exposure <- "PP"
      dxres[[length(dxres)+1]] <- dx
    }
  }
  
  if(!is.null(ppanalysis3)){
    dx <- readRDS(paste0("results/bonfwilcoxon_",naming,"-IMPP_pp_sensitive_3","_loadings.RDS"))
    if(nrow(dx)>1){
      dx$names <- gsub("_pp$","",dx$names)
      dx$outcome <- c("-x vs +x")
      dx$exposure <- "PP"
      dxres[[length(dxres)+1]] <- dx
    }
  }
  
  if(length(dxres)>0){
    dxres <- rbindlist(dxres)
    dxres[,method:="Bonferroni Wilcoxon"]
  } else dxres <- NULL
  saveRDS(dxres,file=paste0("results/bonfwilcoxon_",naming,"-loadings.RDS"))
  
  
  
  
}


###############
###############
###############
###############
###############


RunLASSO <- function(data, group, file){
  a=covTest::lars.glm(as.matrix(data),as.numeric(group)-1,family="binomial")
  a$call <- gsub("^covTest::","",a$call)
  final <- newCovTest(a,as.matrix(data),as.numeric(group)-1)
  final <- na.omit(data.frame(final$results))
  final$importance <- 1:nrow(final)
  final$inclModel <- FALSE
  for(i in 1:nrow(final)){
    if(final$P.value[i]<0.05){
      final$inclModel[i] <- TRUE
    } else break
  }
  results <- data.frame(names=names(data),stringsAsFactors=FALSE)
  results$OR <- a$beta[sum(final$inclModel)+1,]
  results$alpha <- 1
  
  results <- results[,c("OR","names","alpha")]
  
  saveRDS(results,file=paste0("results/en_",file,"_OR.RDS"))
}

RunMultipleLASSOs <- function(
  d,
  pganalysis1,
  pganalysis2,
  pganalysis3,
  ppanalysis1,
  ppanalysis2,
  ppanalysis3,
  naming,
  modelPG=NULL,modelPP=NULL){
  
  if(!is.null(pganalysis1)) RunLASSO(data=d$data[pganalysis1,c(d$namesLog2IMPG,modelPG)],
                                  group=d$data$o_pp_sensitive_1[pganalysis1], 
                                  file=paste0(naming,"-IMPG_pp_sensitive_1"))
  
  if(!is.null(pganalysis2)) RunLASSO(data=d$data[pganalysis2,c(d$namesLog2IMPG,modelPG)],
                                  group=d$data$o_pp_sensitive_2[pganalysis2], 
                                  file=paste0(naming,"-IMPG_pp_sensitive_2"))
  
  if(!is.null(pganalysis3)) RunLASSO(data=d$data[pganalysis3,c(d$namesLog2IMPG,modelPG)],
                                  group=d$data$o_pp_sensitive_3[pganalysis3], 
                                  file=paste0(naming,"-IMPG_pp_sensitive_3"))
  
  if(!is.null(ppanalysis1)) RunLASSO(data=d$data[ppanalysis1,c(d$namesLog2IMPP,modelPP)],
                                  group=d$data$o_pp_sensitive_1[ppanalysis1], 
                                  file=paste0(naming,"-IMPP_pp_sensitive_1"))
  
  if(!is.null(ppanalysis2)) RunLASSO(data=d$data[ppanalysis2,c(d$namesLog2IMPP,modelPP)],
                                  group=d$data$o_pp_sensitive_2[ppanalysis2], 
                                  file=paste0(naming,"-IMPP_pp_sensitive_2"))
  
  if(!is.null(ppanalysis3)) RunLASSO(data=d$data[ppanalysis3,c(d$namesLog2IMPP,modelPP)],
                                  group=d$data$o_pp_sensitive_3[ppanalysis3], 
                                  file=paste0(naming,"-IMPP_pp_sensitive_3"))
  
  
  dxres <- list()
  if(!is.null(pganalysis1)){
    dx <- readRDS(file=paste0("results/en_",naming,"-IMPG_pp_sensitive_1","_OR.RDS"))
    if(nrow(dx)>1){
      dx$names <- gsub("_pg$","",dx$names)
      dx$outcome <- c("-- vs -+")
      dx$exposure <- "PG"
      dxres[[length(dxres)+1]] <- dx
    }
  }
  if(!is.null(pganalysis2)){
    dx <- readRDS(paste0("results/en_",naming,"-IMPG_pp_sensitive_2","_OR.RDS"))
    if(nrow(dx)>1){
      dx$names <- gsub("_pg$","",dx$names)
      dx$outcome <- c("x- vs x+")
      dx$exposure <- "PG"
      dxres[[length(dxres)+1]] <- dx
    }
  }
  
  if(!is.null(pganalysis3)){
    dx <- readRDS(paste0("results/en_",naming,"-IMPG_pp_sensitive_3","_OR.RDS"))
    if(nrow(dx)>1){
      dx$names <- gsub("_pg$","",dx$names)
      dx$outcome <- c("-x vs +x")
      dx$exposure <- "PG"
      dxres[[length(dxres)+1]] <- dx
    }
  }
  
  if(!is.null(ppanalysis1)){
    dx <- readRDS(paste0("results/en_",naming,"-IMPP_pp_sensitive_1","_OR.RDS"))
    if(nrow(dx)>1){
      dx$names <- gsub("_pp$","",dx$names)
      dx$outcome <- c("-- vs -+")
      dx$exposure <- "PP"
      dxres[[length(dxres)+1]] <- dx
    }
  }
  
  if(!is.null(ppanalysis2)){
    dx <- readRDS(paste0("results/en_",naming,"-IMPP_pp_sensitive_2","_OR.RDS"))
    if(nrow(dx)>1){
      dx$names <- gsub("_pp$","",dx$names)
      dx$outcome <- c("x- vs x+")
      dx$exposure <- "PP"
      dxres[[length(dxres)+1]] <- dx
    }
  }
  
  if(!is.null(ppanalysis3)){
    dx <- readRDS(paste0("results/en_",naming,"-IMPP_pp_sensitive_3","_OR.RDS"))
    if(nrow(dx)>1){
      dx$names <- gsub("_pp$","",dx$names)
      dx$outcome <- c("-x vs +x")
      dx$exposure <- "PP"
      dxres[[length(dxres)+1]] <- dx
    }
  }
  
  if(length(dxres)>0){
    dxres <- rbindlist(dxres)
    dxres[,method:="LASSO"]
  } else dxres <- NULL
  saveRDS(dxres,file=paste0("results/en_",naming,"-loadings.RDS"))
  
}









