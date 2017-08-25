CompareSerumToNotSerum <- function(d){
  q <- ggplot(d$data[!is.na(d$data$c_bloodSerumPG),], aes(y=meanZPG,x=c_bloodSerumPG))
  q <- q + geom_boxplot()
  q <- SMAOFormatGGPlot(q, sizeMultiplier=1)
  q <- q + scale_x_discrete("Pregnancy sample taken from serum")
  q <- q + scale_y_continuous(label(d$data$meanZPG))
  SMAOpng(paste0("results/boxplots_serum_PG.png"), landscape=FALSE,w=0.5,h=0.3)
  print(q)
  dev.off()
  
  q <- ggplot(d$data[!is.na(d$data$c_bloodSerumPP),], aes(y=meanZPP,x=c_bloodSerumPP))
  q <- q + geom_boxplot()
  q <- SMAOFormatGGPlot(q, sizeMultiplier=1)
  q <- q + scale_x_discrete("Post-partum sample taken from serum")
  q <- q + scale_y_continuous(label(d$data$meanZPP))
  SMAOpng(paste0("results/boxplots_serum_PP.png"), landscape=FALSE,w=0.5,h=0.3)
  print(q)
  dev.off()
}

ZScoreAnalyses <- function(d){
  
  #for(i in c("all","antiNSAIDS","smoker","inflam","rheum","asthma", "twin", "earlierdep", "caesarean")){
  for(i in c("all", "earlierdep")){
    print(i)
    if(i=="all"){
      suffix <- ""
    } else suffix <- paste0("_",i)
    
    for(outcome in 1:3)
      for(exposure in c("PG","PP","downPG","downPP","upPG","upPP","diff"))
        for(m in c("complex")) {
          if(exposure%in%c("PG","downPG","upPG")){
            if(m=="simple"){
              model <- d$modelPG1
            } else if(m=="complex"){
              model <- d$modelPG2
            }
          } else if(exposure %in% c("PP","downPP","upPP","diff")){
            if(m=="simple"){
              model <- d$modelPP1
            } else if(m=="complex"){
              model <- d$modelPP2
            }
          }
          expstrata <- exposure
          if(expstrata=="diff") expstrata <- "PP"
          if(expstrata %in% c("downPG","upPG")) expstrata <- "PG"
          if(expstrata %in% c("downPP","upPP")) expstrata <- "PP"
          
          if(i=="earlierdep"){
            model <- model[!model %in% "c_earlierDepression"]
            #if(outcome==1 & exposure=="PP" & m=="complex") model <- model[!model %in% "c_medPPNSAID"]
          }
          if(exposure=="diff"){
            if(outcome==1) next
            model <- c("c_age")
          }
          x <- d$data[d[[paste0("analysisDAPCIM",expstrata,"_pp_sensitive_",outcome,suffix)]],c(
            paste0("o_pp_sensitive_",outcome),
            model,
            paste0("meanZ",exposure)
          )]
          
          fit <- glm(as.formula(paste0("o_pp_sensitive_",outcome,"~.")),data=x,family=binomial())
          #fit <- lm(as.formula(paste0("as.numeric(o_pp_sensitive_",outcome,")-1~.")),data=x)
          tab <- Greg::printCrudeAndAdjustedModel(fit,
                                                  ci_lim=c(0,50),
                                                  caption=paste0(label(x[,paste0("o_pp_sensitive_",outcome)]),
                                                                 ": ",
                                                                 i))
          saveRDS(tab,file=paste0("results/meanzscore_",m,"_",outcome,"_",exposure,suffix,".RDS"))
        }
  }
}


ZScoreAnalysesLasso <- function(d){
  for(strata in c("all", "earlierdep")){
    print(strata)
    if(strata=="all"){
      suffix <- ""
    } else suffix <- paste0("_",strata)
    
    for(outcome in 1:2) for(exposure in c("PG")){
      if(exposure%in%c("PG","downPG","upPG")){
        model <- d$modelPG2
      } else if(exposure %in% c("PP","downPP","upPP","diff")){
        model <- d$modelPP2
      }
      if(strata=="earlierdep"){
        model <- model[!model %in% "c_earlierDepression"]
        #if(outcome==1 & exposure=="PP" & m=="complex") model <- model[!model %in% "c_medPPNSAID"]
      }
      expstrata <- exposure
      if(expstrata=="diff") expstrata <- "PP"
      if(expstrata %in% c("downPG","upPG")) expstrata <- "PG"
      if(expstrata %in% c("downPP","upPP")) expstrata <- "PP"
      
      if(exposure=="diff"){
        if(outcome==1) next
        model <- c("c_age")
      }
      x <- d$data[d[[paste0("analysisDAPCIM",expstrata,"_pp_sensitive_",outcome,suffix)]],c(
        paste0("o_pp_sensitive_",outcome),
        model,
        paste0("meanZ",exposure)
      )]
      
      fit <- glm(as.formula(paste0("o_pp_sensitive_",outcome,"~.")),data=x,family=binomial())
      #fit <- lm(as.formula(paste0("as.numeric(o_pp_sensitive_",outcome,")-1~.")),data=x)
      tabFull <- Greg::printCrudeAndAdjustedModel(fit,
                                                  ci_lim=c(0,50),
                                                  caption=paste0(label(x[,paste0("o_pp_sensitive_",outcome)])))
      tabFull <- as.data.frame(tabFull[,1:4])
      tabFull$pval <- coef(summary(fit))[,4]
      
      x <- na.omit(x)
      group <- x[,paste0("o_pp_sensitive_",outcome)]
      data <- x[,-which(names(x)==paste0("o_pp_sensitive_",outcome))]
      
      set.seed(4)
      fit <- glmnet::cv.glmnet(as.matrix(data), group, alpha=1, nfolds=100, family="binomial")

      if(sum(as.numeric(coef(fit))!=0)==1){
        tabLASSO <- tabFull[,3:5]
        for(i in 1:nrow(tabLASSO)) for(j in 1:ncol(tabLASSO)) tabLASSO[i,j] <- NA
      } else {
        vars <- rownames(coef(fit))[as.numeric(coef(fit))!=0]
        vars <- vars[vars!="(Intercept)"]
        fitglm <- glm(as.formula(paste0("o_pp_sensitive_",outcome,"~",paste0(vars,collapse="+"))),data=x,family=binomial())
        tabLASSO <- Greg::printCrudeAndAdjustedModel(fitglm,
                                                     ci_lim=c(0,50),
                                                     caption=paste0(label(x[,paste0("o_pp_sensitive_",outcome)])))
        tabLASSO <- as.data.frame(tabLASSO[,3:4])
        tabLASSO$pval <- coef(summary(fitglm))[,4]
      }
      
      set.seed(4)
      fit <- glmnet::cv.glmnet(as.matrix(data), group, alpha=1, nfolds=100, family="binomial",lambda=c(fit$lambda.min,fit$lambda.min+10))
      if(sum(as.numeric(coef(fit))!=0)==1){
        tabLASSOLoose <- tabFull[,3:5]
        for(i in 1:nrow(tabLASSO)) for(j in 1:ncol(tabLASSO)) tabLASSO[i,j] <- NA
      } else {
        vars <- rownames(coef(fit))[as.numeric(coef(fit))!=0]
        vars <- vars[vars!="(Intercept)"]
        fitglm <- glm(as.formula(paste0("o_pp_sensitive_",outcome,"~",paste0(vars,collapse="+"))),data=x,family=binomial())
        tabLASSOLoose <- Greg::printCrudeAndAdjustedModel(fitglm,
                                                          ci_lim=c(0,50),
                                                          caption=paste0(label(x[,paste0("o_pp_sensitive_",outcome)])))
        tabLASSOLoose <- as.data.frame(tabLASSOLoose[,3:4])
        tabLASSOLoose$pval <- coef(summary(fitglm))[,4]
      }
      
      tabCrude <- as.data.frame(tabFull[,1:2])
      tabCrude$var <- row.names(tabCrude)
      tabLASSO <- as.data.frame(tabLASSO)
      tabLASSO$var <- row.names(tabLASSO)
      m <- merge(tabCrude,tabLASSO,by="var",all.x=TRUE)
      tabLASSOLoose <- as.data.frame(tabLASSOLoose)
      tabLASSOLoose$var <- row.names(tabLASSOLoose)
      m <- merge(m,tabLASSOLoose,by="var",all.x=TRUE)
      tabAdj <- as.data.frame(tabFull[,3:5])
      tabAdj$var <- row.names(tabAdj)
      m <- merge(m,tabAdj,by="var",all.x=TRUE)
      for(i in 1:ncol(m)) m[,i] <- as.character(m[,i])
      for(i in c(6,9,12)) m[,i] <- format(round(as.numeric(m[,i]),2),nsmall=2)
      for(i in c(6,9,12)) m[str_detect(m[,i],"NA"),i] <- NA
      tab <- htmlTable(m[-1,],
                       rnames=FALSE,
                       header=c("Variable","OR","2.5% to 97.5%",rep(c("OR","2.5% to 97.5%","Pval"),3)),
                       cgroup=c("","Crude","LASSO selection&dagger;","Permissive LASSO selection&Dagger;","Full model"),
                       n.cgroup=c(1,2,3,3,3),
                       align="lc",
                       caption=paste0(label(x[,paste0("o_pp_sensitive_",outcome)])," : ",strata),
                       tfoot="&dagger; LASSO's penalisation parameter lambda was selected to be the largest value such that the cross-validated error was within one standard-error of the minimum &Dagger; Permissive LASSO's lambda was selected to minimize the cross-validated error"
      )
      
      saveRDS(tab,file=paste0("results/LASSOmeanzscore_",outcome,"_",exposure,suffix,".RDS"))
    }
  }
  
}


ZScoreAnalysesLassoTest <- function(d){
 # library(lars)
 # library(covTest)
  for(strata in c("all", "earlierdep")){
    print(strata)
    if(strata=="all"){
      suffix <- ""
    } else suffix <- paste0("_",strata)
    
    for(outcome in 1:2) for(exposure in c("PG","PP","downPG","upPG","downPP","upPP")){
      try({
        if(exposure%in%c("PG","downPG","upPG")){
          model <- d$modelPG2
        } else if(exposure %in% c("PP","downPP","upPP","diff")){
          model <- d$modelPP2
        }
        if(strata=="earlierdep"){
          model <- model[!model %in% "c_earlierDepression"]
          #if(outcome==1 & exposure=="PP" & m=="complex") model <- model[!model %in% "c_medPPNSAID"]
        }
        #if(strata=="earlierdep" & outcome==1) next
        expstrata <- exposure
        if(expstrata=="diff") expstrata <- "PP"
        if(expstrata %in% c("downPG","upPG")) expstrata <- "PG"
        if(expstrata %in% c("downPP","upPP")) expstrata <- "PP"
        
        if(exposure=="diff"){
          if(outcome==1) next
          model <- c("c_age")
        }
        x <- d$data[d[[paste0("analysisDAPCIM",expstrata,"_pp_sensitive_",outcome,suffix)]],c(
          paste0("o_pp_sensitive_",outcome),
          model,
          paste0("meanZ",exposure)
        )]
        
        n <- names(x)
        n <- n[-which(n==paste0("o_pp_sensitive_",outcome))]
        fit <- glm(as.formula(paste0("o_pp_sensitive_",outcome,"~.")),data=x,family=binomial())
        
        #fit <- lm(as.formula(paste0("as.numeric(o_pp_sensitive_",outcome,")-1~.")),data=x)
        tabFull <- Greg::printCrudeAndAdjustedModel(fit,
                                                    ci_lim=c(0,50),
                                                    caption=paste0(label(x[,paste0("o_pp_sensitive_",outcome)])),
                                                    desc_column=F)
        descRes <- DescRes(x)
        
        tabFull <- cbind(descRes,as.data.frame(tabFull[,1:4]))
        tabFull$pval <- coef(summary(fit))[,4]
        
        x <- na.omit(x)
        group <- x[,paste0("o_pp_sensitive_",outcome)]
        data <- x[,-which(names(x)==paste0("o_pp_sensitive_",outcome))]
        
        a=covTest::lars.glm(as.matrix(data),as.numeric(group)-1,family="binomial")
        a$call <- gsub("^covTest::","",a$call)
        final <- covTest::covTest(a,as.matrix(data),as.numeric(group)-1)
        final <- data.frame(final$results)
        final$importance <- 1:nrow(final)
        final$inclModel <- FALSE
        for(i in 1:nrow(final)){
          if(final$P.value[i]<0.05){
            final$inclModel[i] <- TRUE
          } else break
        }
        
        finalTable <- data.frame(tabFull)
        finalTable$LASSO[2:nrow(finalTable)] <- format(round(exp(a$beta[1+sum(final$inclModel),]),2),nsmall=2)
        finalTable$lassoPval <- NA
        finalTable$importance <- NA
        for(i in 1:nrow(final)){
          if(final$Predictor_Number[i]<0) next
          finalTable$lassoPval[1+final$Predictor_Number[i]] <- format(round(final$P.value[i],2),nsmall=2)
          finalTable$importance[1+final$Predictor_Number[i]] <- final$importance[i]
        }
        m <- finalTable[-1,]
        m <- cbind(as.character(row.names(m)),m)
        
        for(i in 1:ncol(m)) m[,i] <- as.character(m[,i])
        for(i in 7) m[,i] <- format(round(as.numeric(m[,i]),2),nsmall=2)
        #for(i in c(6,9,12)) m[str_detect(m[,i],"NA"),i] <- NA
        m <- m[,-1]
        names(m) <- c("Mean/n (N)","Crude OR","2.5% to 97.5%","Adj OR","2.5% to 97.5%","Pval","LASSO OR","Pval","Rank")
        openxlsx::write.xlsx(cbind(row.names(m),m),file.path(RPROJ$PROJSHARED,lubridate::today(),paste0("LASSOmeanzscore_",outcome,"_",exposure,suffix,"-",label(x[,paste0("o_pp_sensitive_",outcome)]),".xlsx")))
        capture.output(xtabs(~x[,1]),file=file.path(RPROJ$PROJSHARED,lubridate::today(),paste0("LASSOmeanzscore_",outcome,"_",exposure,suffix,"-",label(x[,paste0("o_pp_sensitive_",outcome)]),"_SAMPLESIZE.txt")))
        
        tab <- pander::pandoc.table.return(m, caption=paste0(label(x[,paste0("o_pp_sensitive_",outcome)])," : ",strata), style="simple", split.tables=Inf, split.cells=Inf)
        #tab <- htmlTable::htmlTable(m,
        #                 rnames=FALSE,
        #                 header=c("Variable","Mean/n (N)","OR","2.5% to 97.5%","OR","2.5% to 97.5%","Pval","OR","Pval","Rank"),
        #                 cgroup=c("","Crude","Adjusted","LASSO"),
        #                 n.cgroup=c(2,2,3,3),
        #                 align="lc",
        #                 caption=paste0(label(x[,paste0("o_pp_sensitive_",outcome)])," : ",strata),
        #                 tfoot="&dagger; LASSO's penalisation parameter lambda was selected to be the largest value such that the cross-validated error was within one standard-error of the minimum &Dagger; Permissive LASSO's lambda was selected to minimize the cross-validated error"
        #)
        
        saveRDS(tab,file=paste0(RPROJ$PROJBAKED,"/LASSOmeanzscore_",outcome,"_",exposure,suffix,".RDS"))
      },TRUE)
    }
  }
}


ZScoreBoxPlots <- function(d){
  plotData <- d$data[,c("CustomDataR","o_pp_sensitive_2","PPREFmeanZupPG","PPREFmeanZdownPG","PPREFmeanZupPP","PPREFmeanZdownPP")]
  plotData <- data.table(na.omit(reshape2::melt(plotData,id=c("CustomDataR","o_pp_sensitive_2"))))
  plotData[,type:="Markers going\nup PP"]
  plotData[variable %in% c("PPREFmeanZdownPG","PPREFmeanZdownPP"),type:="Markers going\ndown PP"]
  plotData[,time:="PG"]
  plotData[variable %in% c("PPREFmeanZdownPP","PPREFmeanZupPP"),time:="PP"]

  q <- ggplot(plotData,aes(x=type,y=value,colour=o_pp_sensitive_2))
  q <- q + geom_boxplot(lwd=1.5,outlier.size=4)
  q <- q + facet_wrap(~time,scales="free")
  q <- q + scale_y_continuous("Z-scores of mean Z-scores of IF marker groups using\nPP values from controls as reference values",lim=c(-10,15))
  q <- q + scale_x_discrete("")
  q <- SMAOFormatGGPlot(q, sizeMultiplier=3)
  q <- q + scale_colour_brewer("",palette="Set1")
  SMAOpng(paste0("results/ZScoreBoxPlotsPPRef.png"), landscape=TRUE)
  print(q)
  dev.off()
  
}



