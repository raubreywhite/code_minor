IndividualIFAnalyses <- function(d){
  
  for(i in c("all","antiNSAIDS","inflamOrRheum", "earlierdep", "caesarean")){
    print(i)
    if(i=="all"){
      suffix <- ""
    } else suffix <- paste0("_",i)
    
    print("EN")
    text=paste0("
                RunMultipleENs(d=d,
                pganalysis1=d$analysisDAPCIMPG_pp_sensitive_1",suffix,",
                pganalysis2=d$analysisDAPCIMPG_pp_sensitive_2",suffix,",
                pganalysis3=d$analysisDAPCIMPG_pp_sensitive_3",suffix,",
                ppanalysis1=d$analysisDAPCIMPP_pp_sensitive_1",suffix,",
                ppanalysis2=d$analysisDAPCIMPP_pp_sensitive_2",suffix,",
                ppanalysis3=d$analysisDAPCIMPP_pp_sensitive_3",suffix,",
                naming=\"",i,"\")")
    text <- gsub("\n","",text)
    text <- gsub(" ","",text)
    eval(parse(text=text))
    
    print("LASSO")
    text=paste0("
                RunMultipleLASSOs(d=d,
                pganalysis1=d$analysisDAPCIMPG_pp_sensitive_1",suffix,",
                pganalysis2=d$analysisDAPCIMPG_pp_sensitive_2",suffix,",
                pganalysis3=d$analysisDAPCIMPG_pp_sensitive_3",suffix,",
                ppanalysis1=d$analysisDAPCIMPP_pp_sensitive_1",suffix,",
                ppanalysis2=d$analysisDAPCIMPP_pp_sensitive_2",suffix,",
                ppanalysis3=d$analysisDAPCIMPP_pp_sensitive_3",suffix,",
                naming=\"",i,"_lasso\")")
    text <- gsub("\n","",text)
    text <- gsub(" ","",text)
    eval(parse(text=text))
    
    print("LOGISTIC")
    text=paste0("
                RunMultipleLogistics(d=d,
                pganalysis1=d$analysisDAPCIMPG_pp_sensitive_1",suffix,",
                pganalysis2=d$analysisDAPCIMPG_pp_sensitive_2",suffix,",
                pganalysis3=d$analysisDAPCIMPG_pp_sensitive_3",suffix,",
                ppanalysis1=d$analysisDAPCIMPP_pp_sensitive_1",suffix,",
                ppanalysis2=d$analysisDAPCIMPP_pp_sensitive_2",suffix,",
                ppanalysis3=d$analysisDAPCIMPP_pp_sensitive_3",suffix,",
                naming=\"",i,"\",
                modelPG=d$modelPG1,modelPP=d$modelPP1)")
    text <- gsub("\n","",text)
    text <- gsub(" ","",text)
    eval(parse(text=text))
    
    print("WILCOXON")
    text=paste0("
                RunMultipleWilcoxon(d=d,
                pganalysis1=d$analysisDAPCIMPG_pp_sensitive_1",suffix,",
                pganalysis2=d$analysisDAPCIMPG_pp_sensitive_2",suffix,",
                pganalysis3=d$analysisDAPCIMPG_pp_sensitive_3",suffix,",
                ppanalysis1=d$analysisDAPCIMPP_pp_sensitive_1",suffix,",
                ppanalysis2=d$analysisDAPCIMPP_pp_sensitive_2",suffix,",
                ppanalysis3=d$analysisDAPCIMPP_pp_sensitive_3",suffix,",
                naming=\"",i,"\",
                modelPG=d$modelPG1,modelPP=d$modelPP1)")
    text <- gsub("\n","",text)
    text <- gsub(" ","",text)
    eval(parse(text=text))
    
    
    print("SUMMARIES")
    text=paste0("
                RunMultipleIFSummaries(d=d,
                pganalysis1=d$analysisDAPCIMPG_pp_sensitive_1",suffix,",
                pganalysis2=d$analysisDAPCIMPG_pp_sensitive_2",suffix,",
                pganalysis3=d$analysisDAPCIMPG_pp_sensitive_3",suffix,",
                ppanalysis1=d$analysisDAPCIMPP_pp_sensitive_1",suffix,",
                ppanalysis2=d$analysisDAPCIMPP_pp_sensitive_2",suffix,",
                ppanalysis3=d$analysisDAPCIMPP_pp_sensitive_3",suffix,",
                naming=\"",i,"\",
                modelPG=d$modelPG1,modelPP=d$modelPP1)")
    text <- gsub("\n","",text)
    text <- gsub(" ","",text)
    eval(parse(text=text))
  }
  
  RunMultipleENs(d=d,
                 pganalysis1=d$analysisDAPCIMPG_pp_sensitive_1_adjusted,
                 pganalysis2=d$analysisDAPCIMPG_pp_sensitive_2_adjusted,
                 pganalysis3=d$analysisDAPCIMPG_pp_sensitive_3_adjusted,
                 ppanalysis1=d$analysisDAPCIMPP_pp_sensitive_1_adjusted,
                 ppanalysis2=d$analysisDAPCIMPP_pp_sensitive_2_adjusted,
                 ppanalysis3=d$analysisDAPCIMPP_pp_sensitive_3_adjusted,
                 naming="adjusted",
                 modelPG=d$modelPG1,modelPP=d$modelPP1)
  
  RunMultipleLASSOs(d=d,
                 pganalysis1=d$analysisDAPCIMPG_pp_sensitive_1_adjusted,
                 pganalysis2=d$analysisDAPCIMPG_pp_sensitive_2_adjusted,
                 pganalysis3=d$analysisDAPCIMPG_pp_sensitive_3_adjusted,
                 ppanalysis1=d$analysisDAPCIMPP_pp_sensitive_1_adjusted,
                 ppanalysis2=d$analysisDAPCIMPP_pp_sensitive_2_adjusted,
                 ppanalysis3=d$analysisDAPCIMPP_pp_sensitive_3_adjusted,
                 naming="adjusted_lasso",
                 modelPG=d$modelPG1,modelPP=d$modelPP1)
  
  print("LOGISTIC")
  RunMultipleLogistics(d=d,
                pganalysis1=d$analysisDAPCIMPG_pp_sensitive_1_adjusted,
                pganalysis2=d$analysisDAPCIMPG_pp_sensitive_2_adjusted,
                pganalysis3=d$analysisDAPCIMPG_pp_sensitive_3_adjusted,
                ppanalysis1=d$analysisDAPCIMPP_pp_sensitive_1_adjusted,
                ppanalysis2=d$analysisDAPCIMPP_pp_sensitive_2_adjusted,
                ppanalysis3=d$analysisDAPCIMPP_pp_sensitive_3_adjusted,
                naming="adjusted",
                modelPG=d$modelPG1,modelPP=d$modelPP1,adjust=TRUE)
  
  lo <- list()
  for(i in c("all","adjusted","antiNSAIDS","inflamOrRheum", "earlierdep", "caesarean")){
    x <- readRDS(paste0("results/logistic_",i,"-loadings.RDS"))
    if(!is.null(x)){
      lo[[length(lo)+1]] <- x
      lo[[length(lo)]]$exclusions <- i
    }
  }
  lo <- rbindlist(lo)
  lo[,supports:="Nothing"]
  lo[pval<0.05 & OR<0,supports:="Controls"]
  lo[pval<0.05 & OR>0,supports:="Cases"]
  lo <- lo[,c("names","outcome","exposure","method","exclusions","supports"),with=FALSE]
  
  bo <- list()
  for(i in c("all","adjusted","antiNSAIDS","inflamOrRheum", "earlierdep", "caesarean")){
    x <- readRDS(paste0("results/bonf_",i,"-loadings.RDS"))
    if(!is.null(x)){
      bo[[length(bo)+1]] <- x
      bo[[length(bo)]]$exclusions <- i
    }
  }
  bo <- rbindlist(bo)
  bo[,supports:="Nothing"]
  bo[pval<0.05 & OR<0,supports:="Controls"]
  bo[pval<0.05 & OR>0,supports:="Cases"]
  bo <- bo[,c("names","outcome","exposure","method","exclusions","supports"),with=FALSE]
  
  #####################
  
  wi <- list()
  for(i in c("all","antiNSAIDS","inflamOrRheum", "earlierdep", "caesarean")){
    x <- readRDS(paste0("results/wilcoxon_",i,"-loadings.RDS"))
    if(!is.null(x)){
      wi[[length(wi)+1]] <- x
      wi[[length(wi)]]$exclusions <- i
    }
  }
  wi <- rbindlist(wi)
  wi[,supports:="Nothing"]
  wi[pval<0.05 & OR<0,supports:="Controls"]
  wi[pval<0.05 & OR>0,supports:="Cases"]
  wi <- wi[,c("names","outcome","exposure","method","exclusions","supports"),with=FALSE]
  
  wibo <- list()
  for(i in c("all","antiNSAIDS","inflamOrRheum", "earlierdep", "caesarean")){
    x <- readRDS(paste0("results/bonfwilcoxon_",i,"-loadings.RDS"))
    if(!is.null(x)){
      wibo[[length(wibo)+1]] <- x
      wibo[[length(wibo)]]$exclusions <- i
    }
  }
  wibo <- rbindlist(wibo)
  wibo[,supports:="Nothing"]
  wibo[pval<0.05 & OR<0,supports:="Controls"]
  wibo[pval<0.05 & OR>0,supports:="Cases"]
  wibo <- wibo[,c("names","outcome","exposure","method","exclusions","supports"),with=FALSE]
  
  #####################
  
  
  en <- list()
  for(i in c("all","adjusted","antiNSAIDS","inflamOrRheum", "earlierdep", "caesarean")){
    x <- readRDS(paste0("results/en_",i,"-loadings.RDS"))
    if(!is.null(x)){
      en[[length(en)+1]] <- x
      en[[length(en)]]$exclusions <- i
    }
  }
  en <- rbindlist(en)
  length(unique(en$names))
  length(d$namesLog2IMPG)
  en[,supports:="Nothing"]
  en[OR<log(1/1.05),supports:="Controls"]
  en[OR>log(1.05),supports:="Cases"]
  en <- en[,c("names","outcome","exposure","method","exclusions","supports"),with=FALSE]
  
  lasso <- list()
  for(i in c("all","adjusted","antiNSAIDS","inflamOrRheum", "earlierdep", "caesarean")){
    x <- readRDS(paste0("results/en_",i,"_lasso-loadings.RDS"))
    if(!is.null(x)){
      lasso[[length(lasso)+1]] <- x
      lasso[[length(lasso)]]$exclusions <- i
    }
  }
  lasso <- rbindlist(lasso)
  length(unique(lasso$names))
  length(d$namesLog2IMPG)
  lasso[,supports:="Nothing"]
  lasso[OR<0,supports:="Controls"]
  lasso[OR>0,supports:="Cases"]
  lasso <- lasso[,c("names","outcome","exposure","method","exclusions","supports"),with=FALSE]
  
  p <- rbind(en,lasso,lo,bo,wi,wibo)
  unique(p$method)
  p[,method:=factor(method,levels=c("Wilcoxon","Bonferroni Wilcoxon","Logistic","Bonferroni Logistic","LASSO","Elastic-net"))]
  levels(p$method) <- c("Mann Whitney U (MWU)","Bonferroni MWU","Logistic","Bonferroni Logistic","LASSO","Elastic-net")
  unique(p$exclusions)
  px <- p[outcome=="-- vs -+" & exclusions=="all"]
  px[,outcome:="x- vs x+"]
  px[,exclusions:="pregdep"]
  p <- rbind(p,px)
  p[,exclusions:=factor(exclusions,levels=c(
    "all",
    "adjusted",
    "pregdep",
    "earlierdep",
    "antiNSAIDS",
    "inflamOrRheum",
    "caesarean"
  ))]
  
  levels(p$exclusions) <- c("All","Adjusted","No preg depr","No earlier depr","No antibiotics/NSAIDs","No inflam/rheum","No caesarean")
  p$names <- gsub("l_im_[0-9][0-9][0-9]_","",p$names)
  p$names <- gsub("_","-",p$names)
  p[,prettyNames:=names]
  setnames(p,"names","name")
  p[name=="c-age",prettyNames:=" Age"]
  p[name=="c-abnormalBMI",prettyNames:=" Abnormal BMI"]
  p[name=="c-educ",prettyNames:=" Education"]
  p[name=="c-chronicInflamOrRheum",prettyNames:=" Chronic inflam/rheum"]
  p[name=="c-fastingAtSample",prettyNames:=" Fasting at sample"]
  p[name=="c-ssriPG",prettyNames:=" SSRIs in PG"]
  p[name=="c-earlierDepression",prettyNames:=" Earlier depression"]
  p[name=="c-babyMale",prettyNames:=" Male baby"]
  p[name=="c-lengthPG",prettyNames:=" PG length"]
  p[name=="c-firstBorn",prettyNames:=" First baby"]
  p[name=="c-caesarean",prettyNames:=" Caesarean"]
  p[name=="c-breastfeedingOnly",prettyNames:=" Exclusive breastfeeding"]
  p[name=="c-medPPNSAID",prettyNames:=" PP NSAIDs"]
  p[name=="c-ssriPP",prettyNames:=" SSRIs in PP"]
  p[name=="c-lengthPP",prettyNames:=" PP length"]
  
  for(i in unique(p$outcome)) for(j in unique(p$exposure)) for(k in c("all","reduced")) {
    if(k=="all"){
      q <- ggplot(p[outcome==i & exposure==j],aes(x=exclusions,y=prettyNames,fill=supports))
    } else {
      q <- ggplot(p[outcome==i & exposure==j & exclusions %in% c("All","Adjusted")],aes(x=exclusions,y=prettyNames,fill=supports))
    }
    q <- q + geom_tile(alpha=0.7,colour="black",lwd=0.2)
    q <- q + facet_wrap(~method,nrow=1)
    q <- RAWmisc::FormatGGPlot(q,xAngle=90, sizeMultiplier = 2)
    q <- q + scale_fill_manual("",values=c("Nothing"="gray","Controls"="green","Cases"="red"))
    #q <- q + labs(title=i)
    q <- q + theme(axis.text.x = element_text(size = 10 * 
                                                2, hjust = 1, vjust = 0.5, colour = "black", 
                                              angle = 90))
    q <- q + scale_x_discrete("", drop=TRUE)
    q <- q + scale_y_discrete("")
    if(k=="all"){
      suffix <- ""
      landscape <- TRUE
    } else {
      suffix <- "_reduced"
      landscape <- FALSE
    }
    SMAOpng(paste0("results/res_",j,"_",gsub(" ","_",i),suffix,".png"),landscape=landscape,h=1.5,w=1.5)
    print(q)
    if(j=="PG"){
      RAWmisc::MakeFootnote("Adjusted analyses uses covariates: PG length, baby sex, fasting at sample, education, earlier depression, chronic inflam/rheum, age, abnormal BMI")
    } else if(j=="PP"){
      RAWmisc::MakeFootnote("Adjusted analyses uses covariates: SSRIs in PP, NSAIDs, PP length, first child, exclusive breastfeeding, education, earlier depression, chronic inflam/rheum, caesarean, age, abnormal BMI")
    }
    dev.off()
  }
  
  summaries <- readRDS(paste0("results/IFsummaries_","all","-loadings.RDS"))
  
  summaries[,controlMean:=format(round(controlMean,2),nsmall=2)]
  
  summaries[,controlSD:=paste0("&plusmn;",format(round(controlSD,2),nsmall=2))]
  summaries[,caseMean:=format(round(caseMean,2),nsmall=2)]
  summaries[,caseSD:=paste0("&plusmn;",format(round(caseSD,2),nsmall=2))]
  
  summaries[,wilcoxpval:=format(round(wilcoxpval,3),nsmall=3)]
  summaries[wilcoxpval=="0.000",wilcoxpval:="<0.001"]
  summaries[,wilcoxpvalbonf:=format(round(wilcoxpvalbonf,3),nsmall=3)]
  summaries[as.numeric(wilcoxpvalbonf)>1,wilcoxpvalbonf:="1.000"]
  summaries[wilcoxpvalbonf=="0.000",wilcoxpvalbonf:="<0.001"]
  summaries[as.numeric(wilcoxpvalbonf)<0.05,wilcoxpvalbonf:=paste0(wilcoxpvalbonf,"*")]
  
  summaries[,OR:=format(round(exp(OR),2),nsmall=2)]
  summaries[,ORpval:=format(round(ORpval,3),nsmall=3)]
  summaries[ORpval=="0.000",ORpval:="<0.001"]
  summaries[,ORpvalbonf:=format(round(ORpvalbonf,3),nsmall=3)]
  summaries[as.numeric(ORpvalbonf)>1,ORpvalbonf:="1.000"]
  summaries[ORpvalbonf=="0.000",ORpvalbonf:="<0.001"]
  summaries[as.numeric(ORpvalbonf)<0.05,ORpvalbonf:=paste0(ORpvalbonf,"*")]
  
  fullSummaries <- summaries
  fullSummaries[,names:=gsub("_","-",gsub("l_im_[0-9][0-9][0-9]_","",names))]
  fullSummaries[,prettyNames:=names]
  setnames(fullSummaries,"names","name")
  fullSummaries[name=="c-age",prettyNames:=" Age"]
  fullSummaries[name=="c-abnormalBMI",prettyNames:=" Abnormal BMI"]
  fullSummaries[name=="c-educ",prettyNames:=" Education"]
  fullSummaries[name=="c-chronicInflamOrRheum",prettyNames:=" Chronic inflam/rheum"]
  fullSummaries[name=="c-fastingAtSample",prettyNames:=" Fasting at sample"]
  fullSummaries[name=="c-ssriPG",prettyNames:=" SSRIs in PG"]
  fullSummaries[name=="c-earlierDepression",prettyNames:=" Earlier depression"]
  fullSummaries[name=="c-babyMale",prettyNames:=" Male baby"]
  fullSummaries[name=="c-lengthPG",prettyNames:=" PG length"]
  fullSummaries[name=="c-firstBorn",prettyNames:=" First baby"]
  fullSummaries[name=="c-caesarean",prettyNames:=" Caesarean"]
  fullSummaries[name=="c-breastfeedingOnly",prettyNames:=" Exclusive breastfeeding"]
  fullSummaries[name=="c-medPPNSAID",prettyNames:=" PP NSAIDs"]
  fullSummaries[name=="c-ssriPP",prettyNames:=" SSRIs in PP"]
  fullSummaries[name=="c-lengthPP",prettyNames:=" PP length"]
  fullSummaries[,name:=prettyNames]
  fullSummaries[,prettyNames:=NULL]
  setorder(fullSummaries,name)
  
  summaries <- fullSummaries[outcome=="x- vs x+" & exposure=="PG"]
  summaries[,outcome:=NULL]
  summaries[,exposure:=NULL]
  summaries <- data.frame(summaries)
  summariesRow <- summaries[1,]
  summariesRow[1,] <- c(
    "IF","N","Mean","SD","N","Mean","SD","Pval","Bonf Pval","OR","Pval","Bonf Pval"
    )
  
  summaries <- rbind(summariesRow,summaries)
  row.names(summaries) <- NULL
  
  for(i in 1:ncol(summaries)) names(summaries)[i] <- "&nbsp;"
  names(summaries)[2] <- "Control"
  names(summaries)[5] <- "Cases"
  names(summaries)[8] <- "Wilcoxon"
  names(summaries)[10] <- "Logistic regression"
  
  tab <- pander::pandoc.table.return(summaries, 
                                     style="simple", split.tables=Inf, split.cells=Inf)
  
  #tab <- htmlTable(summaries,rnames=FALSE,
  #                 header=c("IF","N","Mean","SD","N","Mean","SD","Pval","Bonf Pval","OR","Pval","Bonf Pval"),
  #                 cgroup=c("","Controls","Cases","Wilcoxon","Logistic regression"),
  #                 n.cgroup=c(1,3,3,2,3))
  
  saveRDS(tab, file="results/IFsummariesTablePG.RDS")
  
  summaries <- fullSummaries[outcome=="x- vs x+" & exposure=="PP"]
  summaries[,outcome:=NULL]
  summaries[,exposure:=NULL]
  
  summaries <- data.frame(summaries)
  summariesRow <- summaries[1,]
  summariesRow[1,] <- c(
    "IF","N","Mean","SD","N","Mean","SD","Pval","Bonf Pval","OR","Pval","Bonf Pval"
  )
  
  summaries <- rbind(summariesRow,summaries)
  row.names(summaries) <- NULL
  
  for(i in 1:ncol(summaries)) names(summaries)[i] <- "&nbsp;"
  names(summaries)[2] <- "Control"
  names(summaries)[5] <- "Cases"
  names(summaries)[8] <- "Wilcoxon"
  names(summaries)[10] <- "Logistic regression"
  
  tab <- pander::pandoc.table.return(summaries, 
                                     style="simple", split.tables=Inf, split.cells=Inf)
  #tab <- htmlTable(summaries,rnames=FALSE,
  #                 header=c("IF","N","Mean","SD","N","Mean","SD","Pval","Bonf Pval","OR","Pval","Bonf Pval"),
  #                 cgroup=c("","Controls","Cases","Wilcoxon","Logistic regression"),
  #                 n.cgroup=c(1,3,3,2,3))
  
  saveRDS(tab, file="results/IFsummariesTablePP.RDS")
  
  ######
  
  # SPECIFIC SUMMARIES
  
  
  
  
}

IFSummaryTableInt <- function(summaries, lasso, en,
                              adjlog=NULL,
                              adjlasso=NULL,
                              adjen=NULL,
                              includeCI=FALSE){
  if(includeCI){
    summaries[,ORL95:=OR-1.96*abs(OR/qnorm(1-ORpval/2))]
    summaries[,ORU95:=OR+1.96*abs(OR/qnorm(1-ORpval/2))]
    setcolorder(summaries,c("names", "controlN", "controlMean", "controlSD", 
                            "caseN","caseMean", "caseSD", "wilcoxpval", "wilcoxpvalbonf","OR","ORL95","ORU95","ORpval",
                            "ORpvalbonf" ,"outcome" ,"exposure" ))
    summaries[,ORL95:=format(round(exp(ORL95),2),nsmall=2)]
    summaries[,ORU95:=format(round(exp(ORU95),2),nsmall=2)]
    summaries[ORL95 %in% c("Inf","NaN"),ORL95:="-"]
    summaries[ORU95 %in% c("Inf","NaN"),ORU95:="-"]
  }
  summaries[,controlMean:=format(round(controlMean,2),nsmall=2)]
  summaries[,controlSD:=paste0("&plusmn;",format(round(controlSD,2),nsmall=2))]
  summaries[,caseMean:=format(round(caseMean,2),nsmall=2)]
  summaries[,caseSD:=paste0("&plusmn;",format(round(caseSD,2),nsmall=2))]
  
  summaries[,wilcoxpval:=format(round(wilcoxpval,3),nsmall=3)]
  summaries[wilcoxpval=="0.000",wilcoxpval:="<0.001"]
  summaries[,wilcoxpvalbonf:=format(round(wilcoxpvalbonf,3),nsmall=3)]
  summaries[as.numeric(wilcoxpvalbonf)>1,wilcoxpvalbonf:="1.000"]
  summaries[wilcoxpvalbonf=="0.000",wilcoxpvalbonf:="<0.001"]
  summaries[as.numeric(wilcoxpvalbonf)<0.05,wilcoxpvalbonf:=paste0(wilcoxpvalbonf,"*")]
  
  summaries[,OR:=format(round(exp(OR),2),nsmall=2)]
  summaries[,ORpval:=format(round(ORpval,3),nsmall=3)]
  summaries[ORpval=="0.000",ORpval:="<0.001*"]
  summaries[,ORpvalbonf:=format(round(ORpvalbonf,3),nsmall=3)]
  summaries[as.numeric(ORpvalbonf)>1,ORpvalbonf:="1.000"]
  summaries[ORpvalbonf=="0.000",ORpvalbonf:="<0.001*"]
  summaries[as.numeric(ORpvalbonf)<0.05,ORpvalbonf:=paste0(ORpvalbonf,"*")]
  
  lasso[,alpha:=NULL]
  lasso[,method:=NULL]
  setnames(lasso,"OR","ORlasso")
  lasso[,ORlasso:=format(round(exp(ORlasso),2),nsmall=2)]
  
  en[,alpha:=NULL]
  en[,method:=NULL]
  setnames(en,"OR","ORen")
  en[,ORen:=format(round(exp(ORen),2),nsmall=2)]
  if(!is.null(adjlog)){
    summaries <- merge(summaries,adjlog,by=c("outcome","exposure","names"),all.x=TRUE)
  }
  summaries <- merge(summaries,lasso,by=c("outcome","exposure","names"),all.x=TRUE)
  if(!is.null(adjlog)){
    summaries <- merge(summaries,adjlasso,by=c("outcome","exposure","names"),all.x=TRUE)
  }
  summaries <- merge(summaries,en,by=c("outcome","exposure","names"),all.x=TRUE)
  if(!is.null(adjlog)){
    summaries <- merge(summaries,adjen,by=c("outcome","exposure","names"),all.x=TRUE)
  }
  fullSummaries <- summaries
  
  fullSummaries[wilcoxpval=="0.000",wilcoxpval:="<0.001*"]
  fullSummaries[ORpval=="0.000",ORpval:="<0.001*"]
  fullSummaries[wilcoxpval=="0.000*",wilcoxpval:="<0.001*"]
  fullSummaries[ORpval=="0.000*",ORpval:="<0.001*"]
  fullSummaries[wilcoxpval=="<0.000",wilcoxpval:="<0.001*"]
  fullSummaries[ORpval=="<0.000",ORpval:="<0.001*"]
  
  fullSummaries[wilcoxpvalbonf=="0.000",wilcoxpval:="<0.001*"]
  fullSummaries[ORpvalbonf=="0.000",ORpval:="<0.001*"]
  fullSummaries[wilcoxpvalbonf=="0.000*",wilcoxpval:="<0.001*"]
  fullSummaries[ORpvalbonf=="0.000*",ORpval:="<0.001*"]
  fullSummaries[wilcoxpvalbonf=="<0.000",wilcoxpval:="<0.001*"]
  fullSummaries[ORpvalbonf=="<0.000",ORpval:="<0.001*"]
  
  fullSummaries[wilcoxpval==" 0.000",wilcoxpval:="<0.001*"]
  fullSummaries[ORpval==" 0.000",ORpval:="<0.001*"]
  fullSummaries[wilcoxpval==" 0.000*",wilcoxpval:="<0.001*"]
  fullSummaries[ORpval==" 0.000*",ORpval:="<0.001*"]
  fullSummaries[wilcoxpval==" <0.000",wilcoxpval:="<0.001*"]
  fullSummaries[ORpval==" <0.000",ORpval:="<0.001*"]
  
  fullSummaries[wilcoxpvalbonf==" 0.000",wilcoxpval:="<0.001*"]
  fullSummaries[ORpvalbonf==" 0.000",ORpval:="<0.001*"]
  fullSummaries[wilcoxpvalbonf==" 0.000*",wilcoxpval:="<0.001*"]
  fullSummaries[ORpvalbonf==" 0.000*",ORpval:="<0.001*"]
  fullSummaries[wilcoxpvalbonf==" <0.000",wilcoxpval:="<0.001*"]
  fullSummaries[ORpvalbonf==" <0.000",ORpval:="<0.001*"]
  
  fullSummaries[as.numeric(wilcoxpval)>1,wilcoxpval:="1.000"]
  fullSummaries[as.numeric(wilcoxpvalbonf)>1,wilcoxpvalbonf:="1.000"]
  fullSummaries[as.numeric(ORpval)>1,ORpval:="1.000"]
  fullSummaries[as.numeric(ORpvalbonf)>1,ORpvalbonf:="1.000"]
  
  fullSummaries[,names:=gsub("_","-",gsub("l_im_[0-9][0-9][0-9]_","",names))]
  fullSummaries[,prettyNames:=names]
  setnames(fullSummaries,"names","name")
  fullSummaries[name=="c-age",prettyNames:=" Age"]
  fullSummaries[name=="c-abnormalBMI",prettyNames:=" Abnormal BMI"]
  fullSummaries[name=="c-educ",prettyNames:=" Education"]
  fullSummaries[name=="c-chronicInflamOrRheum",prettyNames:=" Chronic inflam/rheum"]
  fullSummaries[name=="c-fastingAtSample",prettyNames:=" Fasting at sample"]
  fullSummaries[name=="c-ssriPG",prettyNames:=" SSRIs in PG"]
  fullSummaries[name=="c-earlierDepression",prettyNames:=" Earlier depression"]
  fullSummaries[name=="c-babyMale",prettyNames:=" Male baby"]
  fullSummaries[name=="c-lengthPG",prettyNames:=" PG length"]
  fullSummaries[name=="c-firstBorn",prettyNames:=" First baby"]
  fullSummaries[name=="c-caesarean",prettyNames:=" Caesarean"]
  fullSummaries[name=="c-breastfeedingOnly",prettyNames:=" Exclusive breastfeeding"]
  fullSummaries[name=="c-medPPNSAID",prettyNames:=" PP NSAIDs"]
  fullSummaries[name=="c-ssriPP",prettyNames:=" SSRIs in PP"]
  fullSummaries[name=="c-lengthPP",prettyNames:=" PP length"]
  fullSummaries[,name:=prettyNames]
  fullSummaries[,prettyNames:=NULL]
  setorder(fullSummaries,name)
  fullSummaries <- as.data.frame(fullSummaries)
  for(i in 1:ncol(fullSummaries)){
    fullSummaries[,i] <- gsub("<","&lt;",fullSummaries[,i])
  }
  for(i in 2:ncol(fullSummaries)){
    fullSummaries[,i] <- gsub(" ","",fullSummaries[,i])
  }
  fullSummaries <- data.table(fullSummaries)
  return(fullSummaries)
}

IFSummaryTables <- function(){
  
  logAdj <- readRDS(paste0("results/logistic_","adjusted","-loadings.RDS"))
  setnames(logAdj,c("OR","pval"),c("ORlogadj","pvallogadj"))
  logAdj[,method:=NULL]
  bonfAdj <- readRDS(paste0("results/bonf_","adjusted","-loadings.RDS"))
  setnames(bonfAdj,"pval","pvalbonfadj")
  bonfAdj[,method:=NULL]
  bonfAdj[,OR:=NULL]
  log <- merge(logAdj,bonfAdj,by=c("names","outcome","exposure"))
  
  log[,ORlogadjL95:=ORlogadj-1.96*abs(ORlogadj/qnorm(1-pvallogadj/2))]
  log[,ORlogadjU95:=ORlogadj+1.96*abs(ORlogadj/qnorm(1-pvallogadj/2))]
  setcolorder(log,c("names", "outcome", "exposure", "ORlogadj", 
                          "ORlogadjL95","ORlogadjU95", "pvallogadj", "pvalbonfadj"))
  log[,ORlogadjL95:=format(round(exp(ORlogadjL95),2),nsmall=2)]
  log[,ORlogadjU95:=format(round(exp(ORlogadjU95),2),nsmall=2)]
  log[ORlogadjL95 %in% c("Inf","NaN"),ORlogadjL95:="-"]
  log[ORlogadjU95 %in% c("Inf","NaN"),ORlogadjU95:="-"]
  
  log[,ORlogadj:=format(round(exp(ORlogadj),2),nsmall=2)]
  
  log[,pvallogadj:=format(round(pvallogadj,3),nsmall=3)]
  log[as.numeric(pvallogadj)<0.05,pvallogadj:=paste0(pvallogadj,"*")]
  log[pvallogadj=="0.000*",pvallogadj:="<0.001*"]
  
  
  log[,pvalbonfadj:=format(round(pvalbonfadj,3),nsmall=3)]
  log[as.numeric(pvalbonfadj)>1,pvalbonfadj:="1.000"]
  log[as.numeric(pvalbonfadj)<0.05,pvalbonfadj:=paste0(pvalbonfadj,"*")]
  log[pvalbonfadj=="0.000*",pvalbonfadj:="<0.001*"]
  
  lassoAdj <- readRDS(paste0("results/en_","adjusted","_lasso-loadings.RDS"))
  enAdj <- readRDS(paste0("results/en_","adjusted","-loadings.RDS"))
  lassoAdj[,method:=NULL]
  lassoAdj[,alpha:=NULL]
  setnames(lassoAdj,"OR","ORlassoadj")
  lassoAdj[,ORlassoadj:=format(round(exp(ORlassoadj),2),nsmall=2)]
  enAdj[,method:=NULL]
  enAdj[,alpha:=NULL]
  setnames(enAdj,"OR","ORenadj")
  enAdj[,ORenadj:=format(round(exp(ORenadj),2),nsmall=2)]
  
  fullSummaries <- IFSummaryTableInt(
    summaries=readRDS(paste0("results/IFsummaries_","all","-loadings.RDS")),
    lasso=readRDS(paste0("results/en_","all","_lasso-loadings.RDS")),
    en=readRDS(paste0("results/en_","all","-loadings.RDS")),
    adjlog=log,
    adjlasso=lassoAdj,
    adjen=enAdj,
    includeCI=TRUE
    )
  
  tab <- vector("list",4)
  
  summaries <- fullSummaries[outcome=="x- vs x+" & exposure=="PG"]
  summaries[,outcome:=NULL]
  summaries[,exposure:=NULL]
  
  row <- summaries[1]
  for(i in 1:ncol(row)){
    set(row,1,i,c("IF","N","Mean","SD","N","Mean","SD",
                        "Pval","Bonf Pval",
                        "OR","95CIL","95CIU","Pval","Bonf Pval",
                        "aOR","a95CIL","a95CIU","Pval","Bonf Pval",
                        "OR","aOR",
                        "OR","aOR")[i])
  }
  summaries <- rbind(row,summaries)
  for(i in names(summaries)){
    txt <- paste0("summaries[,",i,":=gsub('&plusmn;','±',",i,")]")
    eval(parse(text=txt))
    txt <- paste0("summaries[,",i,":=gsub('&lt;','<',",i,")]")
    eval(parse(text=txt))
    txt <- paste0("summaries[,",i,":=gsub('&nbsp;',' ',",i,")]")
    eval(parse(text=txt))
  }
  
  setnames(summaries,
           c(" ",
             "Controls"," "," ",
             "Cases"," "," ",
             "Wilcoxon"," ",
             "Logistic regression"," "," "," "," ",
             "Adj. logistic regression"," "," "," "," ",
             "LASSO"," ",
             "EN"," "))
  
  openxlsx::write.xlsx(summaries,file=paste0("reports_formatted/supp_all_pg_x-vs_x+.xlsx"))
  tab[["PG x- vs x+"]] <- pander::pandoc.table.return(summaries, 
                                     style="simple", split.tables=Inf, split.cells=Inf)
  #tab[["PG x- vs x+"]] <- htmlTable(summaries,rnames=FALSE,
  #                                  header=c("IF","N","Mean","SD","N","Mean","SD",
  #                                           "Pval","Bonf Pval",
  #                                           "OR","Pval","Bonf Pval",
  #                                           "aOR","Pval","Bonf Pval",
  #                                           "OR","aOR",
  #                                           "OR","aOR"),
  #                                  cgroup=c("","Controls","Cases","Wilcoxon","Logistic regression","Adj. logistic regression","LASSO","EN"),
  #                                  n.cgroup=c(1,3,3,2,3,3,2,2))
  
  summaries <- fullSummaries[outcome=="x- vs x+" & exposure=="PP"]
  summaries[,outcome:=NULL]
  summaries[,exposure:=NULL]
  
  row <- summaries[1]
  for(i in 1:ncol(row)){
    set(row,1,i,c("IF","N","Mean","SD","N","Mean","SD",
                  "Pval","Bonf Pval",
                  "OR","95CIL","95CIU","Pval","Bonf Pval",
                  "aOR","a95CIL","a95CIU","Pval","Bonf Pval",
                  "OR","aOR",
                  "OR","aOR")[i])
  }
  summaries <- rbind(row,summaries)
  for(i in names(summaries)){
    txt <- paste0("summaries[,",i,":=gsub('&plusmn;','±',",i,")]")
    eval(parse(text=txt))
    txt <- paste0("summaries[,",i,":=gsub('&lt;','<',",i,")]")
    eval(parse(text=txt))
    txt <- paste0("summaries[,",i,":=gsub('&nbsp;',' ',",i,")]")
    eval(parse(text=txt))
  }
  
  setnames(summaries,
           c(" ",
             "Controls"," "," ",
             "Cases"," "," ",
             "Wilcoxon"," ",
             "Logistic regression"," "," "," "," ",
             "Adj. logistic regression"," "," "," "," ",
             "LASSO"," ",
             "EN"," "))
  openxlsx::write.xlsx(summaries,file=paste0("reports_formatted/supp_all_pp_x-vs_x+.xlsx"))
  tab[["PP x- vs x+"]] <- pander::pandoc.table.return(summaries, 
                                                      style="simple", split.tables=Inf, split.cells=Inf)
  #tab[["PP x- vs x+"]] <- htmlTable(summaries,rnames=FALSE,
  #                                  header=c("IF","N","Mean","SD","N","Mean","SD",
  #                                           "Pval","Bonf Pval",
  #                                           "OR","Pval","Bonf Pval",
  #                                           "aOR","Pval","Bonf Pval",
  #                                           "OR","aOR",
  #                                           "OR","aOR"),
  #                                  cgroup=c("","Controls","Cases","Wilcoxon","Logistic regression","Adj. logistic regression","LASSO","EN"),
  #                                  n.cgroup=c(1,3,3,2,3,3,2,2))
  #
  summaries <- fullSummaries[outcome=="-- vs -+" & exposure=="PG"]
  summaries[,outcome:=NULL]
  summaries[,exposure:=NULL]
  
  row <- summaries[1]
  for(i in 1:ncol(row)){
    set(row,1,i,c("IF","N","Mean","SD","N","Mean","SD",
                  "Pval","Bonf Pval",
                  "OR","95CIL","95CIU","Pval","Bonf Pval",
                  "aOR","a95CIL","a95CIU","Pval","Bonf Pval",
                  "OR","aOR",
                  "OR","aOR")[i])
  }
  summaries <- rbind(row,summaries)
  for(i in names(summaries)){
    txt <- paste0("summaries[,",i,":=gsub('&plusmn;','±',",i,")]")
    eval(parse(text=txt))
    txt <- paste0("summaries[,",i,":=gsub('&lt;','<',",i,")]")
    eval(parse(text=txt))
    txt <- paste0("summaries[,",i,":=gsub('&nbsp;',' ',",i,")]")
    eval(parse(text=txt))
  }
  
  setnames(summaries,
           c(" ",
             "Controls"," "," ",
             "Cases"," "," ",
             "Wilcoxon"," ",
             "Logistic regression"," "," "," "," ",
             "Adj. logistic regression"," "," "," "," ",
             "LASSO"," ",
             "EN"," "))
  openxlsx::write.xlsx(summaries,file=paste0("reports_formatted/supp_all_pg_--vs_-+.xlsx"))
  tab[["PG -- vs -+"]] <- pander::pandoc.table.return(summaries, 
                                                      style="simple", split.tables=Inf, split.cells=Inf)
  #tab[["PG -- vs -+"]] <- htmlTable(summaries,rnames=FALSE,
  #                                  header=c("IF","N","Mean","SD","N","Mean","SD",
  #                                           "Pval","Bonf Pval",
  #                                           "OR","Pval","Bonf Pval",
  #                                           "aOR","Pval","Bonf Pval",
  #                                           "OR","aOR",
  #                                           "OR","aOR"),
  #                                  cgroup=c("","Controls","Cases","Wilcoxon","Logistic regression","Adj. logistic regression","LASSO","EN"),
  #                                  n.cgroup=c(1,3,3,2,3,3,2,2))
  
  summaries <- fullSummaries[outcome=="-- vs -+" & exposure=="PP"]
  summaries[,outcome:=NULL]
  summaries[,exposure:=NULL]
  
  row <- summaries[1]
  for(i in 1:ncol(row)){
    set(row,1,i,c("IF","N","Mean","SD","N","Mean","SD",
                  "Pval","Bonf Pval",
                  "OR","95CIL","95CIU","Pval","Bonf Pval",
                  "aOR","a95CIL","a95CIU","Pval","Bonf Pval",
                  "OR","aOR",
                  "OR","aOR")[i])
  }
  summaries <- rbind(row,summaries)
  for(i in names(summaries)){
    txt <- paste0("summaries[,",i,":=gsub('&plusmn;','±',",i,")]")
    eval(parse(text=txt))
    txt <- paste0("summaries[,",i,":=gsub('&lt;','<',",i,")]")
    eval(parse(text=txt))
    txt <- paste0("summaries[,",i,":=gsub('&nbsp;',' ',",i,")]")
    eval(parse(text=txt))
  }
  
  setnames(summaries,
           c(" ",
             "Controls"," "," ",
             "Cases"," "," ",
             "Wilcoxon"," ",
             "Logistic regression"," "," "," "," ",
             "Adj. logistic regression"," "," "," "," ",
             "LASSO"," ",
             "EN"," "))
  openxlsx::write.xlsx(summaries,file=paste0("reports_formatted/supp_all_pp_--vs_-+.xlsx"))
  tab[["PP -- vs -+"]] <- pander::pandoc.table.return(summaries, 
                                                      style="simple", split.tables=Inf, split.cells=Inf)
  
  #tab[["PP -- vs -+"]] <- htmlTable(summaries,rnames=FALSE,
  #                                  header=c("IF","N","Mean","SD","N","Mean","SD",
  #                                           "Pval","Bonf Pval",
  #                                           "OR","Pval","Bonf Pval",
  #                                           "aOR","Pval","Bonf Pval",
  #                                           "OR","aOR",
  #                                           "OR","aOR"),
  #                                  cgroup=c("","Controls","Cases","Wilcoxon","Logistic regression","Adj. logistic regression","LASSO","EN"),
  #                                  n.cgroup=c(1,3,3,2,3,3,2,2))
  
  saveRDS(tab, file="results/supplemental_IFsummariesTable_all.RDS")
  
  ##### OTHER
  
  for(i in c("antiNSAIDS","inflamOrRheum", "earlierdep", "caesarean")){
    fullSummaries <- IFSummaryTableInt(
      summaries=readRDS(paste0("results/IFsummaries_",i,"-loadings.RDS")),
      lasso=readRDS(paste0("results/en_",i,"_lasso-loadings.RDS")),
      en=readRDS(paste0("results/en_",i,"-loadings.RDS")),
      includeCI=T
    )
    
    tab <- vector("list",4)
    
    summaries <- fullSummaries[outcome=="x- vs x+" & exposure=="PG"]
    summaries[,outcome:=NULL]
    summaries[,exposure:=NULL]
    
    row <- summaries[1]
    for(j in 1:ncol(row)){
      set(row,1,j,c("IF","N","Mean","SD","N","Mean","SD",
                    "Pval","Bonf Pval",
                    "OR","95CIL","95CIU","Pval","Bonf Pval",
                    "OR",
                    "OR")[j])
    }
    summaries <- rbind(row,summaries)
    for(j in names(summaries)){
      txt <- paste0("summaries[,",j,":=gsub('&plusmn;','±',",j,")]")
      eval(parse(text=txt))
      txt <- paste0("summaries[,",j,":=gsub('&lt;','<',",j,")]")
      eval(parse(text=txt))
      txt <- paste0("summaries[,",j,":=gsub('&nbsp;',' ',",j,")]")
      eval(parse(text=txt))
    }
    setnames(summaries,
             c(" ",
               "Controls"," "," ",
               "Cases"," "," ",
               "Wilcoxon"," ",
               "Logistic regression"," "," "," "," ",
               "LASSO",
               "EN"))
    openxlsx::write.xlsx(summaries,file=paste0("reports_formatted/supp_",i,"_pg_x-vs_x+.xlsx"))
    tab[["PG x- vs x+"]] <- pander::pandoc.table.return(summaries, 
                                                        style="simple", split.tables=Inf, split.cells=Inf)
    
    #tab[["PG x- vs x+"]] <- htmlTable(summaries,rnames=FALSE,
    #                                  header=c("IF","N","Mean","SD","N","Mean","SD","Pval","Bonf Pval","OR","L95","U95","Pval","Bonf Pval","OR","OR"),
    #                                  cgroup=c("","Controls","Cases","Wilcoxon","Logistic regression","LASSO","EN"),
    #                                  n.cgroup=c(1,3,3,2,5,1,1))
    
    summaries <- fullSummaries[outcome=="x- vs x+" & exposure=="PP"]
    summaries[,outcome:=NULL]
    summaries[,exposure:=NULL]
    
    row <- summaries[1]
    for(j in 1:ncol(row)){
      set(row,1,j,c("IF","N","Mean","SD","N","Mean","SD",
                    "Pval","Bonf Pval",
                    "OR","95CIL","95CIU","Pval","Bonf Pval",
                    "OR",
                    "OR")[j])
    }
    summaries <- rbind(row,summaries)
    for(j in names(summaries)){
      txt <- paste0("summaries[,",j,":=gsub('&plusmn;','±',",j,")]")
      eval(parse(text=txt))
      txt <- paste0("summaries[,",j,":=gsub('&lt;','<',",j,")]")
      eval(parse(text=txt))
      txt <- paste0("summaries[,",j,":=gsub('&nbsp;',' ',",j,")]")
      eval(parse(text=txt))
    }
    setnames(summaries,
             c(" ",
               "Controls"," "," ",
               "Cases"," "," ",
               "Wilcoxon"," ",
               "Logistic regression"," "," "," "," ",
               "LASSO",
               "EN"))
    openxlsx::write.xlsx(summaries,file=paste0("reports_formatted/supp_",i,"_pp_x-vs_x+.xlsx"))
    tab[["PP x- vs x+"]] <- pander::pandoc.table.return(summaries, 
                                                        style="simple", split.tables=Inf, split.cells=Inf)
    
    
    summaries <- fullSummaries[outcome=="-- vs -+" & exposure=="PG"]
    summaries[,outcome:=NULL]
    summaries[,exposure:=NULL]
    
    row <- summaries[1]
    for(j in 1:ncol(row)){
      set(row,1,j,c("IF","N","Mean","SD","N","Mean","SD",
                    "Pval","Bonf Pval",
                    "OR","95CIL","95CIU","Pval","Bonf Pval",
                    "OR",
                    "OR")[j])
    }
    summaries <- rbind(row,summaries)
    for(j in names(summaries)){
      txt <- paste0("summaries[,",j,":=gsub('&plusmn;','±',",j,")]")
      eval(parse(text=txt))
      txt <- paste0("summaries[,",j,":=gsub('&lt;','<',",j,")]")
      eval(parse(text=txt))
      txt <- paste0("summaries[,",j,":=gsub('&nbsp;',' ',",j,")]")
      eval(parse(text=txt))
    }
    setnames(summaries,
             c(" ",
               "Controls"," "," ",
               "Cases"," "," ",
               "Wilcoxon"," ",
               "Logistic regression"," "," "," "," ",
               "LASSO",
               "EN"))
    openxlsx::write.xlsx(summaries,file=paste0("reports_formatted/supp_",i,"_pg_--vs_-+.xlsx"))
    tab[["PG -- vs -+"]] <- pander::pandoc.table.return(summaries, 
                                                        style="simple", split.tables=Inf, split.cells=Inf)
    
    summaries <- fullSummaries[outcome=="-- vs -+" & exposure=="PP"]
    summaries[,outcome:=NULL]
    summaries[,exposure:=NULL]
    
    row <- summaries[1]
    for(j in 1:ncol(row)){
      set(row,1,j,c("IF","N","Mean","SD","N","Mean","SD",
                    "Pval","Bonf Pval",
                    "OR","95CIL","95CIU","Pval","Bonf Pval",
                    "OR",
                    "OR")[j])
    }
    summaries <- rbind(row,summaries)
    for(j in names(summaries)){
      txt <- paste0("summaries[,",j,":=gsub('&plusmn;','±',",j,")]")
      eval(parse(text=txt))
      txt <- paste0("summaries[,",j,":=gsub('&lt;','<',",j,")]")
      eval(parse(text=txt))
      txt <- paste0("summaries[,",j,":=gsub('&nbsp;',' ',",j,")]")
      eval(parse(text=txt))
    }
    setnames(summaries,
             c(" ",
               "Controls"," "," ",
               "Cases"," "," ",
               "Wilcoxon"," ",
               "Logistic regression"," "," "," "," ",
               "LASSO",
               "EN"))
    openxlsx::write.xlsx(summaries,file=paste0("reports_formatted/supp_",i,"_pp_--vs_-+.xlsx"))
    tab[["PP -- vs -+"]] <- pander::pandoc.table.return(summaries, 
                                                        style="simple", split.tables=Inf, split.cells=Inf)
    
    
  saveRDS(tab, file=paste0("results/supplemental_IFsummariesTable_",j,".RDS"))
  }
  
}




