SeasonalAnalysis <- function(){

  dir.create(file.path(org::PROJ$SHARED_TODAY,"main_graphs"))
  dir.create(file.path(org::PROJ$SHARED_TODAY,"thesis_graphs"))
  
  toPlotTotal <- vector("list",2)
  for(sens in c("main_analysis","sens_analysis","sub_analysis")){
    dir.create(file.path(org::PROJ$SHARED_TODAY,sens))
    if(sens %in% c("main_analysis","sub_analysis")) dir.create(file.path(org::PROJ$SHARED_TODAY,sens,"scatter_plots"))
    
    sampleSize <- list()
    pg_retval <- pp_retval <- list()
    for(depressed in c("All","Depressed","Not-depressed"))  for(k in 1:2){
      if(sens %in% "main_analysis" & depressed!="All") next
      if(sens %in% "sens_analysis" & depressed!="All") next
      if(sens %in% "sub_analysis" & depressed %in% c("All","Depressed")) next
      
      if(k==1){
        pgorpp <- "pg"
        ims <- pg_ims
        confs <- pg_confs
        data <- pg
        oversampling <- pg_oversampling_main
        dep <- pg_subanalysis_depressed
        sensitivity <- pg_sensitivity
        ssri <- pg_ssri
      } else if(k==2){
        pgorpp <- "pp"
        ims <- pp_ims
        confs <- pp_confs
        data <- pp
        oversampling <- pp_oversampling_main
        dep <- pp_subanalysis_depressed
        sensitivity <- pp_sensitivity
        ssri <- pp_ssri
      }
      
      labelStratificationByDepression <- "None"
      # stratify data after depression if desired
      if(depressed=="Depressed"){
        data <- data[get(dep)==1]
        labelStratificationByDepression <- sprintf("%s==1",dep)
      } else if(depressed=="Not-depressed"){
        data <- data[get(dep)==0]
        labelStratificationByDepression <- sprintf("%s==0",dep)
      }
      
      # exclude according to sensitivity if desired
      labelStratificationBySensitivity <- "None"
      if(sens=="sens_analysis"){
        data <- data[get(sensitivity)!=1]
        labelStratificationBySensitivity <- sprintf("%s!=1",sensitivity)
      }
      
      # include oversampling for main analysis
      if(sens %in% c("main_analysis","sens_analysis") & depressed=="All"){
        confs <- c(confs, oversampling)
      }
      
      fileConn<-file(file.path(org::PROJ$SHARED_TODAY,
                               sens,
                               sprintf("details_%s_%s.txt",pgorpp,depressed)))
      writeLines(c(
        sprintf("\n\n**NUMBER OF PEOPLE**\n%s",nrow(data)),
        sprintf("\n\n**DATA SELECTION (1)**\n%s",labelStratificationByDepression),
        sprintf("\n\n**DATA SELECTION (2)**\n%s",labelStratificationBySensitivity),
        sprintf("\n\n**CONFOUNDERS**\n%s",paste0(confs,collapse="\n")),
        sprintf("\n\n**OUTCOMES (IFS)**\n%s",paste0(ims,collapse="\n"))
      ), fileConn)
      close(fileConn)
      
      retval <- vector("list",length(ims))
      for(i in 1:length(ims)){
        print(pg_ims[i])
        formula0 <- sprintf("%s ~ %s",ims[i],paste0(confs,collapse="+"))
        if(k==1){
          formula1 <- sprintf("%s ~ %s +SIN_366_preg + COS_366_preg",ims[i],paste0(confs,collapse="+"))
        } else {
          formula1 <- sprintf("%s ~ %s +SIN_366_pp + COS_366_pp",ims[i],paste0(confs,collapse="+"))
        }
        fit0 <- lm(as.formula(formula0),data=data)
        fit1 <- lm(as.formula(formula1),data=data)
        temp <- as.data.frame(coef(summary(fit1)))
        names(temp) <- c("beta","se","t","p")
        temp$var <- row.names(temp)
        temp$pseasonality <- anova(fit1,fit0)$`Pr(>F)`[2]
        
        b1 <- temp$beta[nrow(temp)-1] # sin
        b2 <- temp$beta[nrow(temp)] # cos
        
        temp$amplitude <- sqrt(b1^2 + b2^2)
        p <- atan(b1/b2)*366/2/pi
        if(p>0){
          peak <- p
          trough <- p+366/2
        } else {
          peak <- p+366/2
          trough <- p+366
        }
        if(b1<0){
          g <- peak
          peak <- trough
          trough <- g
        }
        temp$peak <- peak
        temp$trough <- trough
        temp$im <- ims[i]
        
        temp$depressed <- depressed
        temp$meanValueLog2 <- mean(data[[ims[i]]],na.rm=T)
        
        retval[[i]] <- temp
      }
      if(k==1){
        pg_retval[[depressed]] <- retval
      } else {
        pp_retval[[depressed]] <- retval
      }
      
      index <- length(sampleSize)+1
      sampleSize[[index]] <- data.frame(pgorpp,depressed,n=nrow(data))
    }
    
    sampleSize <- rbindlist(sampleSize)
    setorder(sampleSize,pgorpp,depressed)
    
    openxlsx::write.xlsx(sampleSize,file=file.path(org::PROJ$SHARED_TODAY,sens,"samplesize.xlsx"))
    
    for(depressed in c("All","Depressed","Not-depressed")){
      pg_retval[[depressed]] <- rbindlist(pg_retval[[depressed]])
      pp_retval[[depressed]] <- rbindlist(pp_retval[[depressed]])
    }
    pg_retval <- rbindlist(pg_retval)
    pp_retval <- rbindlist(pp_retval)
    
    pg_retval[!im %in% c(pg_outcome_zscore,pp_outcome_zscore),trough_to_peak_change:=100*(2^(2*amplitude)-1)]
    pg_retval[im %in% c(pg_outcome_zscore,pp_outcome_zscore),trough_to_peak_change:=2*amplitude]
    pg_retval[,trough_to_peak_change:=RAWmisc::Format(trough_to_peak_change,1)]
    pg_retval[!im %in% c(pg_outcome_zscore,pp_outcome_zscore),trough_to_peak_change:=sprintf("%s%%",trough_to_peak_change)]
    pg_retval[,peak:=RAWmisc::Format(peak,digits=0)]
    pg_retval[,trough:=RAWmisc::Format(trough,digits=0)]
    
    pp_retval[!im %in% c(pg_outcome_zscore,pp_outcome_zscore),trough_to_peak_change:=100*(2^(2*amplitude)-1)]
    pp_retval[im %in% c(pg_outcome_zscore,pp_outcome_zscore),trough_to_peak_change:=2*amplitude]
    pp_retval[,trough_to_peak_change:=RAWmisc::Format(trough_to_peak_change,1)]
    pp_retval[!im %in% c(pg_outcome_zscore,pp_outcome_zscore),trough_to_peak_change:=sprintf("%s%%",trough_to_peak_change)]
    pp_retval[,peak:=RAWmisc::Format(peak,digits=0)]
    pp_retval[,trough:=RAWmisc::Format(trough,digits=0)]
    
    for(k in 1:2){
      if(k==1){
        x <- pg_retval[var=="(Intercept)"]
      } else {
        x <- pp_retval[var=="(Intercept)"]
      }
      
      tab <- x[,c("im","trough_to_peak_change","peak","trough","depressed","meanValueLog2","pseasonality"),with=F]
      tab[,pbonf:=pseasonality*.N/length(unique(tab$depressed))]
      tab[pbonf>1,pbonf:=1]
      tab[,pbonf:=RAWmisc::Format(pbonf,digits=3)]
      tab[,pseasonality:=RAWmisc::Format(pseasonality,digits=3)]
      
      tab[,meanValueLog2:=RAWmisc::Format(meanValueLog2,digits=1)]
      
      tab[pbonf<"0.050",peak:=sprintf("* %s",peak)]
      tab[pbonf<"0.050",trough:=sprintf("* %s",trough)]
      tab[pbonf<"0.050",trough_to_peak_change:=sprintf("* %s",trough_to_peak_change)]
      
      tab <- dcast.data.table(tab,im~depressed,value.var = c("peak","trough","meanValueLog2","pseasonality","pbonf","trough_to_peak_change"))
      openxlsx::write.xlsx(tab,file=file.path(org::PROJ$SHARED_TODAY,sens,sprintf("table%s.xlsx",k+1)))
    }
    
    toPlot <- vector("list",2)
    for(k in 1:2){
      if(k==1){
        x <- pg_retval[var %in% c("im_sample_year_preg","SIN_366_preg","COS_366_preg")]
        RAWmisc::RecodeDT(x,c(
          "im_sample_year_preg"="year_trend",
          "SIN_366_preg"="sin366",
          "COS_366_preg"="cos366"
        ),
        "var")
        
        details <- pg_retval[var=="(Intercept)"]
      } else {
        x <- pp_retval[var %in% c("im_sample_year_pp","SIN_366_pp","COS_366_pp")]
        RAWmisc::RecodeDT(x,c(
          "im_sample_year_pp"="year_trend",
          "SIN_366_pp"="sin366",
          "COS_366_pp"="cos366"
        ),
        "var")
        
        details <- pp_retval[var=="(Intercept)"]
      }
      
      x[,var:=factor(var,levels=c("sin366","cos366","year_trend"))]
      x[,ci_95:=sprintf("%s, %s",
                        RAWmisc::Format(beta-1.96*se, digits=3),
                        RAWmisc::Format(beta+1.96*se, digits=3)
      )]
      x <- x[,c(
        "depressed",
        "im",
        "var",
        "beta",
        "se",
        "ci_95",
        "p",
        "trough_to_peak_change",
        "peak",
        "trough",
        "pseasonality"
      )]
      x[var!="sin366",pseasonality:=NA]
      x[var=="sin366",pbonf:=stats::p.adjust(pseasonality,method="bonf"),by=depressed]
      toPlot[[k]] <- copy(x)
      x[,beta:=RAWmisc::Format(beta,digits=3)]
      x[,se:=RAWmisc::Format(se,digits=3)]
      x[,p:=RAWmisc::Format(p,digits=3)]
      x[,pseasonality:=RAWmisc::Format(pseasonality,digits=3)]
      x[,pbonf:=RAWmisc::Format(pbonf,digits=3)]
      
      x[pseasonality=="  NA",pseasonality:=""]
      x[pbonf=="  NA",pbonf:=""]
      
      x[pbonf=="",trough_to_peak_change:=""]
      x[pbonf=="",peak:=""]
      x[pbonf=="",trough:=""]
      
      setorder(x,depressed,im,var)
      
      openxlsx::write.xlsx(x,file=file.path(org::PROJ$SHARED_TODAY,sens,sprintf("supp_table%s.xlsx",k)))
    }
    
    if(sens=="sens_analysis") next
    
    toPlot <- rbindlist(toPlot)
    toPlot[,sens:=sens]
    toPlotTotal[[sens]] <- toPlot
  }
  toPlotTotal <- rbindlist(toPlotTotal)
  toPlot <- toPlotTotal
  xtabs(~toPlot$depressed+toPlot$sens)
  # PLOTTING
  toPlot <- toPlot[var %in% c("sin366","cos366"),c("depressed","im","var","beta","pbonf"),with=F]
  toPlot[,pbonf:=mean(pbonf,na.rm=T),by=.(depressed,im)]
    
  stack <- dcast.data.table(toPlot,depressed+im+pbonf~var, value.var="beta")
  stack[,cos366:=as.numeric(cos366)]
  stack[,sin366:=as.numeric(sin366)]
    
  data <- vector("list",length=nrow(stack))
  for(i in 1:length(data)){
    data[[i]] <- data.table(Depressed=stack$depressed[i],IF=stack$im[i],day=1:366,pbonf=stack$pbonf[i])
    data[[i]][,y:=stack$cos366[i]*cos(2*pi*day/366) + stack$sin366[i]*sin(2*pi*day/366)]
  }
  
  masterData <- rbindlist(data)
  data <- rbindlist(data)[pbonf<0.05]
  
  data[,date:=as.Date("2016-12-31")+day]
  data[,labels:=""]
  data[IF %in% c(
    "im_log2_101_IL_8_pg",
    "im_log2_136_MCP_4_pp",
    "im_log2_172_SIRT2_pg",
    "im_log2_118_AXIN1_pg",
    "im_log2_192_STAMPB_pg",
    "im_log2_183_MCP_2_pg"),labels:=IF]
  data[,labels:=stringr::str_replace(labels,"im_log2_[0-9][0-9][0-9]_","")]
  unique(data$labels)
  RAWmisc::RecodeDT(data,switch=c(
    "IL_8_pg"="IL-8 (pregnancy)",
    "AXIN1_pg"="AXIN1 (pregnancy)",
    "MCP_4_pp"="MCP-4 (postpartum)",
    "SIRT2_pg"="SIRT2 (pregnancy)",
    "STAMPB_pg"="STAM-BP (pregnancy)"
  ), var="labels")
  unique(data$labels)
  l <- unique(data$labels)
  l_pp <- l[stringr::str_detect(l,"\\(postpartum\\)$")]
  l_pg <- l[stringr::str_detect(l,"\\(pregnancy\\)$")]
  data[,labels:=factor(labels,levels = c("zscorePG",l_pg,l_pp,""))]
  
  saveA4 <- function(q,filename,landscape=T){
    ggsave(filename,plot=q,width=297,height=210, units="mm")
  }
  
  plotData <- data[!IF %in% c("zscorePG","zscorePP") & Depressed!="Depressed"]
  plotData[Depressed=="Not-depressed",Depressed:="No depressive symptoms"]
  q <- ggplot(plotData, aes(x=date,y=y,group=IF))
  q <- q + geom_line()
  #q <- q + expand_limits(x=as.Date("2016-08-01"))
  #q <- q + scale_colour_manual("",values=c("#e41a1c", "#377eb8", "#4daf4a", "#ff7f00", "#f781bf"))
  #q <- q + scale_colour_brewer("",palette="Set2")
  q <- q + scale_x_date("Day/month", labels = scales::date_format("%d/%m"),
                        breaks=as.Date(c("2017-01-01",
                                         "2017-03-01",
                                         "2017-05-01",
                                         "2017-07-01",
                                         "2017-09-01",
                                         "2017-11-01",
                                         "2018-01-01")),
                        minor_breaks=as.Date(c("2017-02-01",
                                               "2017-04-01",
                                               "2017-06-01",
                                               "2017-08-01",
                                               "2017-10-01",
                                               "2017-12-01",
                                               "2018-01-01")))
  q <- q + scale_y_continuous("Change in NPX [log2(concentration)]")
  q <- q + theme_gray(base_size=16)
  q <- q + facet_wrap(~Depressed,ncol=1)
  RAWmisc::saveA4(q,filename=file.path(org::PROJ$SHARED_TODAY,"main_graphs","figure_sig.png"))
  
  
    
  data <- masterData
  data[y>1]
  
  data[,date:=as.Date("2016-12-31")+day]
  
  data[,labels:="Not significant PG"]
  data[stringr::str_detect(IF,"_pp$"),labels:="Not significant PP"]
  data[pbonf<0.05 & stringr::str_detect(IF,"_pg$"),labels:="Significant PG"]
  data[pbonf<0.05 & stringr::str_detect(IF,"_pp$"),labels:="Significant PP"]
  
  plotData <- data[!IF %in% c("zscorePG","zscorePP") & Depressed!="Depressed"]
  plotData[Depressed=="Not-depressed",Depressed:="No depressive symptoms"]
  
  q <- ggplot(as.data.frame(plotData[!IF %in% c("zscorePG","zscorePP")]), aes(x=date,y=y,group=IF,colour=labels))
  q <- q + geom_line(data=plotData[labels %in% c("Not significant PG","Not significant PP") & !IF %in% c("zscorePG","zscorePP")],mapping=aes(colour=labels),lwd=0.25,alpha=0.5)
  q <- q + geom_line(data=plotData[!labels %in% c("Not significant PG","Not significant PP") & !IF %in% c("zscorePG","zscorePP")],mapping=aes(colour=labels))
  #q <- q + expand_limits(x=as.Date("2016-08-01"))
  q <- q + scale_colour_manual("",values=c("black", "#377eb8", "#4daf4a", "#ff7f00", "#f781bf"))
  #q <- q + scale_colour_brewer("",palette="Set2")
  q <- q + scale_x_date("Day/month", labels = scales::date_format("%d/%m"),
                        breaks=as.Date(c("2017-01-01",
                                         "2017-03-01",
                                         "2017-05-01",
                                         "2017-07-01",
                                         "2017-09-01",
                                         "2017-11-01",
                                         "2018-01-01")),
                        minor_breaks=as.Date(c("2017-02-01",
                                               "2017-04-01",
                                               "2017-06-01",
                                               "2017-08-01",
                                               "2017-10-01",
                                               "2017-12-01",
                                               "2018-01-01")))
  q <- q + scale_y_continuous("Change in NPX [log2(concentration)]")
  q <- q + theme_gray(base_size=20)
  q <- q + facet_wrap(~Depressed,ncol=1)
  RAWmisc::saveA4(q,filename=file.path(org::PROJ$SHARED_TODAY,"main_graphs","figure_all.png"))
  
  
  for(im in unique(data$IF)) for(dep in c("All","Not-depressed")){
    fitted <- data[Depressed==dep & IF==im]
    if(fitted$pbonf[1]>0.05) next
    
    if(fitted$labels[1]=="Significant PG"){
      pd <- pg[,c(im,"im_sample_day_preg",pg_subanalysis_depressed),with=F]
      setnames(pd,c("NPX","day","depressed"))
    } else if(fitted$labels[1]=="Significant PP"){
      pd <- pp[,c(im,"im_sample_day_pp",pp_subanalysis_depressed),with=F]
      setnames(pd,c("NPX","day","depressed"))
    }
    
    if(dep=="Depressed"){
      pd <- pd[depressed==1]
    } else if(dep=="Not-depressed"){
      pd <- pd[depressed==0]
    }
    
    zeropoint <- c()
    for(i in 0:8){
      zeropoint <- c(zeropoint,mean(pd[day %in% (i*40+1):(i*40+40)]$NPX,na.rm=T))
    }
    zeropoint <- mean(zeropoint,na.rm=T)
    zeropoint
    mean(pd$NPX,na.rm=T)
    
    q <- ggplot()
    q <- q + geom_point(data=pd,mapping=aes(x=day,y=NPX))
    q <- q + stat_smooth(data=pd,mapping=aes(x=day,y=NPX),se=F,col="blue",lwd=1.5)
    q <- q + geom_line(data=fitted,mapping=aes(x=day,y=y+zeropoint),col="red",lwd=1.5)
    q <- q + scale_x_continuous("Day of year")
    #q <- q + labs(title=sprintf("%s - %s",dep,im))
    q <- q + theme_gray(base_size=20)
    #q <- q + labs(caption="Red line = Cosine/Sine fit from regression models. Blue line = LOESS fit. ")
    RAWmisc::saveA4(q,filename=file.path(org::PROJ$SHARED_TODAY,
                                         ifelse(dep=="All","main_analysis","sub_analysis"),
                                         "scatter_plots",
                                         sprintf("%s_%s.png",dep,im)))
  }
  
  # 2019 hanna requests graphs for her thesis
  
  # VEGF-A, OPG, CSF-1, CDCP1, and TNFRSF9),
  # the three markers with the largest expected relative 
  # difference between trough and peak (SIRT2, AXIN1, and STAM-BP – all pregnancy),
  # as well as the postpartum marker (MCP-4)
  unique(data$IF)
  stringr::str_subset(unique(data$IF),"VEGF")
  stringr::str_subset(unique(data$IF),"OPG")
  stringr::str_subset(unique(data$IF),"CSF")
  stringr::str_subset(unique(data$IF),"CDCP")
  stringr::str_subset(unique(data$IF),"TNFRS")
  stringr::str_subset(unique(data$IF),"SIRT")
  stringr::str_subset(unique(data$IF),"AXIN")
  stringr::str_subset(unique(data$IF),"STAM")
  stringr::str_subset(unique(data$IF),"MCP")
  
  pd <- data[IF %in% c(
    "im_log2_102_VEGF_A_pg",
    "im_log2_110_OPG_pg",
    "im_log2_196_CSF_1_pg",
    "im_log2_107_CDCP1_pg",
    "im_log2_187_TNFRSF9_pg",
    "im_log2_172_SIRT2_pg",
    "im_log2_118_AXIN1_pg",
    "im_log2_192_STAMPB_pg",
    "im_log2_136_MCP_4_pp"
  ) & 
    Depressed=="All"] 
  
  pd[IF=="im_log2_102_VEGF_A_pg",labels:="VEGF-A (pg)"]
  pd[IF=="im_log2_110_OPG_pg",labels:="OPG (pg)"]
  pd[IF=="im_log2_196_CSF_1_pg",labels:="CSF-1 (pg)"]
  pd[IF=="im_log2_107_CDCP1_pg",labels:="CDCP1 (pg)"]
  pd[IF=="im_log2_187_TNFRSF9_pg",labels:="TNFRSF9 (pg)"]
  pd[IF=="im_log2_172_SIRT2_pg",labels:="SIRT2 (pg)"]
  pd[IF=="im_log2_118_AXIN1_pg",labels:="AXIN1 (pg)"]
  pd[IF=="im_log2_192_STAMPB_pg",labels:="STAM-BP (pg)"]
  pd[IF=="im_log2_136_MCP_4_pp",labels:="MCP-4 (pp)"]
  pd[,str_pbonf:=formatC(pbonf,digits=3,format="f")]
  pd[str_pbonf=="0.000",str_pbonf:="<0.001"]
  pd[,labels:=glue::glue("{labels}; p={str_pbonf}",labels=labels,str_pbonf=str_pbonf)]
  
  q <- ggplot(pd, aes(x=date,y=y,group=labels,colour=labels))
  q <- q + geom_line(lwd=2,colour="black")
  q <- q + geom_line(lwd=1.5)
  #q <- q + expand_limits(x=as.Date("2016-08-01"))
  q <- q + scale_colour_brewer("",palette="Set1")
  #q <- q + scale_colour_brewer("",palette="Set2")
  q <- q + scale_x_date("Day/month", labels = scales::date_format("%d/%m"),
                        breaks=as.Date(c("2017-01-01",
                                         "2017-03-01",
                                         "2017-05-01",
                                         "2017-07-01",
                                         "2017-09-01",
                                         "2017-11-01",
                                         "2018-01-01")),
                        minor_breaks=as.Date(c("2017-02-01",
                                               "2017-04-01",
                                               "2017-06-01",
                                               "2017-08-01",
                                               "2017-10-01",
                                               "2017-12-01",
                                               "2018-01-01")))
  q <- q + scale_y_continuous("Change in NPX [log2(concentration)]")
  q <- q + theme_gray(base_size=16)
  RAWmisc::saveA4(q,filename=file.path(org::PROJ$SHARED_TODAY,"thesis_graphs","figure.png"))
  
  
}