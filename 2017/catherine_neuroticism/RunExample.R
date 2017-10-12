RAWmisc::AllowFileManipulationFromInitialiseProject()
RAWmisc::InitialiseProject(
  HOME = "/git/code_minor/2017/catherine_neuroticism/",
  RAW = "/analyses/data_raw/code_minor/2017/catherine_neuroticism/",
  CLEAN = "/analyses/data_clean/code_minor/2017/catherine_neuroticism",
  BAKED = "/analyses/results_baked/code_minor/2017/catherine_neuroticism/",
  FINAL = "/analyses/results_final/code_minor/2017/catherine_neuroticism/",
  SHARED = "/dropbox/results_shared/code_minor/2017/catherine_neuroticism/")

suppressWarnings(suppressMessages(library(data.table)))
suppressWarnings(suppressMessages(library(ggplot2)))
suppressWarnings(suppressMessages(library(rms)))

d <- data.table(haven::read_spss(file.path(RAWmisc::PROJ$RAW,"SSP_MFR_cases_1969.sav")))


library(rms)
library(ggplot2)

nrow(d)

# outcomes
xtabs(~d$Gestational_diabetes_LGA_CA)
xtabs(~d$Gestational_HT_PE_CA)
xtabs(~d$Prolonged_pregnancy_GRVBS_CA)
xtabs(~d$PROM_CA)

# exposures
summary(d$Neuroticism)
p50 <- BASELINE <- 292
p25 <- 261
p75 <- 324
POINTS <- seq(200,500,50)

#### TOTAL ANXIETY
plotData <- vector("list",4)
for(i in 1:4){
  if(i==1){
    outcome <- "Gestational diabetes or LGA"
    d[,outcome:=Gestational_diabetes_LGA_CA]
  } else if(i==2){
    outcome <- "Gestational HT or PE"
    d[,outcome:=Gestational_HT_PE_CA]
  } else if(i==3){
    outcome <- "Prolonged pregnancy (överburenhet)"
    d[,outcome:=Prolonged_pregnancy_GRVBS_CA]
  } else if(i==4){
    outcome <- "Premature rupture of membranes (PROM)"
    d[,outcome:=PROM_CA]
  } 
  ddist <- datadist(d)
  ddist$limits$Neuroticism[2] <- BASELINE ##### SETTING REFERENCE VALUE FOR CHOLESTEROL
  options(datadist='ddist')
  
  ##### RUNNING THE MODEL
  fit <- lrm(outcome ~ rcs(Neuroticism,4),
             x=TRUE, y=TRUE, data=d)
  p <- Predict(fit, Neuroticism=sort(c(BASELINE,POINTS,p25,p75)), ref.zero=T)
  plotData[[i]] <- data.frame(p)
  
  plotData[[i]]$outcome <- outcome
  plotData[[i]]$labelloc <- 0
  plotData[[i]][seq(1,10,2),]$labelloc <- plotData[[i]][seq(1,10,2),]$upper+0.5
  plotData[[i]][seq(2,10,2),]$labelloc <- plotData[[i]][seq(2,10,2),]$lower-0.5
  
  print(outcome)
  print(fit)
}

plotData <- rbindlist(plotData)
#plotData$Neuroticism[plotData$Neuroticism!=BASELINE] <- plotData$Neuroticism[plotData$Neuroticism!=BASELINE] - 0.25

labels <- c("1/32","1/16","1/8","1/4","1/2","1","2","4","8","16","32","64","128","256","512")
breaks <- log(c(1/32,1/16,1/8,1/4,1/2,1,2,4,8,16,32,64,128,256,512))

q <- ggplot(data=plotData, mapping=aes(x=Neuroticism, y=yhat,ymin=lower,ymax=upper))   # or plot()
q <- q + geom_hline(yintercept=0, colour="red")
q <- q + geom_vline(xintercept=p50, colour="red")
q <- q + geom_vline(xintercept=p25, colour="red",lty=2)
q <- q + geom_vline(xintercept=p75, colour="red",lty=2)
q <- q + geom_errorbar(lwd=1,width=0)
q <- q + geom_point()
q <- q + geom_line(lwd=1)
q <- q + geom_label(aes(y=labelloc,label=RAWmisc::Format(exp(yhat),1)))
q <- q + scale_x_continuous("Neuroticism",breaks=seq(0,1000,50))
q <- q + scale_y_continuous("Odds ratio",breaks=breaks,labels=labels)
q <- q + coord_cartesian(ylim = c(log(1/8),log(256)))
q <- q + facet_wrap(~outcome)
q <- q + theme_gray(16)
RAWmisc::saveA4(q, file.path(RAWmisc::PROJ$SHARED_TODAY,"neuroticism.png"))


