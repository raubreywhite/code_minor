RAWmisc::InitialiseProject(
  PROJHOME = "/git/code_minor/cathrine_ppd_aa_splines/",
  PROJRAW = "/analyses/data_raw/cathrine_ppd_aa_splines/",
  PROJCLEAN = "/analyses/data_clean/cathrine_ppd_aa_splines",
  PROJBAKED = "/analyses/results_baked/cathrine_ppd_aa_splines/",
  PROJFINAL = "/analyses/results_final/cathrine_ppd_aa_splines/",
  PROJSHARED = "/dropbox/results_shared/cathrine_ppd_aa_splines/")

suppressWarnings(suppressMessages(library(data.table)))
suppressWarnings(suppressMessages(library(ggplot2)))
suppressWarnings(suppressMessages(library(rms)))

dir.create(file.path(RPROJ$PROJSHARED,lubridate::today()))

d <- data.table(haven::read_spss(file.path(RPROJ$PROJRAW,"pek2_sample_170402.sav")))


library(rms)
library(ggplot2)

nrow(d)

# outcomes
d$ppv6_EPDS_D_9R
d$ppm6_EPDS_D_9R

# exposures
sum(!is.na(d[v32_EPDS_D_9R==0]$ASQSF_total_anxiety))
sum(!is.na(d[v32_EPDS_D_9R==0 & filter_ASQSTAI_160715==0]$CA_Neuroticism))
sum(!is.na(d[v32_EPDS_D_9R==0 & filter_ASQSTAI_160715==1]$v32_STAIT_CA_R))

d_aa <- d[v32_EPDS_D_9R==0,c("ppv6_EPDS_D_9R","ppm6_EPDS_D_9R","ASQSF_total_anxiety"),with=F]
d_n  <- d[v32_EPDS_D_9R==0 & filter_ASQSTAI_160715==0,c("ppv6_EPDS_D_9R","ppm6_EPDS_D_9R","CA_Neuroticism"),with=F]
d_ta <- d[v32_EPDS_D_9R==0 & filter_ASQSTAI_160715==1,c("ppv6_EPDS_D_9R","ppm6_EPDS_D_9R","v32_STAIT_CA_R"),with=F]


# TESTING TO MAKE SURE EVERYTHING WORKS WITH STANDARD R
fit <- glm(ppm6_EPDS_D_9R ~ ASQSF_total_anxiety, data=d_aa, family=binomial)
summary(fit)
exp(coef(fit)[[2]]*15) ## this is 2.17, close enough to what we find below

d_aa$exposure <- NA
d_aa$exposure[d_aa$ASQSF_total_anxiety<=35] <- 0
d_aa$exposure[d_aa$ASQSF_total_anxiety>35] <- 1
fit <- glm(ppm6_EPDS_D_9R ~ exposure, data=d_aa, family=binomial)
summary(fit)

fit <- glm(ppm6_EPDS_D_9R ~ ASQSF_total_anxiety, data=d_aa, family=binomial)
summary(fit)
exp(coef(fit)[[2]]*15) ## this is 2.17, close enough to what we find below


#### TOTAL ANXIETY
summary(d_aa$ASQSF_total_anxiety)
summary(d_n$CA_Neuroticism)
summary(d_ta$v32_STAIT_CA_R)

CUTOFF <- 35
BASELINE <- 13
ddist <- datadist(d_aa)
ddist$limits$ASQSF_total_anxiety[2] <- BASELINE ##### SETTING REFERENCE VALUE FOR CHOLESTEROL
options(datadist='ddist')

##### RUNNING THE MODEL
fit <- lrm(ppv6_EPDS_D_9R ~ rcs(ASQSF_total_anxiety,4),
           x=TRUE, y=TRUE, data=d_aa)
p <- Predict(fit, ASQSF_total_anxiety=c(BASELINE,seq(15,75,5)), ref.zero=T)
plotDataWEEK6 <- data.frame(p)
plotDataWEEK6$outcome <- "EPDS screening (6 weeks)"
plotDataWEEK6$ASQSF_total_anxiety[plotDataWEEK6$ASQSF_total_anxiety!=BASELINE] <- plotDataWEEK6$ASQSF_total_anxiety[plotDataWEEK6$ASQSF_total_anxiety!=BASELINE] - 0.25

fit <- lrm(ppm6_EPDS_D_9R ~ rcs(ASQSF_total_anxiety,4),
           x=TRUE, y=TRUE, data=d_aa)
p <- Predict(fit, ASQSF_total_anxiety=c(BASELINE,seq(15,75,5)), ref.zero=T)
plotDataMONTH6 <- data.frame(p)
plotDataMONTH6$outcome <- "EPDS screening (6 months)"
plotDataMONTH6$ASQSF_total_anxiety[plotDataMONTH6$ASQSF_total_anxiety!=BASELINE] <- plotDataMONTH6$ASQSF_total_anxiety[plotDataMONTH6$ASQSF_total_anxiety!=BASELINE] + 0.25

plotData <- rbind(plotDataWEEK6, plotDataMONTH6)

labels <- c("1/32","1/16","1/8","1/4","1/2","1","2","4","8","16","32","64","128","256","512")
breaks <- log(c(1/32,1/16,1/8,1/4,1/2,1,2,4,8,16,32,64,128,256,512))

q <- ggplot(data=plotData, mapping=aes(x=ASQSF_total_anxiety, y=yhat,ymin=lower,ymax=upper,colour=outcome))   # or plot()
q <- q + geom_hline(yintercept=0, colour="black")
q <- q + geom_rect(xmin=-Inf,xmax=CUTOFF,ymin=-Inf,ymax=Inf,fill="black", colour=NA,alpha=0.01)
q <- q + geom_vline(xintercept=median(d_aa$ASQSF_total_anxiety), colour="red")
q <- q + geom_errorbar(lwd=2,width=0)
q <- q + geom_point(size=8)
q <- q + geom_line(lwd=2)
q <- q + scale_x_continuous("Attachment anxiety",breaks=seq(0,100,10))
q <- q + scale_y_continuous("Odds ratio",breaks=breaks,labels=labels)
q <- q + coord_cartesian(ylim = c(log(1/4),log(512)))
q <- q + scale_color_brewer(palette="Set1")
q <- q + RAWmisc::theme_SMAO()
RAWmisc::SMAOpng(file.path(RPROJ$PROJSHARED,lubridate::today(),"ASQSF_total_anxiety.png"))
print(q)
dev.off()

#### NEUROTICISM

d_n$exposure <- NA
d_n$exposure[d_n$CA_Neuroticism<=275] <- 0
d_n$exposure[d_n$CA_Neuroticism>275] <- 1
fit <- glm(ppm6_EPDS_D_9R ~ exposure, data=d_n, family=binomial)
summary(fit)
fit <- glm(ppm6_EPDS_D_9R ~ CA_Neuroticism, data=d_n, family=binomial)
summary(fit)



summary(d_aa$ASQSF_total_anxiety)
summary(d_n$CA_Neuroticism)
summary(d_ta$v32_STAIT_CA_R)
ddist <- datadist(d_n)
CUTOFF <- 275
BASELINE <- 193
ddist$limits$CA_Neuroticism[2] <- BASELINE ##### SETTING REFERENCE VALUE FOR CHOLESTEROL
options(datadist='ddist')

##### RUNNING THE MODEL
fit <- lrm(ppv6_EPDS_D_9R ~ rcs(CA_Neuroticism,4),
           x=TRUE, y=TRUE, data=d_n)
p <- Predict(fit, CA_Neuroticism=c(BASELINE,seq(200,425,25)), ref.zero=T)
plotDataWEEK6 <- data.frame(p)
plotDataWEEK6$outcome <- "EPDS screening (6 weeks)"
plotDataWEEK6$CA_Neuroticism[plotDataWEEK6$CA_Neuroticism!=BASELINE] <- plotDataWEEK6$CA_Neuroticism[plotDataWEEK6$CA_Neuroticism!=BASELINE] - 1

fit <- lrm(ppm6_EPDS_D_9R ~ rcs(CA_Neuroticism,4),
           x=TRUE, y=TRUE, data=d_n)
p <- Predict(fit, CA_Neuroticism=c(BASELINE,seq(200,425,25)), ref.zero=T)
plotDataMONTH6 <- data.frame(p)
plotDataMONTH6$outcome <- "EPDS screening (6 months)"
plotDataMONTH6$CA_Neuroticism[plotDataMONTH6$CA_Neuroticism!=BASELINE] <- plotDataMONTH6$CA_Neuroticism[plotDataMONTH6$CA_Neuroticism!=BASELINE] + 1

plotData <- rbind(plotDataWEEK6, plotDataMONTH6)

q <- ggplot(data=plotData, mapping=aes(x=CA_Neuroticism, y=yhat,ymin=lower,ymax=upper,colour=outcome))   # or plot()
q <- q + geom_hline(yintercept=0, colour="black")
q <- q + geom_rect(xmin=-Inf,xmax=CUTOFF,ymin=-Inf,ymax=Inf,fill="black", colour=NA,alpha=0.01)
q <- q + geom_vline(xintercept=median(d_n$CA_Neuroticism), colour="red")
q <- q + geom_errorbar(lwd=2,width=0)
q <- q + geom_point(size=8)
q <- q + geom_line(lwd=2)
q <- q + scale_x_continuous("Neuroticism")
q <- q + scale_y_continuous("Odds ratio",breaks=breaks,labels=labels)
q <- q + coord_cartesian(ylim = c(log(1/4),log(512)))
q <- q + scale_color_brewer(palette="Set1")
q <- q + RAWmisc::theme_SMAO()
RAWmisc::SMAOpng(file.path(RPROJ$PROJSHARED,lubridate::today(),"CA_Neuroticism.png"))
print(q)
dev.off()



#### TRAIT ANXIETY
summary(d_aa$ASQSF_total_anxiety)
summary(d_n$CA_Neuroticism)
summary(d_ta$v32_STAIT_CA_R)
ddist <- datadist(d_ta)
CUTOFF <- 35
BASELINE <- 19
ddist$limits$v32_STAIT_CA_R[2] <- BASELINE ##### SETTING REFERENCE VALUE FOR CHOLESTEROL
options(datadist='ddist')

##### RUNNING THE MODEL
fit <- lrm(ppv6_EPDS_D_9R ~ rcs(v32_STAIT_CA_R,4),
           x=TRUE, y=TRUE, data=d_ta)
p <- Predict(fit, v32_STAIT_CA_R=c(BASELINE,seq(20,65,5)), ref.zero=T)
plotDataWEEK6 <- data.frame(p)
plotDataWEEK6$outcome <- "EPDS screening (6 weeks)"
plotDataWEEK6$v32_STAIT_CA_R[plotDataWEEK6$v32_STAIT_CA_R!=BASELINE] <- plotDataWEEK6$v32_STAIT_CA_R[plotDataWEEK6$v32_STAIT_CA_R!=BASELINE] - 0.25

fit <- lrm(ppm6_EPDS_D_9R ~ rcs(v32_STAIT_CA_R,4),
           x=TRUE, y=TRUE, data=d_ta)
p <- Predict(fit, v32_STAIT_CA_R=c(BASELINE,seq(20,65,5)), ref.zero=T)
plotDataMONTH6 <- data.frame(p)
plotDataMONTH6$outcome <- "EPDS screening (6 months)"
plotDataMONTH6$v32_STAIT_CA_R[plotDataMONTH6$v32_STAIT_CA_R!=BASELINE] <- plotDataMONTH6$v32_STAIT_CA_R[plotDataMONTH6$v32_STAIT_CA_R!=BASELINE] + 0.25

plotData <- rbind(plotDataWEEK6, plotDataMONTH6)

q <- ggplot(data=plotData, mapping=aes(x=v32_STAIT_CA_R, y=yhat,ymin=lower,ymax=upper,colour=outcome))   # or plot()
q <- q + geom_hline(yintercept=0, colour="black")
q <- q + geom_rect(xmin=-Inf,xmax=CUTOFF,ymin=-Inf,ymax=Inf,fill="black", colour=NA,alpha=0.01)
q <- q + geom_vline(xintercept=median(d_ta$v32_STAIT_CA_R), colour="red")
q <- q + geom_errorbar(lwd=2,width=0)
q <- q + geom_point(size=8)
q <- q + geom_line(lwd=2)
q <- q + scale_x_continuous("Trait anxiety")
q <- q + scale_y_continuous("Odds ratio",breaks=breaks,labels=labels)
q <- q + coord_cartesian(ylim = c(log(1/16),log(512)))
q <- q + scale_color_brewer(palette="Set1")
q <- q + RAWmisc::theme_SMAO()
RAWmisc::SMAOpng(file.path(RPROJ$PROJSHARED,lubridate::today(),"v32_STAIT_CA_R.png"))
print(q)
dev.off()




confounder <- runif(10000)
d <- data.table(confounder)
d[,x:=2*confounder+runif(.N)]
d[,y:=x+confounder+rnorm(.N)]
d[,yproxy:=y+rnorm(.N)]
d[,xproxy:=x+rnorm(.N)]

d1 <- d[y>1]
d1[,oversampled:=1]
d1[,N:=.N*5]
d2 <- d[y<1]
d2[,oversampled:=0]
d2[,N:=.N]
dx <- rbind(d1,d1,d1,d1,d1,d2)
dx[,tN:=.N]
dx[,inclProb := 5]
dx[oversampled==0, inclProb:=1]
summary(lm(y~x+confounder,data=d))
summary(lm(y~x+confounder,data=dx))
summary(lm(y~x+confounder,data=dx,weights=1/inclProb))




confounder <- runif(10000)
d <- data.table(confounder)
d[,x:=2*confounder+runif(.N)]
d[,y:=x+confounder+rnorm(.N)]
d[,yproxy:=y+rnorm(.N)]
d[,xproxy:=x+rnorm(.N)]

d1 <- d[y>1]
d1[,oversampled:=1]
d1[,N:=.N*5]
d2 <- d[y<1]
d2[,oversampled:=0]
d2[,N:=.N]
dx <- rbind(d1,d1,d1,d1,d1,d2)
dx[,tN:=.N]
dx[,inclProb := 5]
dx[oversampled==0, inclProb:=1]
dx[,id:=1:.N]
dx[,personIntercept := rnorm(.N)]
dxx <- reshape::untable(df=dx, num=5)
dxx[,ylongitudinal:=y+personIntercept+rnorm(.N)/2]
dxx[,invProb:=1/inclProb]

summary(lm(y~x+confounder,data=d))
summary(lm(y~x+confounder,data=dx))
summary(lm(y~x+confounder,data=dx,weights=1/inclProb))

summary(lm(y~x+confounder,data=dxx,weights=1/inclProb))
summary(lme4::lmer(ylongitudinal~x+confounder+(1|id),data=dxx,weights=1/inclProb))
summary(lme4::lmer(ylongitudinal~x+confounder+(1|id),data=dxx))


des <- survey::svydesign(id=~id,prob=~inclProb,data=dxx)
summary(survey::svyglm(ylongitudinal~x+confounder, design=des, family=gaussian))

des <- survey::svydesign(id=~id,prob=~inclProb,data=dx)
summary(survey::svyglm(y~x+confounder, design=des, family=gaussian))



d1 <- d[x>0.5]
d1[,oversampled:=1]
d1[,N:=.N*5]
d2 <- d[x<0.5]
d2[,oversampled:=0]
d2[,N:=.N]
dx <- rbind(d1,d1,d1,d1,d1,d2)
dx[,tN:=.N]
dx[,inclProb := 5]
dx[oversampled==0, inclProb:=1]
dx[,id:=1:.N]
dx[,personIntercept := rnorm(.N)]
dxx <- reshape::untable(df=dx, num=5)
dxx[,ylongitudinal:=y+personIntercept+rnorm(.N)/2]
dxx[,invProb:=1/inclProb]

summary(lm(y~x+confounder,data=d))
summary(lm(y~x+confounder,data=dx))
summary(lm(y~x+confounder,data=dx,weights=1/inclProb))

summary(lm(y~x+confounder,data=dxx,weights=1/inclProb))
summary(lme4::lmer(ylongitudinal~x+confounder+(1|id),data=dxx,weights=1/inclProb))
summary(lme4::lmer(ylongitudinal~x+confounder+(1|id),data=dxx))


des <- survey::svydesign(id=~id,prob=~inclProb,data=dxx)
summary(survey::svyglm(ylongitudinal~x+confounder, design=des, family=gaussian))

des <- survey::svydesign(id=~id,prob=~inclProb,data=dx)
summary(survey::svyglm(y~x+confounder, design=des, family=gaussian))

