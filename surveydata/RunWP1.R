RAWmisc::InitialiseProject(
  PROJHOME = "/home/rstudio/analyses/code_minor/surveydata/",
  PROJRAW = "/home/rstudio/analyses/data_raw/surveydata/",
  PROJCLEAN = "/home/rstudio/analyses/data_clean/surveydata",
  PROJBAKED = "/home/rstudio/analyses/results_baked/surveydata/",
  PROJFINAL = "/home/rstudio/analyses/results_final/surveydata/",
  PROJSHARED = "/dropbox/results_shared/surveydata/")

suppressWarnings(suppressMessages(library(data.table)))
suppressWarnings(suppressMessages(library(ggplot2)))

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

summary(lm(y~x+confounder, data=dx))
summary(lm(y~x+confounder+oversampled, data=dx))

des <- survey::svydesign(id=~id,data=dx)
summary(survey::svyglm(y~x+confounder, design=des, family=gaussian))

des <- survey::svydesign(id=~id,prob=~inclProb,data=dx)
summary(survey::svyglm(y~x+confounder, design=des, family=gaussian))


des <- survey::svydesign(id=~id,prob=~inclProb,data=dxx)
summary(survey::svyglm(ylongitudinal~x+confounder, design=des, family=gaussian))




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

