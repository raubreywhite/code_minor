RAWmisc::InitialiseProject(
  HOME = "/git/code_minor/hanna_paper_3/",
  RAW = "/analyses/data_raw/hanna_paper_3/",
  CLEAN = "/analyses/data_clean/hanna_paper_3",
  BAKED = "/analyses/results_baked/hanna_paper_3/",
  FINAL = "/analyses/results_final/hanna_paper_3/",
  SHARED = "/dropbox/clients/hanna/paper_3/richard/")

assign("RUN_ALL", TRUE, envir=globalenv())
library(data.table)
library(ggplot2)

if(!exists("d")) d <- haven::read_spss(file.path(RAWmisc::PROJ$RAW,"alla_large_final.sav"))
index <- d$im_participating_preg==1
index[is.na(index)] <- FALSE
d <- d[index,]

######## PREGNANCY

yVar <- "zscorePG"
xVar <- "im_sample_day_preg"

min(d[[xVar]],na.rm=T)
max(d[[xVar]],na.rm=T)

xtabs(~d$im_sample_month_preg)

#### NO JULY
dnojuly <- d[d$im_sample_month_preg!=7,]

png(file.path(RAWmisc::PROJ$SHARED_TODAY,"NOJULY_lomb_scargle_periodogram_pg.png"),width=1000,height=600)
lomb::lsp(as.matrix(na.omit(dnojuly[,c(xVar,yVar)])),type="period",from=30,to=400, ofac=20,alpha=0.05)
dev.off()

# formula with one peak/trough
formula <- as.formula(sprintf("%s~sin(2*pi*%s/366)+cos(2*pi*%s/366)",yVar,xVar,xVar))
print(formula)
fit1 <- lm(formula,data=dnojuly)
summary(fit1)
pd <- data.table(x=dnojuly[[xVar]],y=dnojuly[[yVar]],p=predict(fit1,dnojuly))
q <- ggplot(pd,aes(x=x))
q <- q + geom_point(aes(y=y))
q <- q + geom_line(aes(y=p),col="red")
q <- q + scale_x_continuous("Day of year")
q <- q + scale_y_continuous(yVar)
q <- q + labs(title=sprintf("One harmonic for %s",yVar))
png(file.path(RAWmisc::PROJ$SHARED_TODAY,"NOJULY_1_pg_outcome.png"),width=1000,height=600)
print(q)
dev.off()

q <- ggplot(pd,aes(x=x,y=p-y))
q <- q + geom_point()
q <- q + stat_smooth(se=F)
q <- q + scale_x_continuous("Day of year")
q <- q + scale_y_continuous("Residuals")
q <- q + labs(title=sprintf("One harmonic for %s",yVar))
png(file.path(RAWmisc::PROJ$SHARED_TODAY,"NOJULY_1_pg_residuals.png"),width=1000,height=600)
print(q)
dev.off()

formula <- as.formula(sprintf("%s~sin(2*pi*%s/366)+cos(2*pi*%s/366)+sin(2*pi*%s/183)+cos(2*pi*%s/183)",yVar,xVar,xVar,xVar,xVar))
fit2 <- lm(formula,data=dnojuly)
summary(fit2)
pd <- data.table(x=dnojuly[[xVar]],y=dnojuly[[yVar]],p=predict(fit2,dnojuly))
q <- ggplot(pd,aes(x=x))
q <- q + geom_point(aes(y=y))
q <- q + geom_line(aes(y=p),col="red")
q <- q + scale_x_continuous("Day of year")
q <- q + scale_y_continuous(yVar)
q <- q + labs(title=sprintf("Two harmonics for %s",yVar))
png(file.path(RAWmisc::PROJ$SHARED_TODAY,"NOJULY_2_pg_outcome.png"),width=1000,height=600)
print(q)
dev.off()

q <- ggplot(pd,aes(x=x,y=p-y))
q <- q + geom_point()
q <- q + stat_smooth(se=F)
q <- q + scale_x_continuous("Day of year")
q <- q + scale_y_continuous("Residuals")
q <- q + labs(title=sprintf("Two harmonics for %s",yVar))
png(file.path(RAWmisc::PROJ$SHARED_TODAY,"NOJULY_2_pg_residuals.png"),width=1000,height=600)
print(q)
dev.off()

#### WITH JULY

png(file.path(RAWmisc::PROJ$SHARED_TODAY,"WITHJULY_lomb_scargle_periodogram_pg.png"),width=1000,height=600)
lomb::lsp(as.matrix(na.omit(d[,c(xVar,yVar)])),type="period",from=30,to=400, ofac=20,alpha=0.05)
dev.off()

# formula with one peak/trough
formula <- as.formula(sprintf("%s~sin(2*pi*%s/366)+cos(2*pi*%s/366)",yVar,xVar,xVar))
print(formula)
fit1 <- lm(formula,data=d)
summary(fit1)
pd <- data.table(x=d[[xVar]],y=d[[yVar]],p=predict(fit1,d),isJULY=d$im_sample_month_preg==7)
q <- ggplot(pd,aes(x=x))
q <- q + geom_point(aes(y=y,colour=isJULY))
q <- q + geom_line(aes(y=p),col="red")
q <- q + scale_x_continuous("Day of year")
q <- q + scale_y_continuous(yVar)
q <- q + scale_colour_brewer(palette="Set1")
q <- q + labs(title=sprintf("One harmonic for %s",yVar))
png(file.path(RAWmisc::PROJ$SHARED_TODAY,"WITHJULY_1_pg_outcome.png"),width=1000,height=600)
print(q)
dev.off()

q <- ggplot(pd,aes(x=x,y=p-y))
q <- q + geom_point(aes(colour=isJULY))
q <- q + stat_smooth(se=F)
q <- q + scale_x_continuous("Day of year")
q <- q + scale_y_continuous("Residuals")
q <- q + scale_colour_brewer(palette="Set1")
q <- q + labs(title=sprintf("One harmonic for %s",yVar))
png(file.path(RAWmisc::PROJ$SHARED_TODAY,"WITHJULY_1_pg_residuals.png"),width=1000,height=600)
print(q)
dev.off()


formula <- as.formula(sprintf("%s~sin(2*pi*%s/366)+cos(2*pi*%s/366)+sin(2*pi*%s/183)+cos(2*pi*%s/183)",yVar,xVar,xVar,xVar,xVar))
fit2 <- lm(formula,data=d)
summary(fit2)
pd <- data.table(x=d[[xVar]],y=d[[yVar]],p=predict(fit2,d),isJULY=d$im_sample_month_preg==7)
q <- ggplot(pd,aes(x=x))
q <- q + geom_point(aes(y=y,colour=isJULY))
q <- q + geom_line(aes(y=p),col="red")
q <- q + scale_x_continuous("Day of year")
q <- q + scale_y_continuous(yVar)
q <- q + scale_colour_brewer(palette="Set1")
q <- q + labs(title=sprintf("Two harmonics for %s",yVar))
png(file.path(RAWmisc::PROJ$SHARED_TODAY,"WITHJULY_2_pg_outcome.png"),width=1000,height=600)
print(q)
dev.off()

q <- ggplot(pd,aes(x=x,y=p-y))
q <- q + geom_point(aes(colour=isJULY))
q <- q + stat_smooth(se=F)
q <- q + scale_x_continuous("Day of year")
q <- q + scale_y_continuous("Residuals")
q <- q + scale_colour_brewer(palette="Set1")
q <- q + labs(title=sprintf("Two harmonics for %s",yVar))
png(file.path(RAWmisc::PROJ$SHARED_TODAY,"WITHJULY_2_pg_residuals.png"),width=1000,height=600)
print(q)
dev.off()




## ANOVA PVALUES
anova(fit1,fit2) ## MODEL 2 IS SIGNIFICANTLY DIFFERENT TO MODEL 1
anova(fit2,fit3) ## MODEL 2 IS NOT SIGNIFICANTLY DIFFERENT TO MODEL 3

## LOOK AT LASSO

fitData <- data.table(na.omit(d[,c(yVar,xVar)]))
setnames(fitData,c("y","x"))

fitData[,sin_366:=sin(2*pi*x/366)]
fitData[,cos_366:=cos(2*pi*x/366)]

fitData[,sin_188:=sin(2*pi*x/188)]
fitData[,cos_188:=cos(2*pi*x/188)]

fitData[,sin_91:=sin(2*pi*x/91.5)]
fitData[,cos_91:=cos(2*pi*x/91.5)]

fitData[,sin_45:=sin(2*pi*x/45.75)]
fitData[,cos_45:=cos(2*pi*x/45.75)]

fitData[,sin_22:=sin(2*pi*x/22.875)]
fitData[,cos_22:=cos(2*pi*x/22.875)]

x <- as.matrix(fitData[,-c(1:2)])
y <- fitData$y

fit <- glmnet::cv.glmnet(x,y,nfolds=50)
plot(fit)
coef(fit) # CONTAINS VARIABLES FROM 366 and 188 (i.e. two harmonics)


### PP


yVar <- "zscorePP"
xVar <- "im_sample_day_pp"

png(file.path(RAWmisc::PROJ$SHARED_TODAY,"lomb_scargle_periodogram_pp.png"),width=1000,height=600)
lomb::lsp(as.matrix(na.omit(d[,c(xVar,yVar)])),type="period",from=30,to=400, ofac=20,alpha=0.05)
dev.off()

min(d[[xVar]],na.rm=T)
max(d[[xVar]],na.rm=T)

# formula with one peak/trough
formula <- as.formula(sprintf("%s~sin(2*pi*%s/366)+cos(2*pi*%s/366)",yVar,xVar,xVar))
print(formula)
fit1 <- lm(formula,data=d)
summary(fit1) ##### MODEL IS NOT EVEN SIGNIFICANT!!!!!!!!!!!!!!
pd <- data.table(x=d[[xVar]],y=d[[yVar]],p=predict(fit1,d))
q <- ggplot(pd,aes(x=x))
q <- q + geom_point(aes(y=y))
q <- q + geom_line(aes(y=p),col="red")
q <- q + scale_x_continuous("Day of year")
q <- q + scale_y_continuous(yVar)
q <- q + labs(title=sprintf("One harmonic for %s",yVar))
png(file.path(RAWmisc::PROJ$SHARED_TODAY,"1_pp_outcome.png"),width=1000,height=600)
print(q)
dev.off()

q <- ggplot(pd,aes(x=x,y=p-y))
q <- q + geom_point()
q <- q + stat_smooth(se=F)
q <- q + scale_x_continuous("Day of year")
q <- q + scale_y_continuous("Residuals")
q <- q + labs(title=sprintf("One harmonic for %s",yVar))
png(file.path(RAWmisc::PROJ$SHARED_TODAY,"1_pp_residuals.png"),width=1000,height=600)
print(q)
dev.off()


formula <- as.formula(sprintf("%s~sin(2*pi*%s/366)+cos(2*pi*%s/366)+sin(2*pi*%s/183)+cos(2*pi*%s/183)",yVar,xVar,xVar,xVar,xVar))
fit2 <- lm(formula,data=d)
summary(fit2)
pd <- data.table(x=d[[xVar]],y=d[[yVar]],p=predict(fit2,d))
q <- ggplot(pd,aes(x=x))
q <- q + geom_point(aes(y=y))
q <- q + geom_line(aes(y=p),col="red")
q <- q + scale_x_continuous("Day of year")
q <- q + scale_y_continuous(yVar)
q <- q + labs(title=sprintf("Two harmonics for %s",yVar))
png(file.path(RAWmisc::PROJ$SHARED_TODAY,"2_pp_outcome.png"),width=1000,height=600)
print(q)
dev.off()

q <- ggplot(pd,aes(x=x,y=p-y))
q <- q + geom_point()
q <- q + stat_smooth(se=F)
q <- q + scale_x_continuous("Day of year")
q <- q + scale_y_continuous("Residuals")
q <- q + labs(title=sprintf("Two harmonics for %s",yVar))
png(file.path(RAWmisc::PROJ$SHARED_TODAY,"2_pp_residuals.png"),width=1000,height=600)
print(q)
dev.off()

formula <- as.formula(sprintf("%s~sin(2*pi*%s/366)+cos(2*pi*%s/366)+sin(2*pi*%s/183)+cos(2*pi*%s/183)+sin(2*pi*%s/91.5)+cos(2*pi*%s/91.5)",
                              yVar,xVar,xVar,xVar,xVar,xVar,xVar))
fit3 <- lm(formula,data=d)
pd <- data.table(x=d[[xVar]],y=d[[yVar]],p=predict(fit3,d))
q <- ggplot(pd,aes(x=x))
q <- q + geom_point(aes(y=y))
q <- q + geom_line(aes(y=p),col="red")
q <- q + scale_x_continuous("Day of year")
q <- q + scale_y_continuous(yVar)
q <- q + labs(title=sprintf("Three harmonics for %s",yVar))
png(file.path(RAWmisc::PROJ$SHARED_TODAY,"3_pp_outcome.png"),width=1000,height=600)
print(q)
dev.off()

q <- ggplot(pd,aes(x=x,y=p-y))
q <- q + geom_point()
q <- q + stat_smooth(se=F)
q <- q + scale_x_continuous("Day of year")
q <- q + scale_y_continuous("Residuals")
q <- q + labs(title=sprintf("Three harmonics for %s",yVar))
png(file.path(RAWmisc::PROJ$SHARED_TODAY,"3_pp_residuals.png"),width=1000,height=600)
print(q)
dev.off()


## ANOVA PVALUES
anova(fit1,fit2) ## MODEL 2 IS NOT SIGNIFICANTLY DIFFERENT TO MODEL 1
anova(fit2,fit3) ## MODEL 2 IS NOT SIGNIFICANTLY DIFFERENT TO MODEL 3

## LOOK AT LASSO

fitData <- data.table(na.omit(d[,c(yVar,xVar)]))
setnames(fitData,c("y","x"))

fitData[,sin_366:=sin(2*pi*x/366)]
fitData[,cos_366:=cos(2*pi*x/366)]

fitData[,sin_188:=sin(2*pi*x/188)]
fitData[,cos_188:=cos(2*pi*x/188)]

fitData[,sin_91:=sin(2*pi*x/91.5)]
fitData[,cos_91:=cos(2*pi*x/91.5)]

fitData[,sin_45:=sin(2*pi*x/45.75)]
fitData[,cos_45:=cos(2*pi*x/45.75)]

fitData[,sin_22:=sin(2*pi*x/22.875)]
fitData[,cos_22:=cos(2*pi*x/22.875)]

x <- as.matrix(fitData[,-c(1:2)])
y <- fitData$y

fit <- glmnet::cv.glmnet(x,y,nfolds=50)
plot(fit)
coef(fit) # CONTAINS NO VARIABLES




