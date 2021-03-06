RAWmisc::AllowFileManipulationFromInitialiseProject()
RAWmisc::InitialiseProject(
  HOME = "/git/code_minor/lana_live_birth_rate/",
  RAW = "/analyses/data_raw/code_minor/lana_live_birth_rate/",
  CLEAN = "/analyses/data_clean/code_minor/lana_live_birth_rate",
  BAKED = "/analyses/results_baked/code_minor/lana_live_birth_rate/",
  FINAL = "/analyses/results_final/code_minor/lana_live_birth_rate/",
  SHARED = "/dropbox/results_shared/lana_live_birth_rate/"
)

library(data.table)
library(ggplot2)

masterdata <- data.table(openxlsx::read.xlsx(file.path(RAWmisc::PROJ$RAW,"uppstart_prediction_170818.xlsx")))
nrow(masterdata)
masterdata <- masterdata[SEX==2]

masterdata[,AFC_TOTAL:=ULJ_HOGER_AFC+ULJ_VANSTER_AFC]
masterdata[previous_child==1,gravid:=1]

table(masterdata$ET[n_embryo_created==0],useNA="always")
masterdata[n_embryo_created==0]$ET
masterdata[pos_preg==0]$ET

xtabs(~masterdata$pos_preg)
xtabs(~masterdata$miscarriage)
xtabs(~masterdata$livebirth)

masterdata[,birth:=livebirth]
xtabs(~masterdata$birth)
masterdata[ET==0,birth:=0]
xtabs(~masterdata$birth)
masterdata[pos_preg==0,birth:=0]
xtabs(~masterdata$birth)
masterdata[miscarriage==1,birth:=0]
xtabs(~masterdata$birth)

masterdata[livebirth==2,birth:=NA]
masterdata[livebirth==2,livebirth:=NA]

masterdata[final_trying_time_years>100,final_trying_time_years:=NA]
#"HEALTH_HYPERTH",
#"HEALTH_HYPOTH",
#"HEALTH_MUSCLE_PAIN",

# EGG ASPIRATION YES/NO

d <- masterdata[!is.na(egg_asp),c(
  "egg_asp",
  "X_AGE",
  "INF_INTERCOURSE_3MNT_FRQ",
  "education",
  "SMOKE_DAILY_NOW",
  "alcohol",
  "previous_miscarriage",
  "BMI",
  "depression_ever",
  "reason_infert_Q",
  "PAL_total",
  "final_trying_time_years",
  "caffeine_daily",
  "AMH",
  "AFC_TOTAL"
)]

data <- mice::mice(d, m=5, method="pmm")
fit <- with(data,glm2::glm2(egg_asp~
                      splines::ns(X_AGE,2)+
                      factor(INF_INTERCOURSE_3MNT_FRQ)+
                      education+
                      SMOKE_DAILY_NOW+
                      alcohol+
                      previous_miscarriage+
                      splines::ns(BMI,2)+
                      depression_ever+
                      factor(reason_infert_Q)+
                      splines::ns(PAL_total,2)+
                      splines::ns(final_trying_time_years,2)+
                      splines::ns(caffeine_daily,2)+
                      splines::ns(AMH,2)+
                      splines::ns(AFC_TOTAL,2),
                    family="binomial"))
summary(pool(fit))

# EGG ASPIRATION NUMBER

d <- masterdata[!is.na(ASP_EGG),c(
  "ASP_EGG",
  "X_AGE",
  "INF_INTERCOURSE_3MNT_FRQ",
  "education",
  "SMOKE_DAILY_NOW",
  "alcohol",
  "previous_miscarriage",
  "BMI",
  "depression_ever",
  "reason_infert_Q",
  "PAL_total",
  "final_trying_time_years",
  "caffeine_daily",
  "AMH",
  "AFC_TOTAL"
)]

data <- mice::mice(d, m=5, method="pmm")

fit0 <- with(data,glm2::glm2(round(ASP_EGG)~1,
                             family="poisson"))
fit <- with(data,glm2::glm2(round(ASP_EGG)~
                              splines::ns(X_AGE,2)+
                              factor(INF_INTERCOURSE_3MNT_FRQ)+
                              education+
                              SMOKE_DAILY_NOW+
                              alcohol+
                              previous_miscarriage+
                              splines::ns(BMI,2)+
                              depression_ever+
                              factor(reason_infert_Q)+
                              splines::ns(PAL_total,2)+
                              splines::ns(final_trying_time_years,2)+
                              splines::ns(caffeine_daily,2)+
                              splines::ns(AMH,2)+
                              splines::ns(AFC_TOTAL,2),
                            family="poisson"))
summary(pool(fit))
pc <- mice::pool.compare(fit, fit0, method="Wald")
pc$pvalue

retval <- list("vector",5)
for(i in 1:5){
  retval[[i]] <- data.table(complete(data,i))
  retval[[i]][,p:=predict(fit$analyses[[i]],complete(data,i),type="response")]
  retval[[i]][,id:=1:.N]
}
retval <- rbindlist(retval)
retval <- retval[, lapply(.SD, mean, na.rm=TRUE), by=id ]

q <- ggplot(retval,aes(y=ASP_EGG-p,x=p))
q <- q + geom_point()
q <- q + geom_hline(yintercept=0)
q

retval[,errorModel:=ASP_EGG-p]
retval[,errorNull:=ASP_EGG-mean(ASP_EGG)]

retval[,.(
  rmseModel=sqrt(mean(errorModel^2)),
  rmseNull=sqrt(mean(errorNull^2))
)]

fit0 <- with(data,glm2::glm2(round(ASP_EGG)~1,
                            family="poisson"))
summary(pool(fit))
fit <- with(data,glm2::glm2(round(ASP_EGG)~
                              splines::ns(X_AGE,2)+
                              factor(INF_INTERCOURSE_3MNT_FRQ)+
                              SMOKE_DAILY_NOW+
                              alcohol+
                              splines::ns(BMI,2)+
                              splines::ns(final_trying_time_years,2)+
                              splines::ns(AMH,2),
                            family="poisson"))
summary(pool(fit))
pc <- mice::pool.compare(fit, fit0, method="Wald")
pc$pvalue

retval <- list("vector",5)
for(i in 1:5){
  retval[[i]] <- data.table(complete(data,i))
  retval[[i]][,p:=predict(fit$analyses[[i]],complete(data,i),type="response")]
  retval[[i]][,id:=1:.N]
}
retval <- rbindlist(retval)
retval <- retval[, lapply(.SD, mean, na.rm=TRUE), by=id ]

q <- ggplot(retval,aes(y=ASP_EGG-p,x=p))
q <- q + geom_point()
q <- q + geom_hline(yintercept=0)
q

retval[,errorModel:=ASP_EGG-p]
retval[,errorNull:=ASP_EGG-mean(ASP_EGG)]

retval[,.(
  rmseModel=sqrt(mean(errorModel^2)),
  rmseNull=sqrt(mean(errorNull^2))
)]


# NUMBER EMBRYOS CREATED

d <- masterdata[!is.na(n_embryo_created),c(
  "n_embryo_created",
  "X_AGE",
  "INF_INTERCOURSE_3MNT_FRQ",
  "education",
  "SMOKE_DAILY_NOW",
  "alcohol",
  "previous_miscarriage",
  "BMI",
  "depression_ever",
  "reason_infert_Q",
  "PAL_total",
  "final_trying_time_years",
  "caffeine_daily",
  "AMH",
  "AFC_TOTAL"
)]

data <- mice::mice(d, m=5, method="pmm")

fit0 <- with(data,glm2::glm2(round(n_embryo_created)~1,
                             family="poisson"))
fit <- with(data,glm2::glm2(round(n_embryo_created)~
                              splines::ns(X_AGE,2)+
                              factor(INF_INTERCOURSE_3MNT_FRQ)+
                              education+
                              SMOKE_DAILY_NOW+
                              alcohol+
                              previous_miscarriage+
                              splines::ns(BMI,2)+
                              depression_ever+
                              factor(reason_infert_Q)+
                              splines::ns(PAL_total,2)+
                              splines::ns(final_trying_time_years,2)+
                              splines::ns(caffeine_daily,2)+
                              splines::ns(AMH,2)+
                              splines::ns(AFC_TOTAL,2),
                            family="poisson"))
summary(pool(fit))
pc <- mice::pool.compare(fit, fit0, method="Wald")
pc$pvalue


retval <- list("vector",5)
for(i in 1:5){
  retval[[i]] <- data.table(complete(data,i))
  retval[[i]][,p:=predict(fit$analyses[[i]],complete(data,i),type="response")]
  retval[[i]][,id:=1:.N]
}
retval <- rbindlist(retval)
retval <- retval[, lapply(.SD, mean, na.rm=TRUE), by=id ]

q <- ggplot(retval,aes(y=n_embryo_created-p,x=p))
q <- q + geom_point()
q <- q + geom_hline(yintercept=0)
q

retval[,errorModel:=n_embryo_created-p]
retval[,errorNull:=n_embryo_created-mean(n_embryo_created)]

retval[,.(
  rmseModel=sqrt(mean(errorModel^2)),
  rmseNull=sqrt(mean(errorNull^2))
)]

fit0 <- with(data,glm2::glm2(round(n_embryo_created)~1,
                             family="poisson"))
fit <- with(data,glm2::glm2(round(n_embryo_created)~
                              splines::ns(X_AGE,2)+
                              factor(INF_INTERCOURSE_3MNT_FRQ)+
                              SMOKE_DAILY_NOW+
                              alcohol+
                              splines::ns(BMI,2)+
                              splines::ns(final_trying_time_years,2)+
                              splines::ns(AMH,2),
                            family="poisson"))
summary(pool(fit))
pc <- mice::pool.compare(fit, fit0, method="Wald")
pc$pvalue

retval <- list("vector",5)
for(i in 1:5){
  retval[[i]] <- data.table(complete(data,i))
  retval[[i]][,p:=predict(fit$analyses[[i]],complete(data,i),type="response")]
  retval[[i]][,id:=1:.N]
}
retval <- rbindlist(retval)
retval <- retval[, lapply(.SD, mean, na.rm=TRUE), by=id ]

q <- ggplot(retval,aes(y=n_embryo_created-p,x=p))
q <- q + geom_point()
q <- q + geom_hline(yintercept=0)
q

retval[,errorModel:=n_embryo_created-p]
retval[,errorNull:=n_embryo_created-mean(n_embryo_created)]

retval[,.(
  rmseModel=sqrt(mean(errorModel^2)),
  rmseNull=sqrt(mean(errorNull^2))
)]

fit0 <- with(data,glm2::glm2(round(n_embryo_created)~1,
                             family="poisson"))
fit <- with(data,glm2::glm2(round(n_embryo_created)~
                              SMOKE_DAILY_NOW+
                              alcohol+
                              BMI,
                            family="poisson"))
summary(pool(fit))
pc <- mice::pool.compare(fit, fit0, method="Wald")
pc$pvalue

retval <- list("vector",5)
for(i in 1:5){
  retval[[i]] <- data.table(complete(data,i))
  retval[[i]][,p:=predict(fit$analyses[[i]],complete(data,i),type="response")]
  retval[[i]][,id:=1:.N]
}
retval <- rbindlist(retval)
retval <- retval[, lapply(.SD, mean, na.rm=TRUE), by=id ]

q <- ggplot(retval,aes(y=n_embryo_created-p,x=p))
q <- q + geom_point()
q <- q + geom_hline(yintercept=0)
q

retval[,errorModel:=n_embryo_created-p]
retval[,errorNull:=n_embryo_created-mean(n_embryo_created)]

retval[,.(
  rmseModel=sqrt(mean(errorModel^2)),
  rmseNull=sqrt(mean(errorNull^2))
)]


































q <- ggplot(retval,aes(x=X_AGE,y=ASP_EGG))
q <- q + geom_point()
q


retval <- retval[,.(p=mean(p),)]

f <- as.formula(livebirth ~ .*.)
y <- data$imputations[[i]][[1]]
x <- model.matrix(f, data$imputations[[i]])[, -1]

with(fit[[1]],predict)
with(data,predict(fit))



data <- Amelia::amelia(d, m = 5, noms=c(
  "egg_asp",
  "INF_INTERCOURSE_3MNT_FRQ",
  "education",
  "SMOKE_DAILY_NOW",
  "alcohol",
  "previous_miscarriage",
  "depression_ever",
  "reason_infert_Q"
),
empri=5)

data <- transform(data, AGE_SPLINES=splines::ns(X_AGE,2))
data <- transform(data, BMI_SPLINES=splines::ns(BMI,2))
data <- transform(data, PAL_total_SPLINES=splines::ns(PAL_total,2))
data <- transform(data, final_trying_time_years_SPLINES=splines::ns(final_trying_time_years,2))
data <- transform(data, caffeine_daily_SPLINES=splines::ns(caffeine_daily,2))
data <- transform(data, AMH_SPLINES=splines::ns(AMH,2))
data <- transform(data, AFC_TOTAL_SPLINES=splines::ns(AFC_TOTAL,2))


fit <- Zelig::zelig(egg_asp~
                      AGE_SPLINES.1+AGE_SPLINES.2+
                      factor(INF_INTERCOURSE_3MNT_FRQ)+
                      education+
                      SMOKE_DAILY_NOW+
                      alcohol+
                      previous_miscarriage+
                      BMI_SPLINES.1+BMI_SPLINES.2+
                      depression_ever+
                      factor(reason_infert_Q)+
                      PAL_total_SPLINES.1+PAL_total_SPLINES.2+
                      final_trying_time_years_SPLINES.1+final_trying_time_years_SPLINES.2+
                      caffeine_daily_SPLINES.1+caffeine_daily_SPLINES.2+
                      AMH_SPLINES.1+AMH_SPLINES.2+
                      AFC_TOTAL_SPLINES.1+AFC_TOTAL_SPLINES.2,
                    data=data)

summary(fit)
x.out <- Zelig::setx(fit)
s.out <- Zelig::sim(fit, x=x.out)

nd <- copy(d)
nd[,p:=exp(apply(do.call(rbind,predict(fit)),2,mean))]

q <- ggplot(nd,aes(x=p,y=n_embryo_created))
q <- q + geom_point()
q <- q + geom_abline()
q

q <- ggplot(nd,aes(x=BMI,y=n_embryo_created))
q <- q + geom_point()
q

q <- ggplot(nd,aes(x=X_AGE,y=n_embryo_created))
q <- q + geom_point()
q



x <- Zelig::sim(fit)
predict(fit,as.data.frame(data$imputations[[1]]))

data <- mice::mice(d, m = 5, method="pmm")

fit <- with(data,MASS::glm.nb(
  n_embryo_created~
    splines::ns(X_AGE,2)+
    factor(INF_INTERCOURSE_3MNT_FRQ)+
    education+
    SMOKE_DAILY_NOW+
    alcohol+
    previous_miscarriage+
    splines::ns(BMI,2)+
    depression_ever+
    factor(reason_infert_Q)+
    splines::ns(PAL_total,2)+
    splines::ns(final_trying_time_years,2)+
    splines::ns(caffeine_daily,2)+
    splines::ns(AMH,2)+
    splines::ns(AFC_TOTAL,2)
))
summary(pool(fit))

nd <- copy(d)
nd[,p:=predict(pool(fit),d,type="response")]

q <- ggplot(nd,aes(x=n_embryo_created,y=p))
q <- q + geom_point()
q <- q + geom_abline()
q

retval <- vector("list",20)
for(i in 1:20){
  retval[[i]] <- copy(d)
  retval[[i]][,BMI:=14.0+i]
  retval[[i]][,p:=predict(fit,retval[[i]],type="response")]
}
retval <- rbindlist(retval)
retval <- retval[,.(p=mean(p,na.rm=T)),by=BMI]

q <- ggplot(retval,aes(x=BMI))
q <- q + geom_point(aes(y=p))
q



cor(d,use="pairwise.complete.obs")

form <- sprintf("%s~%s",names(d)[1],paste(names(d)[-1],collapse="+"))
form <- as.formula(form)
summary(glm2::glm2(as.formula(form), data=d,family="binomial"))


cor(d,use="pairwise.complete.obs")>0.5
sum(c(cor(d,use="pairwise.complete.obs"))>0.5,na.rm=T)

d[,reason_infert_Q:=as.factor(reason_infert_Q)]
xtabs(~d$reason_infert_Q)
d[,INF_INTERCOURSE_3MNT_FRQ:=as.factor(INF_INTERCOURSE_3MNT_FRQ)]





fit <- with(data, glm2::glm2(as.formula(form), family="binomial"))
summary(pool(fit))

######LOGISTIC

res <- coefs <- fit <- vector("list",length=5)
for(i in 1:5){
  fit[[i]] <- glm2::glm2(birth ~ ., data=data$imputations[[i]])
  
  coefs[[i]] <- as.matrix(coef(fit[[i]]))
  coefs[[i]] <- data.frame(var=row.names(coefs[[i]]),coefs[[i]])
}

coefs <- rbindlist(coefs)
setnames(coefs,c("var","X1"))
coefs <- coefs[,.(est=mean(X1)),by=var]

for(i in 1:5){
  res[[i]] <- cbind(1,as.matrix(data$imputations[[i]][,-1])) %*% matrix(coefs[[2]],ncol=1)
  res[[i]] <- data.table(p=boot::inv.logit(res[[i]]))
  res[[i]][,id:=1:.N]
  setnames(res[[i]],c("p","id"))
}

res <- rbindlist(res)
res <- res[,.(p=mean(p)),by=id]
res[,outcome:=d[[1]]]

res[,pred:=cut(p,breaks=seq(0,1,0.25))]
res[,.(outcome=mean(outcome),n=sum(outcome),N=.N),by=.(pred)]

f <- glm(outcome~p,data=res)
summary(f)


######### lasso

res <- coefs <- fit <- vector("list",length=5)
for(i in 1:5){
  fit[[i]] <- glmnet::cv.glmnet(
    x=as.matrix(data$imputations[[i]][,-1]), 
    y=data$imputations[[i]][[1]], 
    family="binomial",
    alpha=1,
    nfolds=50)
  
  coefs[[i]] <- as.matrix(coef(fit[[i]]))
  coefs[[i]] <- data.frame(var=row.names(coefs[[i]]),coefs[[i]])
}

coefs <- rbindlist(coefs)
coefs <- coefs[,.(est=mean(X1)),by=var]

for(i in 1:5){
  res[[i]] <- cbind(1,as.matrix(data$imputations[[i]][,-1])) %*% matrix(coefs[[2]],ncol=1)
  res[[i]] <- data.table(p=boot::inv.logit(res[[i]]))
  res[[i]][,id:=1:.N]
  setnames(res[[i]],c("p","id"))
}

res <- rbindlist(res)
res <- res[,.(p=mean(p)),by=id]
res[,outcome:=d[[1]]]

res[,pred:=cut(p,breaks=seq(0,1,0.25))]
res[,.(outcome=mean(outcome),n=sum(outcome),N=.N),by=.(pred)]

f <- glm(outcome~p,data=res)
summary(f)


##### INTERACTIONS

res <- coefs <- fit <- vector("list",length=5)
for(i in 1:5){
  print(i)
  f <- as.formula(livebirth ~ .*.)
  y <- data$imputations[[i]][[1]]
  x <- model.matrix(f, data$imputations[[i]])[, -1]
  
  fit[[i]] <- glmnet::cv.glmnet(
    x=x, 
    y=y, 
    family="binomial",
    alpha=1,
    nfolds=5)
  
  coefs[[i]] <- as.matrix(coef(fit[[i]]))
  coefs[[i]] <- data.frame(var=row.names(coefs[[i]]),coefs[[i]])
}

coefs <- rbindlist(coefs)
coefs <- coefs[,.(est=mean(X1)),by=var]

for(i in 1:5){
  f <- as.formula(livebirth ~ .*.)
  y <- data$imputations[[i]][[1]]
  x <- model.matrix(f, data$imputations[[i]])[, -1]
  
  res[[i]] <- cbind(1,x) %*% matrix(coefs[[2]],ncol=1)
  res[[i]] <- data.table(p=boot::inv.logit(res[[i]]))
  res[[i]][,id:=1:.N]
  setnames(res[[i]],c("p","id"))
}

res <- rbindlist(res)
res <- res[,.(p=mean(p)),by=id]
res[,outcome:=d[[1]]]

res[,pred:=cut(p,breaks=seq(0,1,0.25))]
res[,.(outcome=mean(outcome),n=sum(outcome),N=.N),by=.(pred)]


######









set.seed(4)
dataTest <- 1:nrow(d)
dataTraining <- sample(dataTest,0.75*length(dataTest),replace=F)
dataTest <- dataTest[!dataTest %in% dataTraining]

testX <- vector("list",5)
testY <- c()
for(i in 1:5){
  testX[[i]] <- data$imputations[[i]][dataTraining,-1]
  testY <- c(testY,data$imputations[[i]][[1]][dataTraining])
}
testX <- rbindlist(testX)

xgb <- xgboost::xgboost(data = data.matrix(testX), 
                        label = testY, 
                        eta=0.2,
                        max_depth = 10, 
                        nround=20, 
                        seed = 1,
                        nthread = 4,
                        objective = "binary:logistic",
                        eval.metric = "error",
                        eval.metric = "logloss"
)

r <- vector("list",5)
for(i in 1:5){
  p <- predict(xgb, data.matrix(data$imputations[[i]][dataTest,-1]))
  outcome <- data$imputations[[i]][[1]][dataTest]
  r[[i]] <- data.table(p,outcome)
}
r <- rbindlist(r)
r[,pred:=cut(p,breaks=seq(0,1,0.25))]
#r <- r[,.(outcome=mean(outcome),n=sum(outcome),N=.N),by=.(pred)]
#setorder(r,pred)
#r

f <- glm(outcome~p,data=r)
summary(f)


#xgboost::xgb.dump(xgb, with.stats = T)













set.seed(4)
dataTest <- 1:nrow(d)
dataTraining <- sample(dataTest,0.75*length(dataTest),replace=F)
dataTest <- dataTest[!dataTest %in% dataTraining]

res <- coefs <- fit <- vector("list",length=5)
for(i in 1:5){
  fit[[i]] <- gam::gam(
    livebirth ~ .,
    data=data$imputations[[i]][dataTraining,],
    family="binomial")
  
  coefs[[i]] <- as.matrix(coef(fit[[i]]))
  coefs[[i]] <- data.frame(var=row.names(coefs[[i]]),coefs[[i]])
}

coefs <- rbindlist(coefs)
setnames(coefs,c("var","X1"))
coefs <- coefs[,.(est=mean(X1)),by=var]

for(i in 1:5){
  res[[i]] <- cbind(1,as.matrix(data$imputations[[i]][dataTest,-1])) %*% matrix(coefs[[2]],ncol=1)
  res[[i]] <- data.table(p=boot::inv.logit(res[[i]]))
  res[[i]][,id:=1:.N]
  res[[i]][,outcome:=d[[1]][dataTest]]
  setnames(res[[i]],c("p","id","outcome"))
}

res <- rbindlist(res)
res <- res[,.(p=mean(p),outcome=mean(outcome)),by=id]

res[,pred:=cut(p,breaks=seq(0,1,0.25))]
res <- res[,.(outcome=mean(outcome,na.rm=T)),by=.(pred)]
setorder(res,pred)
res

