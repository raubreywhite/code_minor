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

d <- data.table(openxlsx::read.xlsx(file.path(RAWmisc::PROJ$RAW,"uppstart_prediction_170818.xlsx")))
nrow(d)
d <- d[SEX==2]

d[,AFC_TOTAL:=ULJ_HOGER_AFC+ULJ_VANSTER_AFC]
d[previous_child==1,gravid:=1]

xtabs(~d$livebirth)
d[livebirth==2,livebirth:=NA]


#"HEALTH_HYPERTH",
#"HEALTH_HYPOTH",
#"HEALTH_MUSCLE_PAIN",

xtabs(~d$antidepressants)
xtabs(~d$depression_ever)
xtabs(~d$psychiatric)

d <- d[!is.na(miscarriage) & !is.na(ET) & ET==1,c(
  "miscarriage",
  "embryo_utilization_rate",
  "n_embryo_created",
  "n_mature_egg",
  "n_embryo_used",
  "X_AGE",
  "INF_INTERCOURSE_3MNT_FRQ",
  "PHS_WEIGHT_KG",
  "education",
  "SMOKE_DAILY_NOW",
  "SNUFF_NOW",
  "alcohol",
  "gravid",
  "previous_miscarriage",
  "phs_length_m",
  "BMI",
  "depression_ever",
  "reason_infert_Q",
  "PAL_total",
  "final_trying_time_years",
  "caffeine_daily",
  "AMH",
  "AFC_TOTAL"
)]

cor(d,use="pairwise.complete.obs")
cor(d,use="pairwise.complete.obs")>0.5
sum(c(cor(d,use="pairwise.complete.obs"))>0.5,na.rm=T)

a.out <- Amelia::amelia(d, m = 5, noms=c(
  "miscarriage",
  "INF_INTERCOURSE_3MNT_FRQ",
  "education",
  "SMOKE_DAILY_NOW",
  "SNUFF_NOW",
  "alcohol",
  "gravid",
  "previous_miscarriage",
  "depression_ever",
  "reason_infert_Q"
),
empri=5)

res <- coefs <- fit <- vector("list",length=5)
for(i in 1:5){
  fit[[i]] <- glmnet::cv.glmnet(
    x=as.matrix(a.out$imputations[[i]][,-1]), 
    y=a.out$imputations[[i]][[1]], 
    family="binomial",
    alpha=1,
    nfolds=50)
  
  coefs[[i]] <- as.matrix(coef(fit[[i]]))
  coefs[[i]] <- data.frame(var=row.names(coefs[[i]]),coefs[[i]])
}

coefs <- rbindlist(coefs)
coefs <- coefs[,.(est=mean(X1)),by=var]

for(i in 1:5){
  res[[i]] <- cbind(1,as.matrix(a.out$imputations[[i]][,-1])) %*% matrix(coefs[[2]],ncol=1)
  res[[i]] <- data.table(p=boot::inv.logit(res[[i]]))
  res[[i]][,id:=1:.N]
  setnames(res[[i]],c("p","id"))
}

res <- rbindlist(res)
res <- res[,.(p=mean(p)),by=id]
res[,outcome:=d[[1]]]

res[,pred:=cut(p,breaks=seq(0,1,0.25))]
res[,.(outcome=mean(outcome,na.rm=T)),by=.(pred)]





set.seed(4)
dataTest <- 1:nrow(d)
dataTraining <- sample(dataTest,0.75*length(dataTest),replace=F)
dataTest <- dataTest[!dataTest %in% dataTraining]

testX <- vector("list",5)
testY <- c()
for(i in 1:5){
  testX[[i]] <- a.out$imputations[[i]][dataTraining,-1]
  testY <- c(testY,a.out$imputations[[i]][[1]][dataTraining])
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
  p <- predict(xgb, data.matrix(a.out$imputations[[i]][dataTest,-1]))
  outcome <- a.out$imputations[[i]][[1]][dataTest]
  r[[i]] <- data.table(p,outcome)
}
r <- rbindlist(r)
r[,pred:=cut(p,breaks=seq(0,1,0.25))]
r <- r[,.(outcome=mean(outcome),n=sum(outcome),N=.N),by=.(pred)]
setorder(r,pred)
r

#xgboost::xgb.dump(xgb, with.stats = T)













set.seed(4)
dataTest <- 1:nrow(d)
dataTraining <- sample(dataTest,0.75*length(dataTest),replace=F)
dataTest <- dataTest[!dataTest %in% dataTraining]

res <- coefs <- fit <- vector("list",length=5)
for(i in 1:5){
  fit[[i]] <- gam::gam(
    miscarriage ~ .,
    data=a.out$imputations[[i]][dataTraining,],
    family="binomial")
  
  coefs[[i]] <- as.matrix(coef(fit[[i]]))
  coefs[[i]] <- data.frame(var=row.names(coefs[[i]]),coefs[[i]])
}

coefs <- rbindlist(coefs)
setnames(coefs,c("var","X1"))
coefs <- coefs[,.(est=mean(X1)),by=var]

for(i in 1:5){
  res[[i]] <- cbind(1,as.matrix(a.out$imputations[[i]][dataTest,-1])) %*% matrix(coefs[[2]],ncol=1)
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

