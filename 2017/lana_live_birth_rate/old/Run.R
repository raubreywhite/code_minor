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

xtabs(~d$pos_preg)

d <- d[!is.na(pos_preg),c(
  "pos_preg",
  "X_AGE",
  "INF_INTERCOURSE_3MNT_FRQ",
  "PHS_WEIGHT_KG",
  "HEALTH_HYPERTH",
  "HEALTH_HYPOTH",
  "HEALTH_MUSCLE_PAIN",
  "education",
  "SMOKE_DAILY_NOW",
  "SNUFF_NOW",
  "alcohol",
  "previous_child",
  "gravid",
  "previous_miscarriage",
  "phs_length_m",
  "BMI",
  "depression_ever",
  "antidepressants",
  "psychiatric",
  "reason_infert_Q",
  "PAL_total",
  "final_trying_time_years",
  "caffeine_daily",
  "AMH",
  "AFC_TOTAL"
)]

tempData <- mice::mice(d,m=5,maxit=50,meth='pmm',seed=4)
summary(tempData)

a.out <- Amelia::amelia(d, m = 5, noms=c(
  "pos_preg",
  "INF_INTERCOURSE_3MNT_FRQ",
  "HEALTH_HYPERTH",
  "HEALTH_HYPOTH",
  "HEALTH_MUSCLE_PAIN",
  "education",
  "SMOKE_DAILY_NOW",
  "SNUFF_NOW",
  "alcohol",
  "previous_child",
  "gravid",
  "previous_miscarriage",
  "depression_ever",
  "antidepressants",
  "psychiatric","reason_infert_Q"
  ),
  empri=0.01)

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
res[,pos_preg:=d$pos_preg]

res[,pred_pos:=cut(p,breaks=seq(0,1,0.25))]
res[,.(pos_preg=mean(pos_preg,na.rm=T)),by=.(pred_pos)]

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
               max_depth = 5, 
               nround=10, 
               seed = 1,
               nthread = 4,
               objective = "binary:logistic",
               eval.metric = "error",
               eval.metric = "logloss"
)

r <- vector("list",5)
for(i in 1:5){
  p <- predict(xgb, data.matrix(a.out$imputations[[i]][dataTest,-1]))
  pos_preg <- a.out$imputations[[i]][[1]][dataTest]
  r[[i]] <- data.table(p,pos_preg)
}
r <- rbindlist(r)
r[,pred_pos:=cut(p,breaks=seq(0,1,0.25))]
r <- r[,.(pos_preg=mean(pos_preg)),by=.(pred_pos)]
setorder(r,pred_pos)
r

#xgboost::xgb.dump(xgb, with.stats = T)

