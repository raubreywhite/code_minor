org::AllowFileManipulationFromInitialiseProject()
org::InitialiseProject(
  HOME = "/git/code_minor/2018/inger_pcos_fecundity/",
  RAW = "/Volumes/crypt_data/org/data_raw/code_minor/2018/inger_pcos_fecundity/"
)

library(foreach)
library(data.table)

StackIterator <- function(vals) {
  . <- NULL
  
  pb <- RAWmisc::ProgressBarCreate(min=1,max=length(vals))
  it <- iterators::icount(length(vals))
  
  nextEl <- function() {
    i <- it$nextElem()
    RAWmisc::ProgressBarSet(pb,i)
    vals[i]
  }
  
  obj <- list(nextElem = nextEl)
  class(obj) <- c("abstractiter", "iter")
  obj
}

cl <- parallel::makeCluster(8)
doSNOW::registerDoSNOW(cl)

res <- foreach(i = StackIterator(vals=1:500)) %dopar% {
  library(data.table)
  
  covs <- data.frame(
    id = 1:2000,
    trt = stats::rbinom(1000, 1L, 0.5),
    age = rpois(1000,lambda = 30)
  )
  s1 <- simsurv::simsurv(
    lambdas = 0.1,
    gammas = 1.5,
    betas = c(trt = -0.5, age = -0.1),
    x = covs,
    maxt = 10)
  d <- data.table(cbind(s1,covs))
  fit <- survival::coxph(survival::Surv(eventtime, status) ~ trt + age, data=d)
  
  dx <- as.data.table(d)
  dx0 <- dx[status==0]
  dx0[,weights:=.N/nrow(dx)]
  dx1 <- dx[status==1]
  dx1[,weights:=.N/nrow(dx)]
  
  dcc <- rbind(dx0[1:100],dx1[1:100])
  fitcc <- survival::coxph(survival::Surv(eventtime, status) ~ trt + age, data=dcc)
  fitccweight <- survival::coxph(survival::Surv(eventtime, status) ~ trt + age, data=dcc, weights=weights)
  
  dx0[,idold:=id]
  dx0[,id:=NA]
  for(i in 1:nrow(dx1)){
    n <- nrow(dx0[is.na(id) & age==dx1[i]$age])
    n <- min(3,n)
    if(n>0){
      matched <- dx0[is.na(id) & age==dx1[i]$age][1:n]$idold
      dx0[idold %in% matched, id:=dx1[i]$id]
    }
  }
  dx0[,idold:=NULL]
  
  dmatchcc <- rbind(dx1[id %in% dx0$id],dx0[!is.na(id)])
  
  dmatchcc[,numInGroup:=.N,by=id]
  dmatchcc[,numerator:=1]
  dmatchcc[status==0,numerator:=numInGroup-1]
  dmatchcc[,propObservedInGroup:=numerator/numInGroup]
  
  dmatchcc[status==0,propDesired:=sum(d$status==0)/nrow(d)]
  dmatchcc[status==1,propDesired:=sum(d$status==1)/nrow(d)]
  
  dmatchcc[,weights:=propDesired/propObservedInGroup]
  fitmatchcc <- survival::coxph(survival::Surv(eventtime, status) ~ trt + age, data=dmatchcc)
  #fitccweight <- survival::coxph(survival::Surv(eventtime, status) ~ trt + age, data=dcc, weights=weights)
  fitmatchccmm <- coxme::coxme(survival::Surv(eventtime, status) ~ trt + age + (1|id), data=dmatchcc)
  fitmatchccmmweight <- coxme::coxme(survival::Surv(eventtime, status) ~ trt + age + (1|id), data=dmatchcc, weights=weights)
  
  data.table(
    vars=names(fit$coefficients),
    orig=fit$coefficients,
    cc=fitcc$coefficients,
    ccweight=fitccweight$coefficients,
    matchcc=fitmatchcc$coefficients,
    matchccmm=fitmatchccmm$coefficients,
    matchccmmweight=fitmatchccmmweight$coefficients)
}
parallel::stopCluster(cl)
res <- rbindlist(res)

res[,.(
  orig=mean(orig),
  cc=mean(cc),
  ccweight=mean(ccweight),
  matchcc=mean(matchcc),
  matchccmm=mean(matchccmm),
  matchccmmweight=mean(matchccmmweight)
),keyby=vars]

