RAWmisc::InitialiseOpinionatedUnix(project="code_minor/2018/fotis_reviewing_medical_records/")

# http://www.cookbook-r.com/Statistical_analysis/Inter-rater_reliability/

#nRaters <- 10
#probTrue <- 0.5
#probSD <- 0.01
#sampleSize <- 50
#nSims <- 100

#probPerRater <- runif(n=nRaters, min=0.95, max=0.99)

library(data.table)
library(irr)

RaterResult <- function(raterAgrees,truth,probTrue){
  temp <- raterAgrees
  temp[raterAgrees==T] <- truth[raterAgrees==T]
  temp[raterAgrees==F] <- truth <- sample(x=c(TRUE,FALSE), size=sum(raterAgrees==F), replace=T, prob=c(probTrue,1-probTrue))
  return(temp)
}

#RaterResult(raterAgrees=c(T,T,F,F),truth=c(T,F,T,F))

SimulateKappa <- function(probPerRater, probTrue, sampleSize, nRaters=10, nSims=1000){
  
  simData <- data.table(matrix(ncol = nRaters, nrow = nSims*sampleSize))
  truth <- sample(x=c(TRUE,FALSE), size=nrow(simData), replace=T, prob=c(probTrue,1-probTrue))
  simData[,truth:=truth]
  for(i in 1:nRaters){
    temp <- sample(x=c(TRUE,FALSE), size=nrow(simData), replace=T, prob=c(probPerRater[i],1-probPerRater[i]))
    rater <- RaterResult(raterAgrees=temp,truth=simData$truth,probTrue=probTrue)
    set(simData,j=i,value=rater)
  }
  simData[,id:=rep(1:nSims,times=sampleSize)]
  
  #x <- kappam.fleiss(dat)
  
  res <- simData[,.(
    fleissKappa=kappam.fleiss(data.frame(V1,V2,V3,V4,V5,V6,V7,V8,V9,V10))$value,
    fleissKappaSE=rel::spi(data.frame(V1,V2,V3,V4,V5,V6,V7,V8,V9,V10))$se,
    fleissKappaPval=kappam.fleiss(data.frame(V1,V2,V3,V4,V5,V6,V7,V8,V9,V10))$p.value),
    by=.(id)]
  
  resMOE <- sd(res$fleissKappa,na.rm=T)*1.96
  fleissKappaSE <- mean(res$fleissKappaSE,na.rm=T)
  (trueKappa <- kappam.fleiss(simData[,c("V1","V2","V3","V4","V5","V6","V7","V8","V9","V10")]))
  
  return(list(
    EST_L025_kappa = quantile(res$fleissKappa,probs=0.025,na.rm=T),
    EST_U975_kappa = quantile(res$fleissKappa,probs=0.975,na.rm=T),
    fleissKappaSE = fleissKappaSE,
    power = mean(res$fleissKappaPval<0.05,na.rm=T),
    kappa = trueKappa$value
  )) 
}

stack <- expand.grid(
  sampleSize = seq(2,10,1),
  agreement = 1:6,
  probTrue = c(0.5, 0.75)
  )

res <- vector("list",length=nrow(stack))
pb <- RAWmisc::ProgressBarCreate(min=1, max=nrow(stack))
for(i in 1:nrow(stack)){
  RAWmisc::ProgressBarSet(pb,i)
  if(stack$agreement[i]==1){
    p <- rep(0.2,20)
  } else if(stack$agreement[i]==2){
    p <- rep(0.4,20)
  } else if(stack$agreement[i]==3){
    p <- rep(0.6,20)
  } else if(stack$agreement[i]==4){
    p <- rep(0.7,20)
  } else if(stack$agreement[i]==5){
    p <- rep(0.8,20)
  } else if(stack$agreement[i]==6){
    p <- rep(0.95,20)
  } 
  
  temp <- SimulateKappa(
    probPerRater=p,
    probTrue=stack$probTrue[i],
    sampleSize=stack$sampleSize[i],
    nRaters=20,
    nSims=1000
  )
  
  res[[i]] <- data.frame(
    fleissKappaSE = RAWmisc::Format(temp$fleissKappaSE,2),
    REAL_kappa=RAWmisc::Format(temp$kappa,2),
    EST_L025_kappa=RAWmisc::Format(temp$EST_L025_kappa,2),
    EST_U975_kappa=RAWmisc::Format(temp$EST_U975_kappa,2),
    POWER=RAWmisc::Format(temp$power*100,1),
    sampleSize=stack$sampleSize[i],
    agreementLevel=stack$agreement[i],
    probTrue=stack$probTrue[i]
    )
}

res <- rbindlist(res)
setorder(res,probTrue,agreementLevel,sampleSize)
setcolorder(res,c("probTrue","agreementLevel","REAL_kappa","sampleSize","EST_L025_kappa","EST_U975_kappa","fleissKappaSE","POWER"))
res

openxlsx::write.xlsx(res,file=file.path(RAWmisc::PROJ$SHARED_TODAY,"sample_size.xlsx"))





data(diagnoses)
x <- kappam.fleiss(diagnoses)
x

y <- rel::spi(diagnoses)
