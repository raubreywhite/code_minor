RAWmisc::Initialise()

library(data.table)
library(Hmisc)
library(stringr)
library(magrittr)
library(tidyr)

SMAOpng <- function (file = "Figure.png", w = 1, h = 1, landscape = TRUE) 
{
  width <- 2480/2
  height <- 3508/2
  if (landscape) {
    width <- 3508/2
    height <- 2480/2
  }
  width <- width*w
  height <- height*h
  png(file, width = width, height = height)
}

d <- CleanDataInger()
write.table(data.frame(d$ID,d$IF),"inger_if.csv",row.names=FALSE,dec=",",sep=";")

res <- data.frame(IF=names(d$IF),stringsAsFactors=FALSE)
res$multinomPval <- 100
for(i in 1:nrow(res)){
  runData <- na.omit(data.frame(group=d$groups,IF=d$IF[,res$IF[i]],weights=d$weights))
  if(nrow(runData)==0) next
  runData2 <- mlogit::mlogit.data(runData,choice="group",shape="wide")
  fit <- mlogit::mlogit(group~1 | IF,data=runData2, weights=weights)
  res$multinomPval[i] <- summary(fit)$lratio$p.value[1]
}
dim(res)
res <- res[res$multinomPval<10,]
dim(res)
res$multinomPvalBonf <- res$multinomPval*nrow(res)

res$adjPval <- 100
for(i in 1:nrow(res)){
  runData <- na.omit(data.frame(group=d$groups,IF=d$IF[,res$IF[i]],d$confounders,weights=d$weights))
  if(nrow(runData)==0) next
  runData2 <- mlogit::mlogit.data(runData,choice="group",shape="wide")
  try({
    #fit0 <- mlogit::mlogit(group~1 | alder + parity + BMI + Approx_dagartillPartus +Fastande_vid_blodprov + preeclampsia_hypertension +Smoking + inflammatorisk_sjd,data=runData2, weights=weights)
    #fit1 <- mlogit::mlogit(group~1 | IF + alder + parity + BMI + Approx_dagartillPartus +Fastande_vid_blodprov + preeclampsia_hypertension +Smoking + inflammatorisk_sjd,data=runData2, weights=weights)
    
    fit0 <- mlogit::mlogit(group~1 | alder + parity + BMI + Approx_dagartillPartus +Fastande_vid_blodprov + preeclampsia_hypertension +Smoking,data=runData2, weights=weights)
    fit1 <- mlogit::mlogit(group~1 | IF + alder + parity + BMI + Approx_dagartillPartus +Fastande_vid_blodprov + preeclampsia_hypertension +Smoking,data=runData2, weights=weights)
    res$adjPval[i] <- mlogit::lrtest(fit0,fit1)$`Pr(>Chisq)`[2]
  },TRUE)
}

res$adjPvalBonf <- res$adjPval*nrow(res)

res$NOWEIGHTadjPval <- 100
for(i in 1:nrow(res)){
  runData <- na.omit(data.frame(group=d$groups,IF=d$IF[,res$IF[i]],d$confounders,weights=d$weights))
  if(nrow(runData)==0) next
  runData2 <- mlogit::mlogit.data(runData,choice="group",shape="wide")
  try({
    #fit0 <- mlogit::mlogit(group~1 | alder + parity + BMI + Approx_dagartillPartus +Fastande_vid_blodprov + preeclampsia_hypertension +Smoking + inflammatorisk_sjd,data=runData2, weights=weights)
    #fit1 <- mlogit::mlogit(group~1 | IF + alder + parity + BMI + Approx_dagartillPartus +Fastande_vid_blodprov + preeclampsia_hypertension +Smoking + inflammatorisk_sjd,data=runData2, weights=weights)
    
    fit0 <- mlogit::mlogit(group~1 | alder + parity + BMI + Approx_dagartillPartus +Fastande_vid_blodprov + preeclampsia_hypertension +Smoking,data=runData2)
    fit1 <- mlogit::mlogit(group~1 | IF + alder + parity + BMI + Approx_dagartillPartus +Fastande_vid_blodprov + preeclampsia_hypertension +Smoking,data=runData2)
    res$NOWEIGHTadjPval[i] <- mlogit::lrtest(fit0,fit1)$`Pr(>Chisq)`[2]
  },TRUE)
}

which(res$NOWEIGHTadjPval==100)

res$NOWEIGHTadjPvalBonf <- res$NOWEIGHTadjPval*nrow(res)

sum(res$multinomPval<0.05)
sum(res$adjPval<0.05)
sum(res$NOWEIGHTadjPval<0.05)

sum(res$multinomPvalBonf<0.05)
sum(res$adjPvalBonf<0.05)
sum(res$NOWEIGHTadjPvalBonf<0.05)

write.table(res,file="results/inger_multinomial.csv",row.names=FALSE,sep=";",dec=",")

plotData <- data.frame(groups=d$groups,d$IF[,res$IF[res$adjPvalBonf<0.05]])
plotData <- reshape2::melt(plotData,id="groups")

q <- ggplot(plotData,aes(x=groups,colour=groups,y=value))
q <- q + geom_boxplot(lwd=1.5,outlier.size=3)
q <- q + facet_wrap(~variable,scales="free")
q <- SMAOFormatGGPlot(q, sizeMultiplier=2)
q <- q + scale_colour_brewer("",palette="Set1")
q <- q + scale_x_discrete("",breaks=NULL)
q <- q + scale_y_continuous("")
SMAOpng(paste0("results/inger_boxplots.png"),landscape=FALSE)
print(q)
dev.off()



x <- data.frame(d$IF[,percExists$IF[percExists$perc>0.95]],d$confounders)
for(i in 1:ncol(x)) if(is.factor(x[,i])) x[,i] <- as.numeric(x[,i])-1

keep <- rep(TRUE,nrow(x))
for(i in 1:ncol(x)) keep[is.na(x[,i])] <- FALSE

x <- x[keep,]
y <- d$groups[keep]
weights <- d$weights[keep]

alphas <- seq(0, 1, by=.1)
mses <- numeric(length(alphas))
mins <- numeric(length(alphas))
maxes <- numeric(length(alphas))

for(i in 1:length(alphas)){
  cvfits <- glmnet::cv.glmnet(as.matrix(x),y, weights=weights, alpha=alphas[i], nfolds=10, family="multinomial")
  loc <- which(cvfits$lambda==cvfits$lambda.min)
  maxes[i] <- cvfits$lambda %>% max
  mins[i] <- cvfits$lambda %>% min
  mses[i] <- cvfits$cvm[loc]
}

cvFit <- data.frame(mse=mses, alpha=alphas)
cvFit$mse <- caTools::runmean(cvFit$mse,10)
alpha <- cvFit$alpha[which(cvFit$mse==min(cvFit$mse))]
q <- ggplot(cvFit,aes(x=alpha,y=mse))
q <- q + geom_point(size=5)
q <- q + scale_x_continuous("Alpha parameter (0=ridge, 1=lasso)")
q <- q + scale_y_continuous("LOOCV mean squared error (moving average of width 10 points")
q <- SMAOFormatGGPlot(q, sizeMultiplier=2)
SMAOpng(paste0("results/inger_EN_error.png"),w=1)
print(q)
dev.off()

fit <- glmnet::cv.glmnet(as.matrix(x),y, weights=weights, alpha=1, nfolds=10, family="multinomial")


CSF1
CCL25

plot(fit)
plot(fitEN)



if(lasso){
  alpha=1
} 
fit <- glmnet::cv.glmnet(na.omit(scale(as.matrix(data))), group, alpha=alpha, family="binomial")
oddsRatios <- data.frame((matrix(coef(fit))))
oddsRatios$names <- row.names(coef(fit))
row.names(oddsRatios) <- NULL
oddsRatios <- oddsRatios[-1,]
names(oddsRatios)[1] <- "OR"
oddsRatios$names <- as.character(oddsRatios$names)
#oddsRatios <- oddsRatios[oddsRatios$OR!=0,]
oddsRatios$alpha <- alpha
saveRDS(oddsRatios,file=paste0("results/en_",file,"_OR.RDS"))
}


