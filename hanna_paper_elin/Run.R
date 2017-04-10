RAWmisc::InitialiseProject(
  PROJHOME = "/analyses/code_minor/hanna_paper_elin/",
  PROJRAW = "/analyses/data_raw/hanna_paper_elin/",
  PROJCLEAN = "/analyses/data_clean/hanna_paper_elin",
  PROJBAKED = "/analyses/results_baked/hanna_paper_elin/",
  PROJFINAL = "/analyses/results_final/hanna_paper_elin/",
  PROJSHARED = "/dropbox/results_shared/hanna_paper_elin/")

suppressWarnings(suppressMessages(library(data.table)))
suppressWarnings(suppressMessages(library(ggplot2)))
suppressWarnings(suppressMessages(library(tidyr)))
suppressWarnings(suppressMessages(library(magrittr)))
suppressWarnings(suppressMessages(library(pomp)))

assign("RUN_ALL", TRUE, envir=globalenv())

unlink(file.path(RPROJ$PROJSHARED,lubridate::today()), recursive=TRUE, force=TRUE)
dir.create(file.path(RPROJ$PROJSHARED,lubridate::today()))

CleanData()
rmarkdown::render("Notes.Rmd", output_dir = file.path(RPROJ$PROJSHARED,lubridate::today()))
cmd <- sprintf("cd %s; zip -P sqo391 %s %s",
               RPROJ$PROJBAKED,
               file.path(RPROJ$PROJSHARED,lubridate::today(),"data.zip"),
               "data_inflammation_factors.xlsx")
system(cmd)

utils::data(anorexia, package = "MASS")
anorexia$outcome <- 0
anorexia$outcome[1:20] <- 1

fit <- glm(outcome ~ Prewt + splines::ns(Postwt,df=3), family=binomial, data=anorexia)
summary(fit)
attr(terms(fit), "predvars")
plot(effect("Postwt",fit,xlevels=list(Postwt=c(100,140))))


library(Hmisc)
library(survival)
library(rms)

n <- 1000    # define sample size
set.seed(17) # so can reproduce the results
age            <- rnorm(n, 50, 10)
blood.pressure <- rnorm(n, 120, 15)
cholesterol    <- rnorm(n, 200, 25)
sex            <- factor(sample(c('female','male'), n,TRUE))
label(age)            <- 'Age'      # label is in Hmisc
label(cholesterol)    <- 'Total Cholesterol'
label(blood.pressure) <- 'Systolic Blood Pressure'
label(sex)            <- 'Sex'
units(cholesterol)    <- 'mg/dl'   # uses units.default in Hmisc
units(blood.pressure) <- 'mmHg'
fit <- lrm(y ~ blood.pressure + sex * (age + rcs(cholesterol,4)),
           x=TRUE, y=TRUE)
#      x=TRUE, y=TRUE allows use of resid(), which.influence below
#      could define d <- datadist(fit) after lrm(), but data distribution
#      summary would not be stored with fit, so later uses of Predict
#      or summary.rms would require access to the original dataset or
#      d or specifying all variable values to summary, Predict, nomogram
anova(fit)
p <- Predict(fit, age, sex)
ggplot(p)   # or plot()

data(pbc)
d <- pbc
rm(pbc)
d$died <- ifelse(d$status == 2, 1, 0)
d$status <- ifelse(d$status != 0, 1, 0)

ddist <- datadist(d)
options(datadist='ddist')

fit <- lrm(status ~ rcs(age, 4), data=d)
(an <- anova(fit))
plot(Predict(fit), anova=an, pval=TRUE)

fit2 <- cph(Surv(time, status) ~  rcs(age, 4), data=d)
(an2 <- anova(fit2))
plot(Predict(fit2), anova=an, pval=TRUE)

getProbability <- function(x) {
  exp(x)/(1+exp(x))*100
}

fit <- lrm(status ~ rcs(age, 4), data=d)
(an <- anova(fit))
plot(Predict(fit, fun=getProbability), anova=an, pval=TRUE, ylab="Probability of death [%]")

# overall probability to die
table(d$status)
round(table(d$status)[[2]]/sum(table(d$status))*100, digits=1) # = 44.5%

library(mfx)
# simulate some data
set.seed(12345)
n = 1000
x = rnorm(n)
# binary outcome
y = ifelse(pnorm(1 + 0.5*x + rnorm(n))>0.5, 1, 0)
data = data.frame(y,x)
logitmfx(formula=y~splines::ns(x,df=5), data=data)



library(lme4)
library(effects)
data(cars)
cars$xvariable <- rnorm(nrow(cars))
cars$outcome <- 0
cars$outcome[cars$speed > 18] <- 1
fm1 <- lm(dist ~ speed + xvariable, cars) #sleepstudy is an example dataset in lme4   
plot(effect("speed", fm1))

fm1 <- lm(dist ~ splines::ns(speed,df=5) + xvariable, cars) #sleepstudy is an example dataset in lme4   
plot(effect("splines::ns(speed, df = 5)", fm1))

fm1 <- glm(outcome ~ as.numeric(speed), data=rbind(cars,cars,cars,cars,cars), family=binomial) #sleepstudy is an example dataset in lme4   
plot(effect("speed", fm1))

fm1 <- glm(dist ~ splines::ns(speed,df=5) + xvariable, data=cars, family=binomial) #sleepstudy is an example dataset in lme4   
plot(effect("splines::ns(speed, df = 5)", fm1))

data(iris)
iris$outcome <- 0
iris$outcome[1:50] <- 1

fm1 <- glm(outcome ~ Sepal.Length+Sepal.Width, data=iris, family=binomial) #sleepstudy is an example dataset in lme4   
plot(effect("Sepal.Length", fm1))







library(rms)
#### CREATE DATA
n <- 1000    # define sample size
set.seed(17) # so can reproduce the results
age            <- rnorm(n, 50, 10)
blood.pressure <- rnorm(n, 120, 15)
cholesterol    <- rnorm(n, 200, 25)
sex            <- factor(sample(c('female','male'), n,TRUE))

# Specify population model for log odds that Y=1
L <- .4*(sex=='male') + .045*(age-50) +
  (log(cholesterol - 10)-5.2)*(-2*(sex=='female') + 2*(sex=='male'))
# Simulate binary y to have Prob(y=1) = 1/[1+exp(-L)]
y <- ifelse(runif(n) < plogis(L), 1, 0)

DATASET <- data.frame(age, blood.pressure, cholesterol, sex)

##### SETTING THE DATA DICTIONARY
ddist <- datadist(DATASET)
ddist$limits$cholesterol[2] <- 160 ##### SETTING REFERENCE VALUE FOR CHOLESTEROL
options(datadist='ddist')

fit <- lrm(y ~ blood.pressure + sex + (age + rcs(cholesterol,4)),
           x=TRUE, y=TRUE, data=DATASET)
p <- Predict(fit, cholesterol, ref.zero=T)

plotData <- data.frame(p)
q <- ggplot(data=plotData, mapping=aes(x=cholesterol, y=exp(yhat),ymin=exp(lower),ymax=exp(upper)))   # or plot()
q <- q + geom_hline(yintercept=1, colour="red")
q <- q + geom_vline(xintercept=160, colour="red")
q <- q + geom_ribbon(alpha=0.3)
q <- q + geom_line()
q <- q + scale_x_continuous("Cholesterol")
q <- q + scale_y_continuous("Odds ratio")
q


