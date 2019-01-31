data(kidney)

survminer::surv_summary(survfit(Surv(kidney$time,kidney$status)~1))
kfit <- coxph(Surv(time, status)~ 1,
              kidney
)
survminer::surv_summary(survfit(kfit))

kfit <- coxph(Surv(time, status)~ 1,
              kidney
)
H<-basehaz(kfit,centered=FALSE)
exp(-H$hazard)

kfit <- coxph(Surv(time, status)~ age + sex + disease + frailty(id),
              kidney
)
H<-basehaz(kfit,centered=FALSE)

# three new data points
temp <- kidney[1:3,c("age","sex","disease")]
# model.matrix without intercept
Mtemp <- model.matrix(~age+sex+disease,temp)[,-1]
# fitted log hazard ratio
logHR <- Mtemp%*%coef(kfit)
# turn it into a vector, not matrix
logHR <- drop(logHR)
# Hazard ratio
HR <- exp(logHR)

# survival curves with frailty=1
frail1 <- exp( -outer(H$hazard,HR))
# survival curves with frailty=2
frail2 <- exp( -outer(H$hazard,HR)*2)
# survival curves with frailty=0.5
frail.5 <- exp( -outer(H$hazard,HR)*0.5)






data(kidney)
kfit <- coxph(Surv(time, status)~ age + sex + disease + frailty(id),
              kidney
)
H<-basehaz(kfit,centered=FALSE)

# three new data points
temp <- kidney[1:3,c("age","sex","disease")]
# model.matrix without intercept
Mtemp <- model.matrix(~age+sex+disease,temp)[,-1]
# fitted log hazard ratio
logHR <- Mtemp%*%coef(kfit)
# turn it into a vector, not matrix
logHR <- drop(logHR)
# Hazard ratio
HR <- exp(logHR)

# survival curves with frailty=1
frail1 <- exp( -outer(H$hazard,HR))
# survival curves with frailty=2
frail2 <- exp( -outer(H$hazard,HR)*2)
# survival curves with frailty=0.5
frail.5 <- exp( -outer(H$hazard,HR)*0.5)
