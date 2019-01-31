g <- function(){
  print(1)
  print(Sys.time())
  form <- "survival::Surv(Time_to_first_pregnancy, First_pregnancy_all) ~ 
  group + 
  as.factor(birth_periods) +
  as.factor(Country_of_birth) +
  Education"
  if(isMixed) form <- sprintf("%s + survival::frailty.gaussian(lopnr_fall)",form)
  f1 <- Sys.time()
  fit <- survival::coxph(as.formula(form),data = d[!is.na(First_pregnancy_all) & !is.na(Time_to_first_pregnancy)][1:150000],ties="breslow",y=F)
  f2 <- Sys.time()
  f2-f1 #145
  print(fit)
  tab <- CoxToDF(fit)
  
                                     
}