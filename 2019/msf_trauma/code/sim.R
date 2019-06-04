
sim <- function(type,num_people,sd_baseline=1,sd_effect=1,master){
  
  if(type=="LL"){
    d <- master[injurysite=="lower"]
  } else if(type=="UL"){
    d <- master[injurysite=="upper"]
  } 
  
  perc_early <- as.numeric(d[exposure=="early physio" & variable=="percentage of patients"]$`Afghanistan - Kunduz -2015`)
  perc_late <- as.numeric(d[exposure=="late physio" & variable=="percentage of patients"]$`Afghanistan - Kunduz -2015`)
  
  baseline_early <- as.numeric(d[exposure=="early physio" & variable==glue::glue("baseline {type} subscore")]$`Afghanistan - Kunduz -2015`)
  baseline_late <- as.numeric(d[exposure=="late physio" & variable==glue::glue("baseline {type} subscore")]$`Afghanistan - Kunduz -2015`)
  
  final_early <- as.numeric(d[exposure=="early physio" & variable==glue::glue("final {type} subscore")]$`Afghanistan - Kunduz -2015`)
  final_late <- as.numeric(d[exposure=="late physio" & variable==glue::glue("final {type} subscore")]$`Afghanistan - Kunduz -2015`)
  
  
  effect_early <- final_early-baseline_early
  effect_late <- final_late-baseline_late
  
  fake_early <- data.table(is_early=rep(TRUE,round(num_people*perc_early/100)))
  fake_late <- data.table(is_early=rep(FALSE,round(num_people*perc_late/100)))
  
  fake_early[,id:=glue::glue("a_{num}",num=formatC(1:.N,width=3,flag="0"))]
  fake_late[,id:=glue::glue("b_{num}",num=formatC(1:.N,width=3,flag="0"))]
  
  fake_early[,baseline:=baseline_early+rnorm(.N,sd=sd_baseline)]
  fake_early[,final:=baseline+effect_early+rnorm(.N,sd=sd_effect)]
  
  fake_late[,baseline:=baseline_late+rnorm(.N,sd=sd_baseline)]
  fake_late[,final:=baseline+effect_late+rnorm(.N,sd=sd_effect)]
  
  fake_w <- rbind(fake_early, fake_late)
  
  fit <- lm(final~baseline+is_early, data=fake_w)
  effect_difference <- as.numeric(coef(fit)["is_earlyTRUE"])
  
  retval <- data.frame(
    type=type,
    num_people=num_people,
    num_early=round(num_people*perc_early/100),
    num_late=round(num_people*perc_late/100),
    sd_baseline=sd_baseline,
    sd_effect=sd_effect,
    effect_early=effect_early,
    effect_late=effect_late,
    effect_difference=effect_difference,
    pval_difference=coef(summary(fit))["is_earlyTRUE","Pr(>|t|)"]
  )
  
  return(retval)
}

sim_data <- function(num_people){
  
  people <- data.table(person_id=1:num_people)
  people[, time_since_injury := 30 - rpois(.N, 20)]
  people[time_since_injury < 0, time_since_injury := 0]
  
  people[,outcome_baseline := 22 + rnorm(n=.N, mean=0, sd=8.5)]
  people[,outcome_end := outcome_baseline+15-0.25*time_since_injury+rnorm(n=.N, mean=0, sd=8.5)]
  
  people[outcome_baseline<0,outcome_baseline:=0]
  people[outcome_baseline>50,outcome_baseline:=50]
  
  people[outcome_end<0,outcome_end:=0]
  people[outcome_end>50,outcome_end:=50]
  
  return(people)
}



sim <- function(num_people){
  people <- sim_data(num_people)
  
  fit <- lm(outcome_end~outcome_baseline+time_since_injury,data=people)
  effects <- data.frame(coef(summary(fit)))
  effects$var <- row.names(effects)
 
  effects$num_people <- num_people
  
  return(effects)
}





