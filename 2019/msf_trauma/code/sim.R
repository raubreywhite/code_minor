
sim_data <- function(num_people){
  
  people <- data.table(person_id=1:num_people)
  
  people[,`exposure_0.50`:=sample(c(0,1),.N,replace=T)]
  people[,`exposure_1.00`:=sample(c(0,1),.N,replace=T)]
  people[,`exposure_1.50`:=sample(c(0,1),.N,replace=T)]
  people[,`exposure_2.00`:=sample(c(0,1),.N,replace=T)]
  people[,`exposure_2.50`:=sample(c(0,1),.N,replace=T)]
  people[,`exposure_3.00`:=sample(c(0,1),.N,replace=T)]
  
  people[,outcome_baseline := 22 + rnorm(n=.N, mean=0, sd=8.5)]
  people[,outcome_end := 
           15 + 
           outcome_baseline +
           0.50 * `exposure_0.50` +
           1.00 * `exposure_1.00` +
           1.50 * `exposure_1.50` +
           2.00 * `exposure_2.00` +
           2.50 * `exposure_2.50` +
           3.00 * `exposure_3.00` +
           rnorm(n=.N, mean=0, sd=8.5)]
  
  people[outcome_baseline<0,outcome_baseline:=0]
  people[outcome_baseline>55,outcome_baseline:=55]
  
  #people[outcome_end<0,outcome_end:=0]
  #people[outcome_end>55,outcome_end:=55]
  
  return(people)
}



sim <- function(num_people){
  people <- sim_data(num_people)
  
  fit <- lm(outcome_end ~ 
              outcome_baseline + 
              `exposure_0.50` +
              `exposure_1.00` +
              `exposure_1.50` +
              `exposure_2.00` +
              `exposure_2.50` +
              `exposure_3.00`
            ,data=people)
  effects <- data.frame(coef(summary(fit)))
  effects$var <- row.names(effects)
 
  effects$num_people <- num_people
  
  return(effects)
}





