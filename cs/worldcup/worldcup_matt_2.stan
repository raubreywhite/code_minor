data {
  int nteams;
  int ngames;
  vector[nteams] prior_score;
  int team1[ngames];
  int team2[ngames];
  vector[ngames] score1;
  vector[ngames] score2;
}
transformed data {
  vector[ngames] dif;
  vector[ngames] sqrt_dif;
  dif <- score1 - score2;
  for (i in 1:ngames)
    sqrt_dif[i] <- (step(dif[i]) - .5)*sqrt(fabs(dif[i]));
}
parameters {
  real b;
  real<lower=0> sigma_a;
  real<lower=0> sigma_y;
  vector[nteams] eta_a;
  real<lower=1> df;
}
transformed parameters {
  vector[nteams] a;
  a <- b*prior_score + sigma_a*eta_a;
}  
model {
  df ~ gamma(2,0.1);
  eta_a ~ normal(0,1);
  for (i in 1:ngames)
    sqrt_dif[i] ~ student_t(df, a[team1[i]]-a[team2[i]], sigma_y);
}