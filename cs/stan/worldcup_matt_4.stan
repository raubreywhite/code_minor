data {
  int nteams;
  int ngames;
  int team1[ngames];
  int team2[ngames];
  vector[ngames] score1;
  vector[ngames] score2;
  vector[ngames] lastWin_a;
  vector[ngames] lastWin_b;
  vector[ngames] firstObs_a;
  vector[ngames] firstObs_b;
  real df;
}
transformed data {
  vector[ngames] dif;
  dif = score1 - score2;
}
parameters {
  real homeAdvantage;
  real sigma_a;
  real sigma_y;
  vector[nteams] eta_a;
}
transformed parameters {
  vector[nteams] a;
  a = sigma_a*eta_a;
}  
model {
  eta_a ~ normal(0,1);
  for (i in 1:ngames)
    dif[i] ~ student_t(df, homeAdvantage+a[team1[i]]-a[team2[i]], sigma_y);
}
