data {
  int nteams;
  int ngames;
  int team1[ngames];
  int team2[ngames];
  vector[ngames] score1;
  vector[ngames] score2;
  vector[ngames] lastWin_a;
  vector[ngames] lastWin_b;
  real df;
}
transformed data {
  vector[ngames] dif;
  dif = score1 - score2;
}
parameters {
  real homeAdvantage;
  real est_lastWin_a;
  real est_lastWin_b;
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
    dif[i] ~ student_t(df, est_lastWin_a*lastWin_a[i] + est_lastWin_b*lastWin_b[i] + homeAdvantage+a[team1[i]]-a[team2[i]], sigma_y);
}
