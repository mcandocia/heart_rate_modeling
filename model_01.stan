data {
  int<lower=0> N;
  vector[N] heart_rate_start;
  vector[N] heart_rate_stop;
  vector[N] avg_temp;
  vector[N] rest_time;
}

parameters {
  real hr_a_const;
  real c1_const;
  
  real hr_a_temp;
  real c1_temp;
  
  real<lower=0> sigma2_1;
}

model{
  vector[N] hr_a;
  vector[N] c1;
  vector[N] yhat;
  vector[N] sigma2;
  //c1 ~ 
  
  //sigma2_2 ~ normal(500, 100);
  //c1 ~ exponential(0.1);
  //c2 ~ exponential(0.1);
  //sigma2_rate ~ exponential(0.01);
  
  for (n in 1:N){
    hr_a[n] = hr_a_const + avg_temp[n]*hr_a_temp;
    c1[n] = c1_const + avg_temp[n] * c1_temp;
  }
  
  for (n in 1:N){
      yhat[n] = heart_rate_stop[n] + (hr_a[n] - heart_rate_stop[n]) * (1-exp(-rest_time[n]*c1[n]));
      sigma2[n] = sigma2_1;
  }
  heart_rate_start ~ normal(yhat, sigma2);
}
