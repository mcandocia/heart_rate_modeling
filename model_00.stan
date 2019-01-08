data {
  int<lower=0> N;
  vector[N] heart_rate_start;
  vector[N] heart_rate_stop;
  vector[N] avg_temp;
  vector[N] rest_time;
}
parameters {
  real hr_a_const;
  real lag_const;
  real c1_const;
  real c2_const;
  
  real hr_a_temp;
  real lag_temp;
  real c1_temp;
  real c2_temp;
  
  real<lower=0> sigma2_1;
  real<lower=0> sigma2_2;
  real<lower=0> sigma2_rate;
  
}
transformed parameters {
 
}
model{
  vector[N] hr_a;
  vector[N] lag;
  vector[N] c1;
  vector[N] c2;
  vector[N] yhat;
  vector[N] sigma2;
  
  //sigma2_2 ~ normal(500, 100);
  //c1 ~ exponential(0.1);
  //c2 ~ exponential(0.1);
  sigma2_rate ~ exponential(0.01);
  
  for (n in 1:N){
    hr_a[n] = hr_a_const + avg_temp[n]*hr_a_temp;
    lag[n] = lag_const + avg_temp[n] * lag_temp;
    c1[n] = c1_const + avg_temp[n] * c1_temp;
    c2[n] = c2_const + avg_temp[n] * c2_temp;
  }
  
  for (n in 1:N){
    if (lag[n] >= rest_time[n]){
      yhat[n] = heart_rate_stop[n] + (hr_a[n] - heart_rate_stop[n]) * (1-exp(-lag[n]*c1[n]) * exp((rest_time[n]-lag[n]) * c2[n]));
      sigma2[n] = sigma2_1 * (exp(-(rest_time[n]-lag[n]) * sigma2_rate)) + sigma2_2 * (1-exp(-(rest_time[n]-lag[n]) * sigma2_rate));
    } else {
       yhat[n] = heart_rate_stop[n] + (hr_a[n] - heart_rate_stop[n]) * (1-exp(-rest_time[n]*c1[n]));
       sigma2[n] = sigma2_1;
    }
  }
  heart_rate_start ~ normal(yhat, sigma2);
}
