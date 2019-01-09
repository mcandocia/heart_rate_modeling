library(rstan)
library(dplyr)

heart = read.csv('s1_heart_data.csv') %>%
  filter(!is.na(heart_rate_start) & !is.na(heart_rate_stop) & !is.na(avg_temp))

heart_data = list(N=nrow(heart))
for (name in names(heart))
  heart_data[[name]] = heart[,name]

options(mc.cores = 6);

init_00 = list(
  hr_a_const=80,
  lag_const=1,
  c1_const=0.01,
  c2_const=0.005,
  
  hr_a_temp=0.1,
  lag_temp=0.1,
  c1_temp=0.01,
  c2_temp=0.01,
  sigma2_1 = 400,
  sigma2_2 = 600, 
  sigma_rate=0.5
)

init_01 = list(
  hr_a_const=80,
  hr_a_temp=1,
  c1_const=0.01,
  c1_temp=-0.001,
  sigma2_1=400
)

fit <- stan(file='model_01.stan', data=heart_data,
            init= function(chain_id) init_01,
            verbose=TRUE,
            chains=1,
            diagnostic_file='model_00_diagnostic.txt')


predict.01 <- function(data){
  data$heart_rate_stop + 
    (75.92-data$heart_rate_stop + 0.882*data$avg_temp) * ( 1-
    exp(-(0.018283 + 1.922e-4*data$avg_temp)*data$rest_time)
  )
}

preds = predict.01(heart)
residuals = preds - heart$heart_rate_start
var(residuals)