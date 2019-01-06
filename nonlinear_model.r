library(plyr)
library(dplyr)
library(dfoptim)

heart=read.csv('s1_heart_data.csv')

na.penalty <- function(x, ceiling=5000){
  return(ifelse(is.na(x),ceiling,x))
}

# creates a function to optimize
heart_rate_estimator_function <- function(
  data,
  variables,
  coefficients_to_exponentiate=character(0),
  polynomial_order=1 #may be implemented later
){
  # clean input
  for (variable in c('heart_rate_stop','rest_time','heart_rate_start', variables)){
    print(variable)
    data = data[!is.na(data[,variable]),]
  }
  print(colwise(function(x)sum(is.na(x)))(data))
  
  # function to return
  model_func <- function(X){
    # X - description below
    # HR_a(1), lag(1), C(1), HR_a(v1), lag(v1), C1(v1), ..., C2(vn), sigma^2
    # HR_a is the steady-state heart rate
    # lag is the lag in which process of heart rate relaxation changes
    # C1 is the coefficient of heart rate change in first phase
    # C2 is the coefficient of heart rate change in second phase
    # sigma2 is standard error in phase 1
    # sigma2_rate is coefficient of rate of change of sigma in phase 2
    # sigma2_final is final sigma for second phase of heart rate change
    # 1 means "constant" - first three variables in model are the constant
    # vn means the nth variable
    HR_a = X[1]
    lag = X[2]
    C1 = X[3]
    C2 = X[4]
    i = 1
    for (variable in variables){
      
      trans_func = identity
      if (variable %in% coefficients_to_exponentiate){
        trans_func = function(x, min_slope=.01) {
          ifelse(x>0, exp(x)*min_slope-min_slope, min_slope-exp(abs(x))*min_slope)
        }
      }
      HR_a = HR_a + trans_func(X[1+4*i]) * data[,variable]
      lag = lag + trans_func(X[2+4*i]) * data[,variable]
      C1 = C1 + trans_func(X[3+4*i]) * data[,variable]
      C2 = C2 + trans_func(X[4+4*i]) * data[,variable]
      i = i + 1
    }
    
    sigma2 = X[4*i+1]
    sigma2_rate = X[4*i+2]
    sigma2_final = X[4*i + 3]
    
    t = data$rest_time
    
    estimates = data$heart_rate_stop + (HR_a - data$heart_rate_stop) * 
      (1-ifelse(t<lag, exp(-t * C1), exp(-lag * C1) * exp(-(t-lag) * C2)))
    
    # if Inf * 0 occurs above
    estimates = ifelse(is.na(estimates), 1000, estimates)
    
    
    effective_sigma2 = sigma2 + ifelse(t < lag, 0, (sigma2_final-sigma2)*(1-pmin(0, exp(-(t-lag)*sigma2_rate))))
    if (any(is.na(log(effective_sigma2)))){
      print(summary(effective_sigma2))
    }
    #print(C2)
    #print(effective_sigma2)
    
    ll = -sum(na.penalty(log(effective_sigma2)))/2  - 
      sum(1/abs(effective_sigma2) *
         (data$heart_rate_start - estimates)^2
         )
  
    
    penalty=0
    penalty = penalty + 50 * (sum(effective_sigma2==0))
    if (sigma2 < 0 | sigma2_final < 0){
      penalty=penalty+1e6
    }
    if (sigma2_rate < 0)
      penalty=penalty+1e6
    if (sigma2 > sigma2_final)
      penalty= penalty + (sigma2-sigma2_final)^2
    
    if (any(lag < 0)){
      penalty=penalty+150*sum(exp(abs(lag)[lag<0]))+300
    }
    if (any(HR_a <= 50)){
      penalty=penalty+150*sum(pmin(1000, exp(abs(50-HR_a)[HR_a<0])))+300
    }
    if (any(C1<= 0)){
      penalty=penalty+150* sum(pmin(2000,exp(abs(C1)[C1<0])))+300
    }
    if (any(C2<= 0)){
      penalty=penalty+150*sum(pmin(2000,exp(abs(C2)[C2<0])))+300
    }
    

    
    return(-ll + penalty)
  }
  return(model_func)
}

make_estimator_function <- function(
  data,
  variables,
  coefficients_to_exponentiate=character(0),
  polynomial_order=1 #may be implemented later
){
  # clean input
  for (variable in c('heart_rate_stop','rest_time','heart_rate_start', variables)){
    print(variable)
    data = data[!is.na(data[,variable]),]
  }
  print(colwise(function(x)sum(is.na(x)))(data))
  
  # function to return
  model_func <- function(X){
    # X - description below
    # HR_a(1), lag(1), C(1), HR_a(v1), lag(v1), C1(v1), ..., C2(vn), sigma^2
    # HR_a is the steady-state heart rate
    # lag is the lag in which process of heart rate relaxation changes
    # C1 is the coefficient of heart rate change in first phase
    # C2 is the coefficient of heart rate change in second phase
    # sigma2 is standard error in phase 1
    # sigma2_rate is coefficient of rate of change of sigma in phase 2
    # sigma2_final is final sigma for second phase of heart rate change
    # 1 means "constant" - first three variables in model are the constant
    # vn means the nth variable
    HR_a = X[1]
    lag = X[2]
    C1 = X[3]
    C2 = X[4]
    i = 1
    for (variable in variables){
      
      trans_func = identity
      if (variable %in% coefficients_to_exponentiate){
        trans_func = function(x, min_slope=.01) {
          ifelse(x>0, exp(x)*min_slope-min_slope, min_slope-exp(abs(x))*min_slope)
        }
      }
      HR_a = HR_a + trans_func(X[1+4*i]) * data[,variable]
      lag = lag + trans_func(X[2+4*i]) * data[,variable]
      C1 = C1 + trans_func(X[3+4*i]) * data[,variable]
      C2 = C2 + trans_func(X[4+4*i]) * data[,variable]
      i = i + 1
    }
    
    sigma2 = X[4*i+1]
    sigma2_rate = X[4*i+2]
    sigma2_final = X[4*i + 3]
    
    t = data$rest_time
    
    estimates = data$heart_rate_stop + (HR_a - data$heart_rate_stop) * 
      (1-ifelse(t<lag, exp(-t * C1), exp(-lag * C1) * exp(-(t-lag) * C2)))
    
    # if Inf * 0 occurs above
    estimates = ifelse(is.na(estimates), 1000, estimates)
    
    
    effective_sigma2 = sigma2 + ifelse(t < lag, 0, (sigma2_final-sigma2)*(1-pmin(0, exp(-(t-lag)*sigma2_rate))))
    if (any(is.na(log(effective_sigma2)))){
      print(summary(effective_sigma2))
    }
    #print(C2)
    #print(effective_sigma2)
    
    ll = -sum(na.penalty(log(effective_sigma2)))/2  - 
      sum(1/abs(effective_sigma2) *
            (data$heart_rate_start - estimates)^2
      )
    
    return(estimates)
  }
  data_filtered <<- data
  return(model_func)
}

heart = heart %>% mutate(avg_temp_from_room_squared = (avg_temp-22)^2)

hr_func = heart_rate_estimator_function(
  heart %>% filter(distance_start> 0.8),
  #coefficients_to_exponentiate=c('avg_temp', 'speed_past_60_seconds'),
  variables=c('avg_temp', 'speed_past_60_seconds')#, 'avg_temp_from_room_squared')
)

estimator_func = make_estimator_function(
  heart %>% filter(distance_start> 0.8),
  #coefficients_to_exponentiate=c('avg_temp', 'speed_past_60_seconds'),
  variables=c('avg_temp','speed_past_60_seconds')#, 'avg_temp_from_room_squared')
)

# steady_temp, lag, rate
initial_params = c(
  80,5,0.1,0.05, # constant
  0.1,0.1,0.1,0.1, # avg_temp
  0.1,0.1,0.10,0, # speed_past_60_seconds
  #0.001,0.001,0.001, # avg_temp_from_room_squared
  400, # sigma2
  0.2, # sigma2_rate
  600 # sigma2_final
)

initial_parameter_modifications<- function(n=14){
  modifications = c(
    rnorm(4)*30,
    rnorm(8)*10*(rnorm(8)< -0.1) + rlnorm(8,1,1.5),
    rnorm(1)*5 + rlnorm(1, 1, 1) * (1-2*(rnorm(1) < 0)),
    rnorm(1)*0.05,
    rnorm(1)*5 + rlnorm(1, 1, 1.1) * (1-2*(rnorm(1) < 0))
  )
  if (rnorm(1)>-1)
   return(modifications)
  else{
    return(modifications/1000)
  }
}

if (TRUE){
  best_optims = list()
  best_optims_counter = 1
  best_value = Inf
  best_method=NULL
  for (i in 1:5000){
    print(i)
    ipars = initial_params+initial_parameter_modifications()/4.5
    if (i %% 2==0.1)
      new_optimal_params = optim(
        ipars,
        hr_func,
        method='SANN',
        control=list(
          maxit=120000,
          ndeps=2e-4,
          tmax=100,
          temp=30
        )
      )
    else if (i %% 2==1)
      new_optimal_params = hjk(ipars, hr_func, control=list(maxfeval=5e6))
    else if (i %% 2==2)
      new_optimal_params = nmk(ipars, hr_func)
    if (new_optimal_params$value < best_value){
      
      best_method = c('optim','hjk')[(i %% 2)+1]
      print(best_method)
      optimal_params=new_optimal_params
      print(optimal_params)
      best_value = optimal_params$value
      
      # for output
      predictions = estimator_func(optimal_params$par)
      
      residuals = (data_filtered$heart_rate_start-predictions)
      
      (r2 = 1-var(residuals)/var(data_filtered$heart_rate_start))
      print(r2)
      best_optims[[best_optims_counter]] = list(method=best_method, ipars=ipars, params=new_optimal_params, r2=r2)
      best_optims_counter = best_optims_counter+1
    }
  }
}




predictions = estimator_func(optimal_params$par)

residuals = (data_filtered$heart_rate_start-predictions)

(r2 = 1-var(residuals)/var(data_filtered$heart_rate_start))


## HR_f = HR_0 + HR_a * (1-exp(-max(t-lag,0) * rate))
# HR_a = -6.63 + 0.12*avg_temp - 4.496 * v
# lag = 6.7 - .14 * avg_temp - 0.28 * v
# rate = 0.045 - 0.0002 * avg_temp - 0.001 * v

## HR_f = HR_0 + (HR_a - HR_0) * (exp(-max(t-lag,0) * rate))
# HR_a = 87 + 0.1*T - .05 * s
# lag = -2.45 -0.59 * T + 0.61 * s
# rate = 0.0336 - 6e-4 * T -5.75e-4 * s


# HR_a = 80 + 0.71 * T - 0.054 * s
# lag = 1.47 - 0.029 * T - 0.03469 * s
# rate = 0.0327 + 7.77e-6 * T - 1.13e-3 * s
# sigma2 = 258.5

## HR_f
