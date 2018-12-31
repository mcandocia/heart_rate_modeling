
# creates a function to optimize
heart_rate_estimator_function <- function(
  data,
  variables,
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
    # HR_a(1), lag(1), C(1), HR_a(v1), lag(v1), C(v1), ..., C(vn), sigma^2
    # HR_a is the steady-state heart rate
    # lag is the lag in how the heart rate will change
    # C is the coefficient of heart rate change
    # 1 means "constant" - first three variables in model are the constant
    # vn means the nth variable
    HR_a = X[1]
    lag = X[2]
    C = X[3]
    i = 1
    for (variable in variables){
      
      HR_a = HR_a + X[1+3*i] * data[,variable]
      lag = lag + X[2+3*i] * data[,variable]
      C = C + X[3+3*i] * data[,variable]
      i = i + 1
    }
    
    sigma2 = X[3*i+1]
    
    estimates = data$heart_rate_stop + HR_a - HR_a * exp(-pmax(data$rest_time - lag, 0) * C)
    
    ll = -nrow(data)/2 * log(sigma2)  - 
      1/sigma2*sum(
         (data$heart_rate_start - estimates)^2
    )
    
    penalty=0
    if (sigma2 < 0){
      penalty=penalty+1e6
    }
    if (any(lag < 0)){
      penalty=penalty+1e6
    }
    if (any(HR_a <= 50)){
      penalty=penalty+1e6
    }
    if (any(C<= 0)){
      penalty=penalty+1e6
    }
    
    return(-ll)
  }
  return(model_func)
}

make_estimator_function <- function(
  data,
  variables,
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
    # HR_a(1), lag(1), C(1), HR_a(v1), lag(v1), C(v1), ..., C(vn), sigma^2
    # HR_a is the steady-state heart rate
    # lag is the lag in how the heart rate will change
    # C is the coefficient of heart rate change
    # 1 means "constant" - first three variables in model are the constant
    # vn means the nth variable
    HR_a = X[1]#;HR_a=45
    lag = X[2]
    C = X[3]
    i = 1
    for (variable in variables){
      
      HR_a = HR_a + X[1+3*i] * data[,variable]
      lag = lag + X[2+3*i] * data[,variable]
      C = C + X[3+3*i] * data[,variable]
      i = i + 1
    }
    
    sigma2 = X[3*i+1]
    
    estimates = data$heart_rate_stop + HR_a - HR_a * exp(-pmax(data$rest_time - lag, 0) * C)
    
    ll = -nrow(data)/2 * log(sigma2)  - 
      1/sigma2*sum(
        (data$heart_rate_start - estimates)^2
      )
    return(estimates)
  }
  data_filtered <<- data
  return(model_func)
}

hr_func = heart_rate_estimator_function(
  heart,
  variables=c('avg_temp', 'speed_past_60_seconds')
)

estimator_func = make_estimator_function(
  heart,
  variables=c('avg_temp','speed_past_60_seconds')
)

initial_params = c(
  155,5,0.1, # constant
  0,0,0, # avg_temp
  0,0,0, # speed_past_60_seconds
  400 # sigma2
)

if (TRUE){
  best_value = Inf
  for (i in 1:500){
    print(i)
    (
    
    new_optimal_params = optim(
      initial_params,
      hr_func,
      method='SANN',
      control=list(
        maxit=100000,
        ndeps=2e-4,
        tmax=100,
        temp=30
      )
    ))
    if (new_optimal_params$value < best_value){
      optimal_params=new_optimal_params
      print(optimal_params)
      best_value = optimal_params$value
      
      # for output
      predictions = estimator_func(optimal_params$par)
      
      residuals = (data_filtered$heart_rate_start-predictions)
      
      (r2 = 1-var(residuals)/var(data_filtered$heart_rate_start))
      print(r2)
    }
  }
}
else {
  (
    optimal_params = optim(
      initial_params,
      hr_func,
      method='SANN',
      control=list(
        maxit=100000,
        ndeps=1e-4,
        tmax=100,
        temp=40
      )
    )
  )
}



predictions = estimator_func(optimal_params$par)

residuals = (data_filtered$heart_rate_start-predictions)

(r2 = 1-var(residuals)/var(data_filtered$heart_rate_start))


# nls function
if (FALSE){
  model = nls(
    heart_rate_start ~ heart_rate_stop + (1+avg_temp+speed_past_60_seconds) * exp(max(0, rest_time-(1+avg_temp + speed_past_60_seconds))*(1+avg_temp + speed_past_60_seconds)),
    data=data_filtered,
    start=list(heart_rate_stop=1, avg_temp=0, speed_past_60_seconds=0, rest_time=1)
  )
  
}
