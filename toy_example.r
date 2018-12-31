n = 1000

sample_data = data.frame(
  t = pmax(3, 4+rnorm(n)*3),
  x1 = rnorm(n),
  x2=rbinom(n, 50, 0.5)
) 

B1 = 100
B2 = 0.5
B3 = 5
B4 = 0.1
B5 = 0.1
B6 = 0.01

sample_data$y = with(sample_data, x1 + (B1-x1+B2*x2) * (exp(pmax(t-(B3+B4*x2), 0) * (B5 + B6*x2))))# + rnorm(n)


make_ll_function <- function(
  data
){

  # function to return
  model_func <- function(X){
    B1 = X[1]
    B2 = X[2]
    B3 = X[3]
    B4 = X[4]
    B5 = X[5]
    B6 = X[6]
    sigma2 = X[7]
    
    estimates = with(
      data,
      x1 + (B1-x1+B2*x2) * (exp(pmax(t-(B3+B4*x2), 0) * (B5 + B6*x2)))
    ) 
    
    ll = -nrow(data)/2 * log(sigma2)  - 
      1/sigma2*sum(
        (data$y - estimates)^2
      )
    # invert for maximization
    if (any(X < 0))
      penalty=1e5
    else
      penalty=0
    return(-ll+penalty)
  }
  return(model_func)
}

make_sample_estimator_function <- function(
  data
){
  
  # function to return
  model_func <- function(X){
    B1 = 100#X[1]
    B2 = X[2]
    B3 = X[3]
    B4 = X[4]
    B5 = X[5]
    B6 = X[6]
    sigma2 = X[7]
    
    estimates = with(
      data,
      x1 + (B1-x1+B2*x2) * (exp(pmax(t-(B3+B4*x2), 0) * (B5 + B6*x2)))
    ) 
    
    ll = -nrow(data)/2 * log(sigma2)  - 
      1/sigma2*sum(
        (data$y - estimates)^2
      )
    # invert for maximization
    return(estimates)
  }
  return(model_func)
}

initial_values = c(
  50,
  1,
  1,
  1,
  1,
  0.1,
  1000
)

model_func = make_ll_function(sample_data)
sample_estimate_func = make_sample_estimator_function(sample_data)

(model_params = optim(initial_values, model_func,
                      method='SANN',
                      control=list(
                        maxit=100000,
                        ndeps=1e-4,
                        tmax=100,
                        temp=5
                      )))

(sample_r2=1-var(sample_data$y-sample_estimate_func(model_params$par))/var(sample_data$y))

"
Removing the error term from the model, if I run a simple linear regression 

`(summary(lm(y~t+x1+x2, data=sample_data))`

 on this model, I get an R^2 about about 0.4. Running a random forest model (with default parameters) gets an R^2 of about 0.85.
"