df <- with(mtcars, data.frame(y=mpg, x1=disp, x2=hp, x3=wt))
df

nll_lm <- function(data, par) {
  y <- data$y
  X <- as.matrix(cbind(1, data[, -1]))
  beta <- par[1:(ncol(X))] 
  sigma <- par[length(par)] 
  residuals <- y - X %*% beta
  llik_first <- dnorm(residuals[1], mean = 0, sd = sigma, log = TRUE)  
  llik_rest <- sum(dnorm(residuals[-1], mean = 0, sd = sigma, log = TRUE)) 
  full_llik <- llik_first + llik_rest
  return(-full_llik)
}
inits <- c(mean(df$y), rep(0, ncol(df) - 1), 1) 

fit <- optim(inits, nll_lm, data = df, method = "L-BFGS-B",             
             lower = c(rep(-Inf, ncol(df)), 1e-3), 
             upper = c(rep(Inf, ncol(df)), Inf)   
)

beta_estimates <- fit$par[1:(ncol(df))] 
sigma_estimate <- fit$par[length(fit$par)] 
beta_estimates
sigma_estimate

