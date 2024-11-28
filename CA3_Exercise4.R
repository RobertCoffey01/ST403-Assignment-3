df <- data.frame(y = mtcars$mpg, x1 = mtcars$disp, x2 = mtcars$hp, x3 = mtcars$wt)
df
lm_fit <- lm(y ~ x1 + x2 + x3, data = df)

beta_lm <- coef(lm_fit)
sigma_lm <- summary(lm_fit)$sigma

beta_lm
sigma_lm 