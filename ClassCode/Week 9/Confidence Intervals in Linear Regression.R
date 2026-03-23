
####################################################################
# Confidence Intervals in Linear Regression
####################################################################
set.seed(123)
n <- 40
x <- seq(0, 20, length.out = n)
beta0 <- 5
beta1 <- 2
sigma <- 4
y <- beta0 + beta1 * x + rnorm(n, sd = sigma )
fit <- lm(y ~ x)
summary (fit )
confint (fit )
# Two x- values : center and tail
newdat <- data.frame(x = c( mean (x), max (x)))
# CI for expected response
pred_mean <- predict(fit , newdata = newdat , interval = "confidence")
# PI for new response
pred_obs <- predict(fit , newdata = newdat , interval = "prediction")
pred_mean
pred_obs

# Leverages
h <- hatvalues(fit)
cbind(x = x, leverage = round (h, 4))
# Diagnostic measures
rstandard (fit)
cooks.distance(fit)
# Plot fitted line and intervals on a fine grid
grid <- data.frame (x = seq (min(x), max(x), length.out = 200) )
ci_grid <- predict(fit , newdata = grid , interval = "confidence")
pi_grid <- predict(fit , newdata = grid , interval = "prediction")
plot (x, y, pch = 19, main = "Fitted_line_with_CI_and_PI")
lines ( grid $x, ci_grid [, "fit"], lwd = 2)
lines ( grid $x, ci_grid [, "lwr"], lty = 2)
lines ( grid $x, ci_grid [, "upr"], lty = 2)
lines ( grid $x, pi_grid [, "lwr"], lty = 3)
lines ( grid $x, pi_grid [, "upr"], lty = 3)
legend ("topleft",
        legend = c("fit ", "mean_CI", "prediction_PI"),lty = c(1 ,2 ,3) , bty = "n")
