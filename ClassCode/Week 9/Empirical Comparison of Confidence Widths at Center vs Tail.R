
####################################################################
# Empirical Comparison of Confidence Widths at Center vs Tail
####################################################################

set.seed(1)

B <- 1000
n <- 30
beta0 <- 3
beta1 <- 1.5
sigma <- 3

x <- seq(0, 10, length.out = n)
x_center <- mean(x)
x_tail <- max(x)

width_mean_center <- numeric(B)
width_mean_tail   <- numeric(B)
width_pred_center <- numeric(B)
width_pred_tail   <- numeric(B)

for (b in 1:B) {
  y <- beta0 + beta1 * x + rnorm(n, sd = sigma)
  fit <- lm(y ~ x)
  
  pm_center <- predict(fit,
                       newdata = data.frame(x = x_center),
                       interval = "confidence")
  pm_tail   <- predict(fit,
                       newdata = data.frame(x = x_tail),
                       interval = "confidence")
  
  pp_center <- predict(fit,
                       newdata = data.frame(x = x_center),
                       interval = "prediction")
  pp_tail   <- predict(fit,
                       newdata = data.frame(x = x_tail),
                       interval = "prediction")
  
  width_mean_center[b] <- pm_center[,"upr"] - pm_center[,"lwr"]
  width_mean_tail[b]   <- pm_tail[,"upr"] - pm_tail[,"lwr"]
  width_pred_center[b] <- pp_center[,"upr"] - pp_center[,"lwr"]
  width_pred_tail[b]   <- pp_tail[,"upr"] - pp_tail[,"lwr"]
}

colMeans(cbind(width_mean_center,
               width_mean_tail,
               width_pred_center,
               width_pred_tail))

