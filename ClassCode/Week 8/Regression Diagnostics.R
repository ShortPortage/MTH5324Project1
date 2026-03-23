
#########################################################
# X and Z are highly correlated
# The interaction term is not statistically meaningful
# The true data-generating process is additive
#########################################################

set.seed (123)
n <- 1000
X <- rnorm (n)
Z <- X + rnorm (n, sd = 0.3)
Y <- 1 + 2*X + 3*Z + rnorm (n)
cor (X, Z)
model_no_int <- lm(Y ~ X + Z)
model_int <- lm(Y ~ X * Z)
summary ( model_no_int )
summary ( model_int )

#########################################################
# X and Z are nearly uncorrelated
# The interaction term is large and significant
# Correlation is neither necessary nor sufficient for interaction
#########################################################

set.seed (456)
X <- rnorm (n)
Z <- rnorm (n)
Y <- 1 + 2*X + 3*Z + 4*X*Z + rnorm (n)
cor (X, Z)
model_no_int <- lm(Y ~ X + Z)
model_int <- lm(Y ~ X * Z)
summary ( model_no_int )
summary ( model_int )


# m2 <- lm(mpg ~ hp + wt, data = mtcars)
# 
# library(gtsummary)
# tbl_regression(m2)
# 
# tbl_regression(m2,
#                intercept = TRUE,
#                conf.level = 0.99,
#                label = list(hp ~ "Gross horsepower", wt ~ "Weight (1000 lbs)"))


#####################################################
# Fit two models:
#####################################################

mtcars$cyl <- factor(mtcars$cyl)
m1 <- lm(mpg ~ hp + wt, data = mtcars) # Simple model
m2 <- lm(mpg ~ hp*wt + cyl, data = mtcars) # Complex model

#####################################################
# Create data frames with fitted values:
#####################################################

m1_pred <- data.frame(hp = mtcars$hp, mpg_pred = predict(m1))
m2_pred <- data.frame(hp = mtcars$hp, mpg_pred = predict(m2))

#####################################################
# Plot fitted values:
#####################################################

install.packages(ggplot2)
library(ggplot2)
ggplot(mtcars, aes(hp, mpg)) +
  geom_point() +
  geom_line(data = m1_pred, aes(x = hp, y = mpg_pred),
            colour = "red") +
  geom_line(data = m2_pred, aes(x = hp, y = mpg_pred),
            colour = "blue")


#####################################################
# Plot the observed values against the fitted values
#####################################################

n <- nrow(mtcars)
models <- data.frame(Observed = rep(mtcars$mpg, 2),
                     Fitted = c(predict(m1), predict(m2)),
                     Model = rep(c("Model 1", "Model 2"), c(n, n)))

ggplot(models, aes(Fitted, Observed)) +
  geom_point(colour = "blue") +
  facet_wrap(~ Model, nrow = 3) +
  geom_abline(intercept = 0, slope = 1) +
  xlab("Fitted values") + ylab("Observed values")

#####################################################
# Create diagnostic plots
#####################################################

install.packages("ggfortify")
library(ggfortify)
autoplot(m1, which = 1:6, ncol = 2, label.size = 3)

#####################################################
# Breusch-Pagan test for heteroscedasticity
#####################################################

install.packages("car")
library(car)
ncvTest(m1)

#####################################################
# To check for multicollinearity
#####################################################

install.packages("GGally")
library(GGally)
ggpairs(mtcars[, -1])

#####################################################
# To check for multicollinearity
#####################################################

#library(car)
m <- lm(mpg ~ ., data = mtcars)
vif(m)



#####################################################
# Excersize for Regression Diagnostics 
#####################################################
exdata1 <- data.frame(
  x = c(2.99, 5.01, 8.84, 6.18, 8.57, 8.23, 8.48, 0.04, 6.80,
        7.62, 7.94, 6.30, 4.21, 3.61, 7.08, 3.50, 9.05, 1.06,
        0.65, 8.66, 0.08, 1.48, 2.96, 2.54, 4.45),
  y = c(5.25, -0.80, 4.38, -0.75, 9.93, 13.79, 19.75, 24.65,
        6.84, 11.95, 12.24, 7.97, -1.20, -1.76, 10.36, 1.17,
        15.41, 15.83, 18.78, 12.75, 24.17, 12.49, 4.58, 6.76,
        -2.92))
exdata2 <- data.frame(
  x = c(5.70, 8.03, 8.86, 0.82, 1.23, 2.96, 0.13, 8.53, 8.18,
        6.88, 4.02, 9.11, 0.19, 6.91, 0.34, 4.19, 0.25, 9.72,
        9.83, 6.77, 4.40, 4.70, 6.03, 5.87, 7.49),
  y = c(21.66, 26.23, 19.82, 2.46, 2.83, 8.86, 0.25, 16.08,
        17.67, 24.86, 8.19, 28.45, 0.52, 19.88, 0.71, 12.19,
        0.64, 25.29, 26.72, 18.06, 10.70, 8.27, 15.49, 15.58,
        19.17))



