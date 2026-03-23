
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

