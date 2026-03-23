

#####################################################
# Variable Selection 
#####################################################

# # ols_step_best_subset(model, ...)
# 
# # Default S3 method
# ols_step_best_subset(
#   model,
#   max_order = NULL,
#   include = NULL,
#   exclude = NULL,
#   metric = c("rsquare", "adjr", "predrsq", "cp", "aic", "sbic", "sbc", "msep", "fpe",
#              "apc", "hsp"),
#   ...
# )
# 
# # S3 method for class 'ols_step_best_subset'
# plot(x, model = NA, print_plot = TRUE, ...)

#####################################################
#####################################################

install.packages("olsrr")
library(olsrr)

model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
ols_step_best_subset(model)
ols_step_best_subset(model, metric = "adjr")
ols_step_best_subset(model, metric = "cp")
ols_step_best_subset(model, metric = "sbic")

# maximum subset
model <- lm(mpg ~ disp + hp + drat + wt + qsec, data = mtcars)
ols_step_best_subset(model, max_order = 3)

# plot
model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
k <- ols_step_best_subset(model)
plot(k)



# return only models including `qsec`
ols_step_best_subset(model, include = c("qsec"))

# exclude `hp` from selection process
ols_step_best_subset(model, exclude = c("hp"))

#####################################################
#####################################################

model <- lm(mpg ~ ., data = mtcars)
ols_step_best_subset(model)
ols_step_best_subset(model, metric = "adjr")
ols_step_best_subset(model, metric = "cp")
ols_step_best_subset(model, metric = "hsp")

#####################################################
# Stepwise Variable Selection
#####################################################

install.packages("MASS")
# Load the MASS package
library(MASS)

# Use the built-in mtcars dataset
data(mtcars)

# Define the full model with all potential predictors
full_model <- lm(mpg ~ ., data = mtcars)
summary(full_model)

# Define the null model (intercept only)
null_model <- lm(mpg ~ 1, data = mtcars)

# Perform stepwise regression using both forward and backward steps
# The scope defines the range of models to consider
stepwise_model <- stepAIC(null_model, 
                          direction = "both", 
                          scope = list(lower = null_model, upper = full_model),
                          trace = FALSE) # Set trace=FALSE to suppress detailed output for each step

# View the summary of the final selected model
summary(stepwise_model)

# View the coefficients of the final model
coef(stepwise_model)

# View the final model's AIC
AIC(stepwise_model)


step_model <- step(null_model, 
                          direction = "both", 
                          scope = list(lower = null_model, upper = full_model),
                          trace = FALSE) # Set trace=FALSE to suppress detailed output for each step

summary(step_model)
AIC(step_model)


#####################################################
# Stepwise Variable Selection Example 2
#####################################################

# Remove the 'car model' column as it's not a predictor variable
mtcars_clean <- within(mtcars, rm(vs, am, gear, carb)) 
# Using a subset for a clearer example

# Optional: Handle missing values if your real dataset has them
# mtcars_clean <- na.omit(mtcars_clean)

# Define the full model with all potential predictors
full_model <- lm(mpg ~ ., data = mtcars_clean)
summary(full_model)

# Define the null model (intercept only)
null_model <- lm(mpg ~ 1, data = mtcars_clean)

# Perform stepwise regression using both forward and backward steps
# The scope defines the range of models to consider
stepwise_model <- stepAIC(null_model, 
                          direction = "both", 
                          scope = list(lower = null_model, upper = full_model),
                          trace = FALSE) # Set trace=FALSE to suppress detailed output for each step

# View the summary of the final selected model
summary(stepwise_model)

# View the coefficients of the final model
coef(stepwise_model)

# View the final model's AIC
AIC(stepwise_model)





