

library(rrr)
library(olsrr)

set.seed(100)
#p <- 0.5
x <- seq(-7,12,.1)
length(x)

###################################################
#True Underlying model - In real application, we do not know this
###################################################
#y <- 50 + 0.5*(x-10)^5 + 2*(x-5)^3 - 10*(x-2)
y <- .1*(x-2)*(x+5)^1*(x-8)^2
plot(x,y)

###################################################
#Observation Model (noisy observations) - 
#This is what we have in a real application
###################################################
#This simulates epsilon
###################################################

noise <- rnorm(length(x), mean=0, sd=100) #50, 100, 200
noisy.y <- y + noise
points(x,noisy.y,col="blue")
plot(x,noisy.y,col="blue")
#save(x,noisy.y,file="polyreg")

###################################################
###################################################

model0 <- lm(noisy.y ~ x) #poly(x,1)
plot(x,noisy.y,col='blue',xlab='x',main='Observed data')
lines(x,y,col='red',lwd=3)
points(x,fitted(model0),col='green')
plot(fitted(model0),stats::residuals(model0))
plot(model0$fitted,model0$residuals)

summary(model0)
anova(model0)

###################################################
model1 <- lm(noisy.y ~ poly(x,1)) #poly(x,1)
plot(x,noisy.y,col='blue',xlab='x',main='Observed data')
lines(x,y,col='red',lwd=3)
points(x,fitted(model1),col='green')
plot(fitted(model1),stats::residuals(model1))
plot(model1$fitted,model1$residuals)

summary(model1)
anova(model1)

###################################################
model21 <- lm(noisy.y ~ x + I(x^2)) #poly(x,1)
plot(x,noisy.y,col='blue',xlab='x',main='Observed data')
lines(x,y,col='red',lwd=3)
points(x,fitted(model21),col='green')
plot(fitted(model21),stats::residuals(model21))
plot(model21$fitted,model21$residuals)

summary(model21)
anova(model21)

###################################################
###################################################
model2 <- lm(noisy.y ~ poly(x,2)) #poly(x,1)
plot(x,noisy.y,col='blue',xlab='x',main='Observed data')
lines(x,y,col='red',lwd=3)
points(x,fitted(model2))
plot(fitted(model2),stats::residuals(model2))
plot(model2$fitted,model2$residuals)

summary(model2)
anova(model2)



###################################################

###################################################
model3 <- lm(noisy.y ~ poly(x,3)) #poly(x,1)
plot(x,noisy.y,col='blue',xlab='x',main='Observed data')
lines(x,y,col='red',lwd=3)
points(x,fitted(model3))
plot(fitted(model3),stats::residuals(model3))
plot(model3$fitted,model3$residuals)

summary(model3)
anova(model3)

###################################################
model4 <- lm(noisy.y ~ poly(x,4)) #poly(x,1)
plot(x,noisy.y,col='blue',xlab='x',main='Observed data')
lines(x,y,col='red',lwd=3)
points(x,fitted(model4))
plot(fitted(model4),stats::residuals(model4))
plot(model3$fitted,model3$residuals)

summary(model4)
anova(model4)

###################################################
###################################################
model31 <- lm(noisy.y ~ x + I(x^2) + I(x^3)) #poly(x,1)
plot(x,noisy.y,col='blue',xlab='x',main='Observed data')
lines(x,y,col='red',lwd=3)
points(x,fitted(model31),col='green')
plot(fitted(model31),stats::residuals(model31))
plot(model31$fitted,model31$residuals)

summary(model31)
anova(model31)

###################################################
###################################################
model41 <- lm(noisy.y ~ x + I(x^2) + I(x^3) + I(x^4)) 
plot(x,noisy.y,col='blue',xlab='x',main='Observed data')
lines(x,y,col='red',lwd=3)
points(x,fitted(model41),col='green')
summary(model41)

###################################################
###################################################
model42 <- lm(noisy.y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x*x^4)) 
plot(x,noisy.y,col='blue',xlab='x',main='Observed data')
lines(x,y,col='red',lwd=3)
points(x,fitted(model42),col='green')
summary(model42)


###################################################
###################################################
model51 <- lm(noisy.y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5)) 
plot(x,noisy.y,col='blue',xlab='x',main='Observed data')
lines(x,y,col='red',lwd=3)
points(x,fitted(model51),col='green')
summary(model51)

###################################################
###################################################
model61 <- lm(noisy.y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6)) 
plot(x,noisy.y,col='blue',xlab='x',main='Observed data')
lines(x,y,col='red',lwd=3)
points(x,fitted(model61),col='green')
summary(model61)

###################################################
model4 <- lm(noisy.y ~ poly(x,4)) #poly(x,1)
plot(x,noisy.y,col='blue',xlab='x',main='Observed data')
lines(x,y,col='red',lwd=3)
points(x,fitted(model4))
plot(fitted(model4),stats::residuals(model4))
plot(model4$fitted,model4$residuals)

summary(model4)
anova(model4)

###################################################
###################################################
model5 <- lm(noisy.y ~ poly(x,5)) #poly(x,1)
plot(x,noisy.y,col='blue',xlab='x',main='Observed data')
lines(x,y,col='red',lwd=3)
points(x,fitted(model5))
plot(fitted(model5),stats::residuals(model5))
plot(model5$fitted,model5$residuals)

summary(model5)
anova(model5)

###################################################
###################################################
model6 <- lm(noisy.y ~ poly(x,6)) #poly(x,1)
plot(x,noisy.y,col='blue',xlab='x',main='Observed data')
lines(x,y,col='red',lwd=3)
points(x,fitted(model6))
plot(fitted(model6),stats::residuals(model6))
plot(model6$fitted,model6$residuals)

summary(model6)
anova(model6)
###################################################
###################################################
# model8 <- lm(noisy.y ~ x + I(x^2) + I(x^3)+ I(x^4) + I(x^5)+ I(x^6) + I(x^7))
# summary(model8)
# 
# ols_step_best_subset(model8)
# # plot
# model8 <- lm(noisy.y ~ x + I(x^2) + I(x^3)+ I(x^4) + I(x^5)+ I(x^6) + I(x^7)) 
# k <- ols_step_best_subset(model8)
# plot(k)




