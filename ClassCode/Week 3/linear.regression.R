
set.seed(1234)
x = seq(-5,10,by=0.5)
E.y = -3 + 2*x
y.obs = E.y + rnorm(length(E.y),0,5) 
plot(x,E.y,type = 'l',col = "red")
#plot(x,E.y,col = "red")
points(x,y.obs,col = "blue")
plot(x,y.obs,col = "blue")


reg.mdl = lm(y.obs~x)
abline(reg.mdl,col = "green", lw = 3)
summary(reg.mdl)
reg.summary = summary(reg.mdl)

