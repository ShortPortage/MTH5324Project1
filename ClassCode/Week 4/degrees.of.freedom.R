

set.seed(1000)
smp5 = rnorm(5,10,5)
round(smp5,2)
mean(smp5)

set.seed(1000)
smp4 = rnorm(4,10,5)
rsmp = round(smp4,2)
x.bar = 10 # Enfoced Condition
x5 = 5*x.bar - sum(rsmp)


