
########################################################
# Simple Operations
########################################################
x = 1 + 2

1+2*3-5
(1+2)*3-5

x <- 4
x = 4

x + 1
y = x + x
y

x<- 1+2+3 + 4

2 + 2 -> y

y <- 100
z <- 20
x <- y - z
x
print(x)

income <- 100
taxes <- 20

########################################################
# INCORRECT Variable Names
########################################################
# net income <- income - taxes
# net-income <- income - taxes
# ca$h <- income - taxes

########################################################
# Multiple commands separated by ;
########################################################
income <- 200; taxes <- 30


########################################################
# Vector 
########################################################
age_person_1 <- 28
age_person_2 <- 48
age_person_3 <- 47
# ...and so on

age <- c(28, 48, 47, 71, 22, 80, 48, 30, 31)
purchase <- c(20, 59, 2, 12, 22, 160, 34, 34, 29)

distances <- c(687, 5076, 7270, 967, 6364, 1683, 9394, 5712, 5206,
               4317, 9411, 5625, 9725, 4977, 2730, 5648, 3818, 8241,
               5547, 1637, 4428, 8584, 2962, 5729, 5325, 4370, 5989,
               9030, 5532, 9623)
distances[7]
distances[10]

########################################################
# Vector indexing and length
########################################################
age[1]
age[5]

dist = seq(10,100,by = 10)
length(dist)
dist[7]
dist[10]

########################################################
# Visualize
########################################################
plot(age,purchase)

########################################################
# Covariance vs. Correlation
########################################################
age.mnt = age*12
age.dcd = age/10

cov(age,purchase)
cov(age.mnt,purchase)
cov(age,purchase)*12
cov(age.dcd,purchase)

cor(age,purchase)
cor(age.mnt,purchase)
cor(age.dcd,purchase)

########################################################
# Correlation: Person vs Spearman
########################################################
cov(age,purchase)
cor(age,purchase,method="spearman")

########################################################
# Spearman is Pearson applied on Rank 
########################################################
cor(rank(age),rank(purchase))

########################################################
# Sample Statistics: Mean, Variance, Standard Deviation
########################################################
x.bar = mean(age)
var.hat = var(age)
sd.hat = sd(age)

########################################################
# Calculate the Statistics: Sample Mean
########################################################
x.bar.2 = sum(age)/length(age)

########################################################
# Set Seed to have the same instant of random samples among users
########################################################
set.seed(1234)

########################################################
# Generating random samples from Normal distribution
########################################################
grades.smple = rnorm(20,75,15)

age.smp = abs(round(rnorm(9,mean(age),sd(age))))

age.smp

sort(age)

bookstore <- data.frame(age, purchase)

########################################################
# Generating perfect linear relationships 
########################################################
x = seq(-5,10,by=0.5)
y = -8 + 3*x
plot(x,y)

########################################################
# Generating and visualizing noise using Normal distribution
########################################################
set.seed(1000)
nrm.smp = rnorm(100,0,20)
plot(nrm.smp)
hist(nrm.smp)

########################################################
# Add noise to the perfect linear relationship and quantify the correlation
########################################################
set.seed(1000)
yobs = y + rnorm(length(x),0,20)

plot(x,y)
cor(x,y)
cor(x,y,method="spearman")

plot(x,yobs)
cor(x,yobs)
cor(x,yobs,method="spearman")

########################################################
# Simulate a p-order polynomial, here p = 2
########################################################
x = seq(-5,20,by=0.5)
y = (x-8)*(x+2)
plot(x,y)

cov(x,y)
cor(x,y)

########################################################
# Simulate a p-order polynomial, here p = 3
########################################################
x = seq(-5,20,by=0.5)
y = (x-8)*(x+2)*(x-12)
plot(x,y)

cov(x,y)
cor(x,y)



########################################################





