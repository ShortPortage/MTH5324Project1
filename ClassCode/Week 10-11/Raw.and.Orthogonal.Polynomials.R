#########################################################################
# Raw vs. Orthogonal Polynomials
#########################################################################

set.seed ( 1 )
n <- 100
x <- runif(n , 0 , 10)
y <- 2 + 1.5*x - 0.3*x^2 + rnorm( n , 0 , 2 )
# Raw p o l y n o m i a l
model.raw <- lm( y ~ x + I ( x^2 ) )
summary( model.raw )
# C o r r e l a t i o n
cor ( x , x^2)


Xp = poly(x,2)
model.ort <- lm( y ~ Xp )
summary( model.ort )
# C o r r e l a t i o n
cor ( Xp[,1] ,Xp[,2])

#########################################################################
# Stability
#########################################################################

set.seed ( 1 )
n <- 100
x <- runif (n , 0 , 10 )
y <- 3 + 2*x - 0.5*x^2 + 0.05*x^3 + rnorm(n , 0 , 5 )
model.raw <- lm( y ~ x + I ( x^2 ) + I ( x^3 ) )
summary( model.raw )
cor( cbind(x, x^2 , x^3) )

set.seed ( 1 )
x <- runif (n , 0 , 10 )
y <- 3 + 2*x - 0.5*x^2 + 0.05*x^3 + rnorm(n , 0 , 5 )
Xp = poly(x,3)
model.ort <- lm( y ~ Xp )
summary( model.ort )
# C o r r e l a t i o n
cor(Xp)

#########################################################################
# Numerical Instability
#########################################################################
set.seed ( 1 )
x.big <- seq (1 ,1000 , length=100)
y.big <- 5 + 2*x.big - 0.01*x.big^2 + 0.00001*x.big^3 + rnorm(100 , 0 , 50 )
m.raw.big <- lm( y.big ~ x.big + I ( x.big^2) + I ( x.big^3) )
m.orth.big <- lm( y.big ~ poly( x.big , 3 ) )
summary(m.raw.big)
summary(m.orth.big )



