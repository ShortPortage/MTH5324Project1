

?mtcars
View(mtcars)
library(ggplot2)
ggplot(mtcars, aes(hp, mpg)) +
  geom_point()
m <- lm(mpg ~ hp, data = mtcars)
summary(m)


m2 <- lm(mpg ~ hp + wt, data = mtcars)
summary(m2)

m3 <- lm(mpg ~ hp + wt + cyl, data = mtcars)
summary(m3)

mtcars$cyl = as.factor(mtcars$cyl)

m3.1 <- lm(mpg ~ hp + wt + cyl, data = mtcars)
summary(m3.1)
