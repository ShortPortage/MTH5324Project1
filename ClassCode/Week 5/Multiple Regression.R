

?mtcars
View(mtcars)
library(ggplot2)
ggplot(mtcars, aes(hp, mpg)) +
  geom_point()
m <- lm(mpg ~ hp, data = mtcars)
summary(m)


m2 <- lm(mpg ~ hp + wt, data = mtcars)
summary(m2)
