dbinom(x = 2, size = 6, prob = 0.5)


```
q1. size = 6, prob = 2/3
q2. 0.3292181
q3. 0.001371742

pbinom(4, 6, prob = 2/3, lower.tail = T)

pbinom(3, 6, prob = 2/3, lower.tail = F)


dnorm(x, mean = 0, sd = 1)
pbinom(1,6,prob = 2/3)


##normal plots

n = 500
x = seq(-5,5, length.out = n)

y <-  dnorm(x, mean = 0, sd = 1)
y_2 <- dnorm(x, mean = 0, sd = 2)
y_3 <- dnorm(x, mean = -2, sd = 1)

y_cdf_1 <- pnorm(x, mean = 0, sd = 1)
y_cdf_2 <- pnorm(x, mean = 0, sd = 2)
y_cdf_3 <- pnorm(x, mean = -2, sd = 1)

#par(mfrow = c(1,2))

plot(y ~ x, type = "l", main = "PDF plot")
points(y_2 ~ x, type = "l", lty = "dotted")
points(y_3 ~ x, type = "l", lty = "dashed" )

plot(y_cdf_1, type = "l", main = "CDF plot")
points(y_cdf_2, type = "l", lty = 2)
points(y_cdf_3, type = "l", lty = 3)


x_bin = -5:5
y_bin_2 = dbinom(x_bin, size = 10, prob = 6/16)


barplot(
  height = y_bin_2,
  # the names to print with each bar:
  names.arg = x_bin,
  # Tells R to remove space between bars:
  space = 0,
  ylab = "Pr(x)",
  main = "Binomial: n = 5, p = 0.4")

