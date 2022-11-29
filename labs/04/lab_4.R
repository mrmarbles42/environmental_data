library(here)

##functions and necessary info

# Calculates the value of y for a linear function, given the coordinates
# of a known point (x1, y1) and the slope of the line.
line_point_slope = function(x, x1, y1, slope){
  
  get_y_intercept = 
    function(x1, y1, slope) 
      return(-(x1 * slope) + y1)
  
  linear = 
    function(x, yint, slope) 
      return(yint + x * slope)
  
  return(linear(x, get_y_intercept(x1, y1, slope), slope))
}

#####
# Generate a vector of x-values
x = seq(-6, 6, length.out = 100)
y = dnorm(x)

plot(x, y, main = "Standard Normal PDF", type = "l", xlim = c(-3, 3))
abline(h = 0)

#####
n_pts = 10
x_min = 1
x_max = 10

# X values are uniformly distributed
x_random = runif(n = n_pts, min = x_min, max = x_max)

# Y values are normally-distributed.
# I used the default parameters for mean and sd.
y_random = rnorm(n = n_pts)

dat_random = data.frame(x = x_random, y = y_random)

#plot(y ~ x, data = dat_random, pch = 8)



##### 
#plots and Q's

#Q1
norm_17 <- rnorm(n = 17, mean = 10.4, sd = 2.4)
norm_30 <- rnorm(n = 30, mean = 10.4, sd = 2.4)
norm_300 <- rnorm(n = 300, mean = 10.4, sd = 2.4)
norm_3000 <- rnorm(n = 3000, mean = 10.4, sd = 2.4)


#Q2-Q5
#png(filename = here("lab_04_hist_01.png"), width = 1500, height = 1600,
 #   res = 120, units = "px")
par(mfrow= c(2,2))
hist(norm_17,
     main = "Normal Distribution of 17 points")
hist(norm_30,
     main = "Normal Distribution of 30 points")
hist(norm_300,
     main = "Normal Distribution of 300 points")
hist(norm_3000,
     main = "Normal Distribution of 3000 points")
#

#Q6
"The parameters for the standard normal distribution are the mean and the standard deviation from the mean. The standard values are a mean of zero and a standard deviation of one."

#Q7
x <- seq(0, 20, length.out = 1000)
y <- dnorm(x, mean=10.4, sd=2.4)
plot(x , y, 
     type = "l",
     main = "Normal density distribution (mean = 10.4, sd = 2.4)")
abline(v=10.4)
#svg(filename = "norm_1.svg", 
    #width = 7,
    #height = 7)

#Q8

#Q9

point <- sample(10, size = 1000, replace = T)
xmin <- 1
xmax <- 1000


set.seed(42)
a <- rnorm(42, mean = 21, sd = 2)
#par(mfrow = c(2,2))
plot(log10(a), 
     type = "b",
     cex = 2,
     pch = 1,
     col = c("blue", "red"))

b <- rpois(n = 42, lambda = 42)
plot(b,
     type = "s",
     col = "lightblue")

plot(log10(a),
     type = "h",
     col = "forestgreen")

plot(a, 
     type = "o",
     col = "purple")

#Q11
n_pts = 10
x_min = 1
x_max = 10

x_random = runif(n = n_pts, min = x_min, max = x_max)
y_random = rnorm(n = n_pts)

guess_x <- 5
guess_y <- 0
guess_slope <- 0.1
plot(y~x, data = dat_random,
     main = "random datapoints + line")
curve(line_point_slope(x, guess_x, guess_y, guess_slope), add = T)



dat_random[,3] <- line_point_slope(dat_random$x, guess_x, guess_y, guess_slope)
names(dat_random)[3] <- "y_predicted"

residual <- (dat_random$y_predicted - dat_random$y)
dat_random[,4] <- residual
names(dat_random)[4] <- "residuals"

hist(dat_random$residuals,
     xlab = "residuals",
     main = "Predicted model residuals")

plot(residuals ~ y_predicted, data = dat_random,
     main = "Predicted Y-values vs residuals")
