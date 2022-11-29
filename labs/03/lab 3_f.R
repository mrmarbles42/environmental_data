require(psych)
library(here)

pairs.panels(iris)

pairs.panels(iris[, c("Sepal.Length", "Sepal.Width", "Petal.Length")])

dat_bird <- read.csv(here("data", "bird.sta.csv"))
dat_habitat <- read.csv(here("data", "hab.sta.csv"))
dat_all <- merge(dat_bird, dat_habitat)
cewa_present_absent <- dat_all$CEWA > 0

plot(x = dat_all$elev, y = cewa_present_absent)

##Functions##
# Function to calculate the logistic parameter a given the slope and midpoint
get_logistic_param_a = function(slope, midpoint)
{
  b = slope / 4
  return (-midpoint * (slope / 4))
}

# Function to calculate the logistic parameter b given the slope
get_logistic_param_b = function(slope)
{
  return (slope / 4)
}


# Calculate the value of the logistic function at x, given the parameters a and b.
logistic = function(x, a, b)
{
  val = exp(a + b * x)
  return(val / (1 + val))
}

# Calculate the value of the logistic function at x, given a slope and midpoint.
logistic_midpoint_slope = function(x, midpoint, slope)
{
  b = get_logistic_param_b(slope)
  a = get_logistic_param_a(slope, midpoint)
  return(logistic(x, a, b))
}


plot(x = dat_all$elev, y = cewa_present_absent)
curve(logistic_midpoint_slope(x, midpoint = 400, slope = -0.1), add = TRUE)

