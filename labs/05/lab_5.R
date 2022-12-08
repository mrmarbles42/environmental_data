require(here)

#Functions----
ricker_fun = function(x, a, b)
{
  return(a * x * exp(-b * x))
}

exp_fun <- function(a, b, x) {
  return(a * exp(-b * x))
}


line_point_slope = function(x, x1, y1, slope)
{
  get_y_intercept = 
    function(x1, y1, slope) 
      return(-(x1 * slope) + y1)
  
  linear = 
    function(x, yint, slope) 
      return(yint + x * slope)
  
  return(linear(x, get_y_intercept(x1, y1, slope), slope))
}


#Data import ----
disp <- read.csv(here("data", "dispersal.csv"))


#Exponential ----
curve(
  exp_fun(2.2, 1/15, x),
  from = 0,
  to = 50,
  add = F,
  axes = T,
  main = "Exponential function",
  ylab = "f(x)",
  xlab = "x"
)

par(mfrow = c(2,2))
curve(
  exp_fun(1.9, 0.1, x),
  from = 0,
  to = 50,
  add = F,
  col = "black",
  main = "Exponential function (a = 1.9, b = 0.1)",
  ylab = "f(x)"
)

##2
curve(
  exp_fun(1.9, 0.3, x),
  from = 0,
  to = 15,
  add = F,
  col = "black",
  lty = "dotted",
  main = "Exponential function (a = 1.9, b = 0.3)",
  ylab = "f(x)"
)

##3
curve(
  exp_fun(1.2, 0.2, x),
  from = 0,
  to = 25,
  add = F,
  col = "red",
  main = "Exponential function (a = 1.2, b = 0.2)",
  ylab = "f(x)"
)

##4
curve(
  exp_fun(1.2, 0.4, x),
  from = 0,
  to = 15,
  add = F,
  col = "red",
  lty = "dotted",
  main = "Exponential function (a = 1.2, b = 0.4)",
  ylab = "f(x)"
)

#Ricker----

curve(
  ricker_fun(x, 1, 1),
  from = 0,
  to = 5,
  add = FALSE,
  main = "Ricker function: a = 1, b = 1",
  ylab = "f(x)",
  xlab = "x"
)

par(mfrow = c(2,3))
##1
curve(
  ricker_fun(x, 25, 0.2),
  from = 0,
  to = 50,
  add = FALSE,
  main = "Ricker function: a = 25, b = 0.2",
  ylab = "f(x)",
  xlab = "x"
)

##2
curve(
  ricker_fun(x, 20, 0.2),
  from = 0,
  to = 55,
  add = FALSE,
  main = "Ricker function: a = 20, b = 0.2",
  ylab = "f(x)",
  xlab = "x",
  lty = "dotted"
)

##3
curve(
  ricker_fun(x, 10, 0.2),
  from = 0,
  to = 50,
  add = FALSE,
  main = "Ricker function: a = 10, b = 0.2",
  ylab = "f(x)",
  xlab = "x",
  lty = "dotted"
)

##4
curve(
  ricker_fun(x, 75, 0.3),
  from = 0,
  to = 40,
  add = FALSE,
  main = "Ricker function: a = 75, b = 0.3",
  ylab = "f(x)",
  xlab = "x",
  col = "red"
)

##5
curve(
  ricker_fun(x, 50, 0.3),
  from = 0,
  to = 40,
  add = FALSE,
  main = "Ricker function: a = 50, b = 0.3",
  ylab = "f(x)",
  xlab = "x",
  lty = "dotted",
  col = "red"
)

##6
curve(
  ricker_fun(x, 40, 0.3),
  from = 0,
  to = 40,
  add = FALSE,
  main = "Ricker function: a = 40, b = 0.3",
  ylab = "f(x)",
  xlab = "x",
  lty = "dotted",
  col = "red"
)

ftb_lm <- lm(disp$disp.rate.ftb ~ disp$dist.class)
#plots----

plot(x = disp$dist.class, 
     y = disp$disp.rate.ftb,
     xlab = "Distance Class",
     ylab = "Dispersal rate",
     main = "First-time breeder Dispersal rate vs Distance class (exponential)") 
curve(line_point_slope(x, 0, 0.45, -0.00035), add = T)  
#abline(0.45, -0.00035)
  

##Expo
plot(x = disp$dist.class, 
     y = disp$disp.rate.ftb,
     xlab = "Distance Class",
     ylab = "Dispersal rate",
     main = "First-time breeder Dispersal rate vs Distance class (exponential)") 
curve(
  exp_fun(1.2, 0.0045, x),
  from = 0,
  to = 1750,
  add = T,
  main = "Exponential function",
  ylab = "f(x)",
  xlab = "x"
)


##ricker
plot(x = disp$dist.class, 
     y = disp$disp.rate.ftb,
     xlab = "Distance Class",
     ylab = "Dispersal rate",
     main = "First-time breeder Dispersal rate vs Distance class (Ricker)"
     )

curve(
  ricker_fun(x, 0.008, 0.00475),
  from = -100,
  to = 1750,
  add = T
)

##Residuals

#ftb is observed

par(mfrow = c(1,3))
####lm
lm_obs <- line_point_slope(disp$dist.class, 0, 0.45, -0.00035)
lm_exp <- disp$disp.rate.ftb

resids_linear <- data.frame(lm_obs - lm_exp)
hist(resids_linear,
     main = "Linear model residuals")

####expo 
expo_obs <- exp_fun(1.2, 0.0045, disp$dist.class)
expo_exp <- disp$disp.rate.ftb

resids_exp <-  data.frame(expo_obs - expo_exp)
hist(resids_exp,
     main = "Exponential model residuals")

####Ricker
rick_obs <- ricker_fun(disp$dist.class, 0.008, 0.00475)
rick_exp <- disp$disp.rate.ftb

resids_ricker <-  data.frame(rick_obs - rick_exp)

hist(resids_ricker,
     main = "Ricker model residuals")

resids_linear <- data.frame(lm_obs - lm_exp)
resids_exp <-  data.frame(expo_obs - expo_exp)
resids_ricker <-  data.frame(rick_obs - rick_exp)

residual_model_df <- cbind(resids_linear,resids_exp,resids_ricker) 
