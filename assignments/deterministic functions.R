require("here")

# Calculates the value of y for a linear function, given the coordinates
# of a known point (x1, y1) and the slope of the line.
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

#read data
dat_habitat <- read.csv(here("data", "hab.sta.csv"))

par(mfrow = c(3,1))
#histograms
hist(dat_habitat$elev)
hist(dat_habitat$slope)
hist(dat_habitat$aspect)

#scatterplots and linear models
plot(dat_habitat$ba.tot ~ dat_habitat$elev, 
     cex = 0.75,
     xlab = "Elevation (ft)",
     ylab = "Total Basal area")
elev_mod <- lm(ba.tot ~ elev, data = dat_habitat)
abline(elev_mod, col = "blue")

plot(dat_habitat$ba.tot ~ dat_habitat$slope, 
     cex = 0.75,
     xlab = "Slope (%)",
     ylab = "Total Basal area")
slope_mod <- lm(ba.tot ~ slope, data = dat_habitat)
abline(slope_mod, col = "blue")

plot(dat_habitat$ba.tot ~ dat_habitat$aspect, 
     cex = 0.75,
     xlab = "Aspect (degrees)",
     ylab = "Total Basal area")
asp_mod <- lm(ba.tot ~ aspect, data = dat_habitat)
abline(asp_mod, col = "blue")
