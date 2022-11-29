data(iris)
head(iris)
library(ggplot2)

##Point-slope line function
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

##mean/sd of value
mean(iris$Sepal.Length)
sd(iris$Sepal.Length)

##simple plot
plot(iris$Sepal.Length, iris$Sepal.Width)

#delineation of center value
data_center_x = mean(iris$Sepal.Length)
data_center_y = mean(iris$Sepal.Width)

center <- c(data_center_x, data_center_y)

##plot of center point 
plot(x = iris$Sepal.Length, y = iris$Sepal.Width,)
points(x = data_center_x, y = data_center_y, col = "red")

##Point-slope line plot
plot(x = iris$Sepal.Length, y = iris$Sepal.Width, 
     xlab = "Sepal Width (mm)",
     ylab = "Sepal length (mm)",
     main = "Evan's Sepal Length vs Sepal Width plot!")

#center point
points(x = data_center_x, y = data_center_y, col = "red")

#slope line
curve(
  line_point_slope(
    x, 
    data_center_x, 
    data_center_y,
    mean(iris$Sepal.Length / iris$Sepal.Width)), 
  add = TRUE)

ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) +
  geom_point(size = iris$Petal.Width) +
  labs(x = "Sepal Length",
       y = "Sepal Width",
       title = "Evan's Sepal ggplot")
