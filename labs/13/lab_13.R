
#random walk components----

##position
# How many steps?
# This is a variable... could be useful as a function argument later on.
n_steps = 1000

# Create the empty data.frame:
walk = data.frame(
  step = numeric(n_steps + 1),
  x = numeric(n_steps + 1),
  y = numeric(n_steps + 1)
)

##Direction
# Discrete set of angles.
# R works better with radians than degrees.

thetas = c(0, 0.5 * pi, pi, 1.5 * pi)

# Choose a random angle
theta = sample(thetas, size = 1)
print(theta, digits = 3)

# Calculate the x- and y- distances
# Round to 3 decimal points
delta_x = round(cos(theta), 3)
delta_y = round(sin(theta), 3)

print(data.frame(delta_x = delta_x, delta_y = delta_y))

##step length

# step_length = 1
# step_length = runif(n = 1, min = 1, max = 2)
step_length = rexp(n = 1, rate = 1)
#Random walk sample----
n_steps = 1000

# Create our empty data frame
walk = data.frame(
  step = numeric(n_steps + 1),
  x = numeric(n_steps + 1),
  y = numeric(n_steps + 1)
)

for (i in 1:n_steps)
{
  # Choose a step length:
  step_length = rexp(n = 1, rate = 1)
  
  # Choose a random direction:
  theta = sample(thetas, size = 1)
  
  # What is the current position?
  current_x = walk$x[i]
  current_y = walk$y[i]
  
  # Figure out the change in the x- and y-directions:
  delta_x = step_length * round(cos(theta), 3)
  delta_y = step_length * round(sin(theta), 3)
  
  # Record the new position
  walk$x[i + 1] = current_x + delta_x
  walk$y[i + 1] = current_y + delta_y
  
  # Record the step number:
  walk$step[i + 1] = i
}

plot(y ~ x, data = walk, type = "l", asp = 1)

#random (grid) walk function----
grid_walk <- function(n_steps) {
  n_steps = n_steps
  thetas = c(0, 0.5 * pi, pi, 1.5 * pi)
  
  # Create our empty data frame
  walk = data.frame(
    step = numeric(n_steps + 1),
    x = numeric(n_steps + 1),
    y = numeric(n_steps + 1)
  )
  
  for (i in 1:n_steps)
  {
    # Choose a step length:
    step_length = rexp(n = 1, rate = 1)
    
    # Choose a random direction:
    theta = sample(thetas, size = 1)
    
    # What is the current position?
    current_x = walk$x[i]
    current_y = walk$y[i]
    
    # Figure out the change in the x- and y-directions:
    delta_x = step_length * round(cos(theta), 3)
    delta_y = step_length * round(sin(theta), 3)
    
    # Record the new position
    walk$x[i + 1] = current_x + delta_x
    walk$y[i + 1] = current_y + delta_y
    
    # Record the step number:
    walk$step[i + 1] = i
  }
  return(dat_walk = walk)
}

plot_walk = function(dat_walk, ...)
{
  plot(y ~ x, data = dat_walk, type = "l", asp = 1, ...)
  
  # Plot a blue-filled point at the starting point
  points(
    y ~ x, data = head(dat_walk, 1),
    pch = 21, col = 1, bg = "steelblue")
  
  # Plot a red-filled point at the end of the walk
  points(
    y ~ x, data = tail(dat_walk, 1),
    pch = 21, col = 1, bg = "red")
}