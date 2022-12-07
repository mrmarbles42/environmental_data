require(here)

#data----
dat_1 = read.csv(here("data", "bird.sub.csv"))
dat_2 = read.csv(here("data", "hab.sub.csv"))
birdhab = merge(dat_1, dat_2, by = c("basin", "sub"))
dim(birdhab)

#functions----

linear <- function(x, y_int, slope) {
  return((x * slope) + y_int)
}

linear_simulator = function(x, y_int, slope, st_dev) {
  sim <- linear(x, y_int, slope)
  
  ifelse(x > 0,
  return(rnorm(lengths(sim), 
               mean = sim, 
               sd = st_dev)), 
  return(0))
}


linear_sim_fit = function(x, slope, y_int, st_dev) {
  y_sim = linear_simulator(
    x = x,
    y_int = y_int,
    slope = slope,
    st_dev = st_dev
  )
  fit_sim = lm(y_sim ~ x)
  return(fit_sim)
}

#Sample code ----
alpha = 0.05

# Start with a small number
n_sims = 10
p_vals = numeric(n_sims) 

# What was the observed standard deviation?
sd_obs

# specify the number of different standard deviation values to simulate:
# Start with a small number
n_sds = 20
pop_sds = seq(from = 0.05, to = , length.out = n_sds)

# The maximum x value in the simulation.
# Use the maximum observed x-value in the data
max_x = max(birdhab$ls)
pop_sd_powers = numeric(n_sds)
sample_sizes = seq(5, 100)
sim_output_3 = matrix(nrow = length(effect_sizes), ncol = length(sample_sizes))

for(k in 1:length(pop_sds))
{
  pop_sd_k = pop_sds[k]
  
  for(j in 1:length(sample_sizes))
  {
    x_vals = seq(0, max_x, length.out = sample_sizes[j])
    
    for(i in 1:n_sims)
    {
      fit_sim = linear_sim_fit(
        x = x_val,
        y_int = int_obs,
        slope = effect_size,
        st_dev = sd_obs
      )
      p_vals[i] = summary(fit_sim)$coefficients
    }
    
    sim_output_3[k, j] = ...
  }
  print(paste0("Testing standard deviation ", k, " of ", n_sds))
}

image(sim_output_3)

sim_3_dat = 
  list(
    power       = sim_output_3,
    sample_size = sample_sizes,
    pop_sd      = pop_sds)


# You should save your simulation results so you don't have to run it every time.
save(
  sim_3_dat, 
  file = here::here("data", "lab_ll_sim_output_dispersion_n_1000.RData"))



# plot(BRCR ~ ls, data = birdhab)
# fit_1 = lm(BRCR ~ ls, data = birdhab)
# abline(fit_1)
# summary(fit_1)

#linear simulator tests----

# ## positive
# n = 200
# 
# par(mfrow = c(2, 2), mar = c(1, 1, 1, 1))
# for (i in 1:4)
# {
#   x = runif(n = n)
#   plot(
#     x,
#     linear_simulator(x, y_int = 1, slope = 4.5, st_dev = 0.1),
#     main = "", xlab = "x", ylab = "y",
#     pch = 16, col = rgb(0, 0.2, 0, 0.2),
#     axes = FALSE)
#   box()
# }
# 
# 
# #negative
# n = 400
# 
# par(mfrow = c(2, 2), mar = c(1, 1, 1, 1))
# for (i in 1:4)
# {
#   x = runif(n = n)
#   plot(
#     x, linear_simulator(x, y_int = 10, slope = -6.5, st_dev = 1.1),
#     main = "", xlab = "x", ylab = "y",
#     pch = 16, col = rgb(0, 0.2, 0, 0.2),
#     axes = FALSE)
#   box()
# }


#model----

#BRCR vs late succession
fit_1 <- lm(birdhab$BRCR ~ birdhab$ls)

fit_1_coef <- coef(fit_1)
str(fit_1_coef)

fit_1_sum <- summary(fit_1)
str(fit_1_sum)

int_obs <- fit_1_coef[1]
slope_obs <- fit_1_coef[2]
sd_obs <- fit_1_sum$sigma

##plot1
plot(
  x = birdhab$ls, 
  y = linear_simulator(
    x = birdhab$ls,
    y_int = int_obs,
    slope = slope_obs,
    st_dev = sd_obs
  ),
  main = "Simulated Data",
  xlab = "late-successional forest",
  ylab = "Brown Creeper Abundance")





#power analysis for LRM----

##single simulation
y_sim = linear_simulator(
  x = birdhab$ls,
  y_int = int_obs,
  slope = slope_obs,
  st_dev = sd_obs
)

fit_sim = lm(y_sim ~ birdhab$ls)
summary(fit_sim)


##simulating effect sizes
alpha = 0.05
n_sims = 1000
p_vals = numeric(n_sims)

n_effect_sizes = 20
effect_sizes_1 = seq(-.01, .01, length.out = n_effect_sizes)

effect_size_powers = numeric(n_effect_sizes)

for(j in 1:n_effect_sizes)
{
  for(i in 1:n_sims)
  {
    fit_sim = linear_sim_fit(
      x = birdhab$ls,
      y_int = int_obs,
      slope = effect_sizes_1[j],
      st_dev = sd_obs
    )
    
    p_vals[i] = summary(fit_sim)$coefficients[2, 'Pr(>|t|)']
  }
  effect_size_powers[j] = sum(p_vals < alpha) / n_sims
}

sim_effect_size = 
  data.frame(
    effect_size = effect_sizes_1,
    power       = effect_size_powers)

plot(
  power ~ effect_size, data = sim_effect_size,
  type = 'l', xlab = 'Effect size', ylab = 'Power')
abline(v = slope_obs, lty = 2, col = 'red')


##simulating sample sizes
alpha = 0.05
n_sims = 1000
p_vals = numeric(n_sims)

sample_sizes = seq(5, 100)
sample_size_powers = numeric(length(sample_sizes))

# The maximum x value in the simulation.
# Use the maximum observed x-value in the data
max_x = max(birdhab$ls)

for(j in 1:length(sample_sizes))
{
  # A sequence of equally-spaced x-values:
  x_vals = seq(0, max_x, length.out = sample_sizes[j])
  
  for(i in 1:n_sims)
  {
    fit_sim = linear_sim_fit(
      x = x_vals,
      y_int = int_obs,
      slope = slope_obs,
      st_dev = sd_obs
    )
    p_vals[i] = summary(fit_sim)$coefficients[2, 'Pr(>|t|)']
  }
  sample_size_powers[j] = sum(p_vals < alpha) / n_sims
}

sim_sample_size = 
  data.frame(
    sample_size = sample_sizes,
    power       = sample_size_powers)
plot(
  power ~ sample_size, data = sim_sample_size,
  type = 'l', xlab = 'Sample size', ylab = 'Power')
abline(v = nrow(birdhab), lty = 2, col = 'red')


#Questions ----
## Q1
# Line plot of standard deviation (x-axis) and statistical power (y-axis)
plot(
  power ~ sd,
  data = sim_output_dispersion,
  type = "l")
# Add a dotted vertical red line at the observed population standard deviation value.
abline(v = sd_obs, lty = 2, col = 2)


## Q3

contour(
  y = sim_3_dat$sample_size,
  x = sim_3_dat$pop_sd,
  z = sim_3_dat$power,
  ylab = "sample size",
  xlab = "standard deviation",
  main = "Contour Plot of Statistical Power",
  levels = seq(0, 1, length.out = 9),
  drawlabels = TRUE,
  # method = "simple")
  method = "edge")

## Q5

require(rgl)
persp3d(
  y = sim_3_dat$sample_size,
  x = sim_3_dat$pop_sd,
  z = sim_3_dat$power
)