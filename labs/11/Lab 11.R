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


#Sample code ----
alpha = 0.05

# Start with a small number
n_sims = 10
p_vals = ... 

# What was the observed standard deviation?
sd_obs

# specify the number of different standard deviation values to simulate:
# Start with a small number
n_sds = 20
pop_sds = seq(from = 0.05, to = , length.out = n_sds)

# The maximum x value in the simulation.
# Use the maximum observed x-value in the data
max_x = max(birdhab$ls)


pop_sd_powers = numeric(...)

sample_sizes = seq(5, 100)

sim_output_3 = matrix(...)

for(k in 1:length(pop_sds))
{
  pop_sd_k = pop_sds[k]
  
  for(j in 1:length(sample_sizes))
  {
    x_vals = seq(0, max_x, length.out = sample_sizes[j])
    
    for(i in 1:n_sims)
    {
      fit_sim = ...
      p_vals[i] = ...
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