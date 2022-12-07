require(here)
#data----
dat_1 = read.csv(here("data", "bird.sub.csv"))
dat_2 = read.csv(here("data", "hab.sub.csv"))
birdhab = merge(dat_1, dat_2, by = c("basin", "sub"))


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

#model and coefficients----

#fit the modeled data
fit_1 <- lm(BRCR ~ ls, data = birdhab)

#extract model intercept and slope as int_obs and slope_obs
fit_1_coef <- coef(fit_1)

int_obs <- fit_1_coef[1]
slope_obs <- fit_1_coef[2]

#extract standard deviation as sd_obs 
fit_1_sum <- summary(fit_1)
sd_obs <- fit_1_sum$sigma



#sample size simulation----
alpha = 0.05
n_sims = 30
p_vals = numeric(n_sims)

sample_sizes = seq(1, 20)
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
    p_vals[i] = summary(fit_sim)$coefficients[2, "Pr(>|t|)"]
  }
  sample_size_powers[j] = sum(p_vals < alpha) / n_sims
}


sim_sample_size = 
  data.frame(
    sample_size = sample_sizes,
    power       = sample_size_powers)


#dispersion simulation----
alpha = 0.05
n_sims = 100
p_vals = numeric(n_sims)

# specify the number of different standard deviation values to simulate:
n_sds = 20
pop_sds = seq(from = 0.01, to = 1.5, length.out = n_sds)

pop_sd_powers = numeric(n_sds)

for(j in 1:length(pop_sds))
{
  pop_sd_j = pop_sds[j]
  for(i in 1:n_sims)
  {
    fit_sim = linear_sim_fit(
      x = birdhab$ls,
      y_int = int_obs,
      slope = slope_obs,
      st_dev = pop_sd_j
    )
    p_vals[i] = summary(fit_sim)$coefficients[2, 'Pr(>|t|)']
  }
  pop_sd_powers[j] = sum(p_vals / alpha) / n_sims
}

sim_output_dispersion = data.frame(
  sd = pop_sds,
  power = pop_sd_powers)


# You should save your simulation results so you don't have to run it every time.
save(
  sim_output_dispersion, 
  file = here::here("data", "lab_ll_dat_dispersion_sim.RData"))

# Line plot of standard deviation (x-axis) and statistical power (y-axis)
plot(sim_output_dispersion$power~sim_output_dispersion$sd,
     type = "l")
abline(v = sd_obs,
       lty = "dotted",
       col = "red")


#dispersion and sample size simulation----

alpha = 0.05
n_sims = 10
p_vals = numeric(n_sims)

# specify the number of different standard deviation values to simulate:
# Start with a small number
n_sds = 20
pop_sds = seq(from = 0.05, to = 3 * sd_obs, length.out = n_sds)
# The maximum x value in the simulation.
# Use the maximum observed x-value in the data
max_x = max(birdhab$ls)
pop_sd_powers = numeric(n_sds)
sample_sizes = seq(5, 100)
sim_output_3 = matrix(
  nrow = length(pop_sds),
  ncol = length(sample_sizes)
)
for(k in 1:length(pop_sds))
{
  pop_sd_k = pop_sds[k]
  
  for(j in 1:length(sample_sizes))
  {
    x_vals = seq(0, max_x, length.out = sample_sizes[j])
    
    for(i in 1:n_sims)
    {
      fit_sim = linear_sim_fit(
        x = x_vals,
        y_int = int_obs,
        slope = slope_obs,
        st_dev = pop_sd_k
      )
      p_vals[i] = summary(fit_sim)$coefficients[2, 'Pr(>|t|)']
    }
    
    sim_output_3[k, j] = sum(p_vals < alpha) / n_sims
  }
  print(paste0("Testing standard deviation ", k, " of ", n_sds))
}
image(sim_output_3)
sim_3_dat = 
  list(
    power       = sim_output_3,
    sample_size = sample_sizes,
    pop_sd      = pop_sds)

