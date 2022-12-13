require(here)
library(minpack.lm)

#data----
dat_1 = read.csv(here("data", "bird.sub.csv"))
dat_2 = read.csv(here("data", "hab.sub.csv"))
birdhab = merge(dat_1, dat_2, by = c("basin", "sub"))

dat_dispersal <- read.csv(here("data", "dispersal.csv"))

dat_bird = read.csv(here("data", "bird.sta.csv"))
dat_habitat = read.csv(here("data", "hab.sta.csv"))
dat_all = merge(dat_bird, dat_habitat)
dat_all$GCKI_pres = dat_all$GCKI > 0

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

ricker_fun = function(x, a, b) 
{
  return(a * x * exp(-b * x))
}

exp_fun <- function(a, b, x) {
  return(a * exp(-b * x))
}

# linear model and coefficients----

#fit the modeled data
fit_1 <- lm(BRCR ~ ls, data = birdhab)

#extract model intercept and slope as int_obs and slope_obs
fit_1_coef <- coef(fit_1)

int_obs <- fit_1_coef[1]
slope_obs <- fit_1_coef[2]

#extract standard deviation as sd_obs 
fit_1_sum <- summary(fit_1)
sd_obs <- fit_1_sum$sigma

# Ricker model and coefs----

fit_ricker_nls = nls(
  disp.rate.ftb ~ ricker_fun(dist.class, a, b),
  data = dat_dispersal,
  start = list(b = 0, a = 1))
summary(fit_ricker_nls)


# logistic regression----



#sample size simulation----
alpha = 0.05
n_sims = 30
p_vals = numeric(n_sims)

sample_sizes = seq(2, 21)
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



# lowess modeling----
fit_lowess_50 = loess(power ~ sample_size, data = sim_sample_size, span = 0.5)

newdata_sample_size = data.frame(sample_size = seq(2, 20, length.out = 100)) 

# exponential nls model ----

#example
# curve(
#   exp_fun(2.2, 1/15, x),
#   from = 0,
#   to = 50,
#   add = F,
#   axes = T,
#   main = "Exponential function",
#   ylab = "f(x)",
#   xlab = "x"
# )

fit_exp_nls = nls.lm(
  disp.rate.eb ~ exp_fun(dist.class, a, b),
  data = dat_dispersal,
  start = list(b = 1, a = 2))
summary(fit_exp_nls)

nls.lm(par = list(b = 1, a = 2), fn = exp_fun(dat_dispersal$dist.class, a, b))

dist_newdata = data.frame(dist.class = seq(0, 1600, length.out = 1600))  

lines(predict(fit_ricker_nls, newdata = dist_newdata))
legend("topright", legend = c("nls fit"), lty = 1, col = c(1))

# logistic model----

# Create model fits
fit_gcki_slope = glm(GCKI_pres ~ slope, data = dat_all, family = binomial)
fit_gcki_ba_tot = glm(GCKI_pres ~ ba.tot, data = dat_all, family = binomial)
fit_gcki_both_additive = glm(GCKI_pres ~ slope + ba.tot, data = dat_all, family = binomial)
fit_gcki_both_interactive = glm(GCKI_pres ~ slope * ba.tot, data = dat_all, family = binomial)

n = 500

slope_newdata = data.frame(
  slope = seq(
    from = min(dat_all$slope, na.rm = T),
    to = max(dat_all$slope, na.rm = T),
    length.out = n
  )
)

ba_newdata = data.frame(
  ba.tot = seq(
    from = min(dat_all$ba.tot, na.rm = T),
    to = max(dat_all$ba.tot, na.rm = T),
    length.out = n
  )
)
ba_newdata$gcki_predicted = 
  predict(
    fit_gcki_ba_tot,
    newdata = ba_newdata,
    type = "response"
  )


slope_newdata$gcki_predicted = 
  predict(
    fit_gcki_slope,
    newdata = slope_newdata,
    type = "response"
  )

plot(
  GCKI_pres ~ slope, data = dat_all,
  xlab = "Percent Slope",
  ylab = "GCKI presence/absence",
  pch = 16, cex = 1.5, col = gray(0, 0.2)
)
lines(gcki_predicted ~ slope, data = slope_newdata)

plot(
  GCKI_pres ~ ba.tot, data = dat_all,
  xlab = "Basal area",
  ylab = "GCKI presence/absence",
  pch = 16, cex = 1.5, col = gray(0, 0.2)
)

lines(gcki_predicted ~ ba.tot, data = ba_newdata)


#AIC test
AIC(fit_gcki_slope,
    fit_gcki_ba_tot,
    fit_gcki_both_additive,
    fit_gcki_both_interactive)
# contour----

n = 50

ba.tot = seq(
  from = min(dat_all$ba.tot, na.rm = T),
  to = max(dat_all$ba.tot, na.rm = T),
  length.out = n)
slope = seq(
  from = min(dat_all$slope, na.rm = T),
  to = max(dat_all$slope, na.rm = T),
  length.out = n)

new_dat_all = expand.grid(
  ba.tot = ba.tot,
  slope = slope)

new_dat_all$pred_add = predict(
  fit_gcki_both_additive,
  newdata = new_dat_all,
  type = "response")

z_gcki_add = matrix(
  new_dat_all$pred_add,
  nrow = length(ba.tot),
  byrow = FALSE)


new_dat_all$pred_int = predict(
  fit_gcki_both_interactive,
  newdata = new_dat_all,
  type = "response")

z_gcki_int = matrix(
  new_dat_all$pred_int,
  nrow = length(ba.tot),
  byrow = FALSE)


require(rgl)

rgl::persp3d(
  x = ba.tot,
  y = slope,
  z = z_gcki_add,
  col = "steelblue",
  xlab = "Basal Area",
  ylab = "Slope",
  zlab = "Pr(present)",
  alpha = 0.4)
rglwidget()

rgl::persp3d(
  x = ba.tot,
  y = slope,
  z = z_gcki_int,
  col = "steelblue",
  xlab = "Basal Area",
  ylab = "Slope",
  zlab = "Pr(present)",
  alpha = 0.4)
rglwidget()

par(mfrow = c(1, 2))
contour(
  x = ba.tot, y = slope,
  z = z_gcki_add,
  xlab = "Total Basal Area",
  ylab = "Percent Slope",
  main = "Additive")
contour(
  x = ba.tot,
  y = slope,
  z = z_gcki_int,
  xlab = "Total Basal Area",
  ylab = "Percent Slope",
  main = "Interactive")
# plots ----

#base plot
plot(sim_sample_size$sample_size, sim_sample_size$power,
     type = "l",
     ylim = 0:1)

#loess smoothing
plot(
  x = newdata_sample_size$sample_size,
  y = predict(fit_lowess_50, newdata = newdata_sample_size),
  type = "l",
  ylab = "Statistical Power", xlab = "Sample Size",
  main = "Statistical Power ~ Sample size (n)")
points(sim_sample_size$sample_size, sim_sample_size$power,
       col = "blue")
legend("bottomright", legend = c("Smoothed", "Original"),
       col = c("black", "blue"), lty = c(1,0), pch = c(-1, 1))


#questions----


#Q5
# There is a stat-significant slight negative relationship between GCKI presence and slope with a p-value of 1.01e-4 suggesting that observed presence decreases at higher altitudes.
# 
