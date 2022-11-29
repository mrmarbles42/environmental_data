#setup----
library(palmerpenguins)
require(simpleboot)
require(boot)
require(here)
require(dplyr)
require(rstatix)

penguin_dat = droplevels(subset(penguins, species != "Gentoo"))

dat_bird <- read.csv(here("data", "bird.sub.csv"))
dat_habitat <- read.csv(here("data", "hab.sub.csv"))
dat_all <- merge(dat_bird, dat_habitat,
                 by = c("basin", "sub"))

veg <- read.csv(here("data", "vegdata.csv"))






#Q1-Q4----

pen_boot <- two.boot(
  na.omit(subset(penguin_dat, species == "Adelie")$flipper_length_mm),
  na.omit(subset(penguin_dat, species == "Chinstrap")$flipper_length_mm),
                     mean, R = 10000,
                     na.rm = T)
str(pen_boot)

#95% bootstrap CI
quantile(pen_boot$t, c(0.025, 0.975))

#mean/ median differences
mean(pen_boot$t)
median(pen_boot$t)

#1
sd(pen_boot$t, na.rm = T)
#2
hist(pen_boot$t,
     main = "pen_boot Bootstrapped Differences",
     xlab = "Flipper Lengths (mm)")
#3
quantile(pen_boot$t, c(0.025, 0.975))
#4


rm(penguin_dat)

#Q5-7----
pen_ecdf <- ecdf(pen_boot$t)
pen_ecdf(-8)
plot(pen_ecdf)


rm(pen_ecdf)

#Q9----
veg <- read.csv(here("data", "vegdata.csv"))
dat_tree <- veg %>%
  filter(treatment %in% c("control", "clipped"))

boxplot(pine ~ treatment, dat = dat_tree)

veg_boot <- two.boot(veg$treatment, 
                     veg$pine,
                     mean, R = 1000)


#Q10-11----

pine <- veg %>%
  select(block, plot, date, treatment, pine)

ctrl_pine <- pine %>%
  filter(treatment == "control")
clip_pine <- pine %>%
  filter(treatment == "clipped")

tree_boot <- two.boot(ctrl_pine$pine, 
                      clip_pine$pine, 
                      mean, R = 10000,
                      na.rm = T)
boot.ci(tree_boot)
quantile(tree_boot$t, c(0.025, 0.975))

hist(tree_boot$t)
#Q12-17----
b_sidi_mean = mean(dat_all$b.sidi, na.rm = TRUE)
b_sidi_sd   = sd(dat_all$b.sidi, na.rm = TRUE)

s_sidi_mean = mean(dat_all$s.sidi, na.rm = T)
s_sidi_sd = sd(dat_all$s.sidi, na.rm = T)

dat_all$b.sidi.standardized <- ((dat_all$b.sidi - b_sidi_mean)/ b_sidi_sd)
dat_all$s.sidi.standardized <- ((dat_all$s.sidi - s_sidi_mean)/ s_sidi_sd)

dat_1 = 
  subset(
    dat_all,
    select = c(b.sidi, s.sidi))

#MC resample loop

m = 10000
result_mc = numeric(m) 
set.seed(42)
for(i in 1:m) 
  {
  index_1 = sample(nrow(dat_1), replace = T)
  index_2 = sample(nrow(dat_1), replace = T)
  
  dat_1_resample =
    data.frame(b.sidi = dat_1$b.sidi[index_1],
               s.sidi = dat_1$s.sidi[index_2])
  
  fit_resampled_i <- lm(b.sidi ~ s.sidi, 
                        data = dat_1_resample)
  slope_resampled = coef(fit_resampled_i)[2]
  crit <- quantile(result_mc, c(0.05))
  
  result_mc[i] = coef(fit_resampled_i)[2]
} 


hist(result_mc,
     xlab = "Regression slopes",
     main = "Monte Carlo distribution of Regression Slope")
abline(v = slope_resampled, lty = "solid", col = "blue", lwd = 2)
abline(v = crit, lty = "dotted", col = "red", lwd = 2)
#Q18-20----

# BS resample loop
n = 10000
result <- numeric(n)
set.seed(42)
for (i in 1:m)
{
  
  index <- sample(nrow(dat_1),
                  replace = T)
  
  dat_boot = dat_1[index,]
  head(dat_boot)
  
  fit_boot = lm(dat_boot$b.sidi ~ dat_boot$s.sidi)
  result[i] <- coef(fit_boot)
}
hist(result)

mc_dens <- density(result_mc)
boot_dens <- density(result)

plot(mc_dens,
     xlim = c(-0.05, 0.085),
     ylim = c(0, 120),
     col = "red",
     main = "Null and Alternative resampling distributions",
     xlab = "Slope Coefficient")

lines(boot_dens,
      lwd = 2,
      lty = "dotted",
      col = "blue")

legend(x = "top", legend = c("Null", "Alt."), lty = c(1, 4), lwd = 2, col = c("red", "steelblue"),
       bty = "n")
