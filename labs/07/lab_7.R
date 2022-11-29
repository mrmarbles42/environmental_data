require(here)
require(boot)
require(palmerpenguins)
require(tidyverse)

penguins <- penguins

gentoo <- penguins %>%
  filter(species == "Gentoo") %>%
  drop_na()


# Choose significance level
alpha = 0.05

# 2: Calculate sample standard error:
n = sum(!is.na(gentoo$bill_length_mm))
sse = sd(gentoo$bill_length_mm, na.rm = TRUE) / sqrt(n-1)

# 3: Calculate critical t-values:
t_crit = abs(qt(alpha / 2, df = n - 1))

# 4: Calculate the CI radius:
ci_radius = sse * t_crit

# The CI is the sample mean +/- the radius:
gentoo_ci = c(
  lower = mean(gentoo$bill_length_mm) - ci_radius,
  upper = mean(gentoo$bill_length_mm) + ci_radius)

length(gentoo$bill_length_mm)

low_ci <- 47.5 - ci_radius
up_ci <-  47.5 + ci_radius
summary(gentoo$bill_length_mm)



m = 10000
result <- numeric(m)

for(i in 1:m)
{
  result[i] = mean(sample(gentoo$bill_length_mm, replace=TRUE))
}
mean(result)

summary(result)
quantile(result, c(0.025, 0.975))

gentoo_boot <- boot(
  data = gentoo$bill_length_mm,
  statistic = boot_mean,
  R = 10000
)

plot(gentoo_boot)


rare_mean = apply(rarefact, 2, mean)
rare_quant = apply(rarefact, 2, quantile, probs=c(0.025, 0.975))
rare = t(rbind(rare_mean, rare_quant))


matplot(
  rare,
  type='l',
  lty = c("dotted", "dashed", "solid"),
  col = c("blue", "forestgreen", "red"),
  xlab='Number of sampling plots',
  ylab='Species richness',
  main="Evan's Moth Rarefaction Curve")


legend(
  'right',
  legend=c('mean','Lower CI','Upper CI'),
  lty = c("dotted", "dashed", "solid"),col=c("blue", "forestgreen", "red"), inset=c(.1,.1))

