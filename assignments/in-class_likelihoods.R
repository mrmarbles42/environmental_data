library(dplyr)

pengu <- palmerpenguins::penguins

dat_bird = read.csv(here::here("data", "bird.sta.csv"))
dat_habitat = read.csv(here::here("data", "hab.sta.csv"))
dat_all = merge(dat_bird, dat_habitat)

summary(dat_all$WIWA)
hist(dat_all$WIWA, breaks = 0:7)

wiwa_counts = c(2, 6)
sum(log(dpois(x = wiwa_counts, lambda = 4)))


summary(dat_all$WIWR)
hist(dat_all$WIWR, breaks = 0:6,
     main = "Winter Wren counts")

summary(dat_all$WIWR)
wiwr_count <-  c(1, 6)
sum(log(dbinom(wiwr_count,
              size = 1046, prob = )))

length(dat_all$WIWR)

(dbinom(c(1,2), size = 2, prob = .666))

nlm


set.seed(1)
vec_rnorm = (rnorm(n = 10, mean = 0, sd = 1))

(log((vec_rnorm), na.rm = TRUE))


sum(log(dpois(x = dat_all$WIWR, lambda = 1.45)))

summary(pengu$flipper_length_mm)
hist(pengu$flipper_length_mm, breaks = 30)

wiwr <- dat_all$WIWR
unique(wiwr)

