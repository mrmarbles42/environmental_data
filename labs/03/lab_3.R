require(here)
require(psych)
require(dplyr)

names(iris)

pairs.panels(iris[, c("Sepal.Length", "Sepal.Width", "Petal.Length")])

dat_bird <- read.csv(here("data", "bird.sta.csv"))
dat_habitat <- read.csv(here("data", "hab.sta.csv"))

dat_all <- merge(dat_bird, dat_habitat)

plot(ba.tot ~ elev, data = dat_all)

# act_site <- dat_all %>%
#   count(as.numeric(CEWA))

