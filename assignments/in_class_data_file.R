require(here)
library(dplyr)
library(ggplot2)

dat_catrate <- read.csv(
  here(
    "OneDrive - University of Massachusetts/environmental_data/data",
"catrate.csv"))
dat_delomys <- read.csv(
  here(
    "OneDrive - University of Massachusetts/environmental_data/data",
    "delomys.csv"))
dat_rope <- read.csv(
  here(
    "OneDrive - University of Massachusetts/environmental_data/data",
    "rope.csv"))

hist(dat_catrate$cat.rate)
hist(dat_delomys$body_mass,
main = "Delomys Body Mass (g)",
xlab = "Body Mass (g)")


hist(dat_rope$p.strength)

ggplot(dat_rope, aes(rope.type, p.strength, size = p.cut)) +
  geom_boxplot()
