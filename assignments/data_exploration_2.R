require(here)
require(psych)



hab_dat <- read.csv(here("data", "hab.sta.csv"))

bird_dat <- read.csv(here("data", "bird.sta.csv"))


hist(bird_dat$b.total) # most sites saw ~15-20 birds
hist(hab_dat$ba.ratio) # many conifer dominant areas

hist(bird_dat$AMRO,
     breaks = max(bird_dat$AMRO),
     xlab = "American Robin occurence distribution")
hist(bird_dat$WIWA, breaks = max(bird_dat$WIWA))

hab_dat$slope
pairs(hab_dat[, c("ba.tot", "ba.ratio", "elev", "slope")])

pairs(hab_dat[, c("")])

